use crate::{
    ast::{
        BinaryOp, Block, BlockItem, Case, ConstQualifier, Datum, Expression, ExpressionKind,
        ForInit, FunctionBody, FunctionDeclaration, FunctionParameter, GlobalDeclaration, Lvalue,
        Program, Statement, Type, VariableDeclaration,
    },
    environment::{Environment, FunctionDeclarationType, GlobalDeclarationType, Symbol},
    evaluator::Evaluator,
    lexer::Lexer,
    token::{Keyword, Token, TokenKind},
    verifier::Verifier,
    Error, ErrorKind, Severity,
};
use alloc::{
    boxed::Box,
    collections::{btree_map::BTreeMap, btree_set::BTreeSet},
    rc::Rc,
    string::String,
    vec::Vec,
};

const PRAGMA_DIRECTIVE: &str = "pragma";
const DATA_DIRECTIVE: &str = "data";

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    environment: Environment,
    evaluator: Evaluator,
    line_number: usize,
    column: usize,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum Op {
    Binary(BinaryOp),
    Conditional,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum DeclarationScope {
    Extern,
    Static,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            environment: Environment::default(),
            evaluator: Evaluator,
            line_number: 1,
            column: 1,
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut globals = BTreeMap::new();
        let mut functions = Vec::new();
        let mut data = Vec::new();

        while let Some(token) = self.lexer.next()? {
            self.set_location(&token);
            match token.kind {
                TokenKind::Keyword(Keyword::Int) => {
                    let name = self.expect_identifier()?;
                    if self.peek_token(TokenKind::OpenParenthesis)? {
                        if let Some(f) = self.function(name, Type::Int, DeclarationScope::Static)? {
                            functions.push(f)
                        }
                    } else {
                        let global = self.global(name, Type::Int, ConstQualifier::NonConst)?;
                        if global.init.is_some() || !globals.contains_key(&global.name) {
                            globals.insert(global.name.clone(), global);
                        }
                    }
                }
                TokenKind::Keyword(Keyword::Void) => {
                    let name = self.expect_identifier()?;
                    if let Some(f) = self.function(name, Type::Void, DeclarationScope::Static)? {
                        functions.push(f)
                    }
                }
                TokenKind::Keyword(Keyword::Const) => {
                    self.expect_token(TokenKind::Keyword(Keyword::Int))?;
                    let name = self.expect_identifier()?;
                    let global = self.global(name, Type::Int, ConstQualifier::Const)?;
                    if global.init.is_some() || !globals.contains_key(&global.name) {
                        globals.insert(global.name.clone(), global);
                    }
                }
                TokenKind::Keyword(Keyword::Extern) => {
                    if let Some(f) = self.extern_declaration()? {
                        functions.push(f);
                    }
                }
                TokenKind::NumberSign => data.push(self.directive()?),
                _ => return Err(self.err(ErrorKind::UnknownType(token.kind))),
            }
        }

        Verifier::new(&self.environment).verify_functions(&functions)?;

        Ok(Program {
            globals: globals.into_values().collect(),
            functions,
            data,
        })
    }
}

impl Parser<'_> {
    fn global(
        &mut self,
        name: Rc<String>,
        r#type: Type,
        r#const: ConstQualifier,
    ) -> Result<GlobalDeclaration, Error> {
        let has_init = self.peek_token(TokenKind::EqualSign)?;
        let init = if has_init {
            self.lexer.next()?;
            Some(self.expect_constant()?)
        } else {
            None
        };
        let name = self
            .environment
            .declare_global(
                name,
                r#const,
                if has_init {
                    GlobalDeclarationType::NonTentative(init)
                } else {
                    GlobalDeclarationType::Tentative
                },
            )
            .map_err(|kind| self.err(kind))?;

        self.expect_token(TokenKind::Semicolon)?;

        Ok(GlobalDeclaration {
            name,
            r#type,
            r#const,
            init,
        })
    }

    fn extern_declaration(&mut self) -> Result<Option<FunctionDeclaration>, Error> {
        let token = self.lexer.next()?;
        self.maybe_set_location(token.as_ref());
        let token = token.map(|t| t.kind);

        match token {
            Some(TokenKind::Keyword(Keyword::Int)) => {
                let name = self.expect_identifier()?;
                self.function(name, Type::Int, DeclarationScope::Extern)
            }
            Some(TokenKind::Keyword(Keyword::Void)) => {
                let name = self.expect_identifier()?;
                self.function(name, Type::Void, DeclarationScope::Extern)
            }
            _ => Err(self.err(
                token
                    .map(ErrorKind::UnknownType)
                    .unwrap_or_else(|| ErrorKind::ExpectedType),
            )),
        }
    }

    fn function(
        &mut self,
        name: Rc<String>,
        r#type: Type,
        scope: DeclarationScope,
    ) -> Result<Option<FunctionDeclaration>, Error> {
        let parameters = self.function_parameters()?;
        let declaration_type = match scope {
            DeclarationScope::Extern => {
                self.expect_token(TokenKind::Semicolon)?;
                FunctionDeclarationType::ExternDeclaration
            }
            DeclarationScope::Static => {
                if self.peek_token(TokenKind::Semicolon)? {
                    self.lexer.next()?;
                    FunctionDeclarationType::ForwardDeclaration
                } else {
                    FunctionDeclarationType::Definition
                }
            }
        };

        self.environment
            .declare_function(name.clone(), parameters.clone(), r#type, declaration_type)
            .map_err(|kind| self.err(kind))?;

        match declaration_type {
            FunctionDeclarationType::ExternDeclaration => Ok(Some(FunctionDeclaration {
                name,
                parameters,
                return_type: r#type,
                body: FunctionBody::Extern,
            })),
            FunctionDeclarationType::ForwardDeclaration => Ok(None),
            FunctionDeclarationType::Definition => {
                self.environment.enter_function(name.clone());
                let body = FunctionBody::Block(self.block()?);
                self.environment.exit_function();

                Ok(Some(FunctionDeclaration {
                    name,
                    parameters,
                    return_type: r#type,
                    body,
                }))
            }
        }
    }

    fn function_parameters(&mut self) -> Result<Vec<FunctionParameter>, Error> {
        let mut parameters = Vec::new();
        self.expect_token(TokenKind::OpenParenthesis)?;

        if self.peek_token(TokenKind::Keyword(Keyword::Void))? {
            self.lexer.next()?;
        } else {
            loop {
                let r#const = if self.peek_token(TokenKind::Keyword(Keyword::Const))? {
                    self.lexer.next()?;
                    ConstQualifier::Const
                } else {
                    ConstQualifier::NonConst
                };
                self.expect_token(TokenKind::Keyword(Keyword::Int))?;
                let name = self.expect_identifier()?;
                let r#type = Type::Int;
                parameters.push(FunctionParameter {
                    name,
                    r#type,
                    r#const,
                });

                if !self.peek_token(TokenKind::Comma)? {
                    break;
                }

                self.lexer.next()?;
            }
        }

        self.expect_token(TokenKind::CloseParenthesis)?;

        Ok(parameters)
    }

    fn directive(&mut self) -> Result<Datum, Error> {
        self.expect_directive(PRAGMA_DIRECTIVE)?;
        self.expect_directive(DATA_DIRECTIVE)?;

        let address = self.expect_constant()?;
        let bytes = self.expect_string()?;

        Ok(Datum { address, bytes })
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        match self.lexer.peek()?.map(|t| &t.kind) {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                self.lexer.next()?;
                let function = self.environment.function().unwrap();
                let expression = match function.return_type {
                    Type::Int => {
                        let expression = self.expression(0)?;
                        self.expect_int(&expression)?;
                        Some(expression)
                    }
                    Type::Void => None,
                };
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Return(expression))
            }
            Some(TokenKind::Semicolon) => {
                self.lexer.next()?;
                Ok(Statement::Null)
            }
            Some(TokenKind::Keyword(Keyword::If)) => {
                self.lexer.next()?;
                let condition = self.condition()?;
                let then = self.statement()?.into();

                let r#else = if self.peek_token(TokenKind::Keyword(Keyword::Else))? {
                    self.lexer.next()?;
                    Some(self.statement()?)
                } else {
                    None
                }
                .map(|s| s.into());

                Ok(Statement::If(condition, then, r#else))
            }
            Some(TokenKind::Keyword(Keyword::Break)) => {
                let token = self.lexer.next()?;
                self.maybe_set_location(token.as_ref());
                let label = self
                    .environment
                    .loop_or_switch_label()
                    .ok_or_else(|| self.err(ErrorKind::BreakOutsideLoopOrSwitch))?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Break(label))
            }
            Some(TokenKind::Keyword(Keyword::Continue)) => {
                let token = self.lexer.next()?;
                self.maybe_set_location(token.as_ref());
                let label = self
                    .environment
                    .loop_label()
                    .ok_or_else(|| self.err(ErrorKind::ContinueOutsideLoop))?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Continue(label))
            }
            Some(TokenKind::Keyword(Keyword::While)) => {
                self.lexer.next()?;
                let condition = self.condition()?;
                let (label, body) = self.loop_body()?;
                Ok(Statement::While(label, condition, body))
            }
            Some(TokenKind::Keyword(Keyword::Do)) => {
                self.lexer.next()?;
                let (label, body) = self.loop_body()?;
                self.expect_token(TokenKind::Keyword(Keyword::While))?;
                let condition = self.condition()?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::DoWhile(label, body, condition))
            }
            Some(TokenKind::Keyword(Keyword::For)) => {
                self.lexer.next()?;
                self.environment.nest();
                self.expect_token(TokenKind::OpenParenthesis)?;
                let init = self.for_init()?;
                let condition = self.maybe_expression(TokenKind::Semicolon)?;
                if let Some(e) = condition.as_ref() {
                    self.expect_int(e)?;
                }
                let post = self.maybe_expression(TokenKind::CloseParenthesis)?;
                let (label, body) = self.loop_body()?;
                self.environment.unnest();
                Ok(Statement::For(label, init, condition, post, body))
            }
            Some(TokenKind::Keyword(Keyword::Switch)) => {
                self.lexer.next()?;

                let label = self.environment.enter_switch();
                let condition = self.condition()?;
                self.expect_token(TokenKind::OpenBrace)?;
                self.environment.nest();

                let mut cases = Vec::new();
                let mut case_values = BTreeSet::new();
                let mut default = None;
                let delimiters = BTreeSet::from_iter([
                    TokenKind::Keyword(Keyword::Case),
                    TokenKind::Keyword(Keyword::Default),
                    TokenKind::CloseBrace,
                ]);

                loop {
                    let label = self.environment.case();
                    let value = if self.peek_token(TokenKind::Keyword(Keyword::Case))? {
                        self.lexer.next()?;
                        let value = self.expect_constant()?;
                        if case_values.contains(&value) {
                            return Err(self.err(ErrorKind::DuplicateCase));
                        }
                        case_values.insert(value);
                        value
                    } else if self.peek_token(TokenKind::Keyword(Keyword::Default))? {
                        if default.is_some() {
                            return Err(self.err(ErrorKind::DuplicateCase));
                        }
                        self.lexer.next()?;
                        default = Some(cases.len() as i32);
                        0
                    } else {
                        break;
                    };

                    self.expect_token(TokenKind::Colon)?;

                    let mut statements = Vec::new();

                    while !self.peek_token_in(&delimiters)? {
                        statements.push(self.statement()?);
                    }

                    cases.push(Case {
                        label,
                        value,
                        statements,
                    })
                }

                self.expect_token(TokenKind::CloseBrace)?;
                self.environment.unnest();
                self.environment.exit_switch();

                Ok(Statement::Switch(label, condition, cases, default))
            }
            Some(TokenKind::OpenBrace) => Ok(Statement::Compound(self.block()?)),
            _ => {
                let expression = self.expression(0)?;
                self.expect_token(TokenKind::Semicolon)?;
                Ok(Statement::Expression(expression))
            }
        }
    }

    fn condition(&mut self) -> Result<Expression, Error> {
        self.expect_token(TokenKind::OpenParenthesis)?;
        let condition = self.expression(0)?;
        self.expect_int(&condition)?;
        self.expect_token(TokenKind::CloseParenthesis)?;
        Ok(condition)
    }

    fn for_init(&mut self) -> Result<Option<ForInit>, Error> {
        if self.peek_token(TokenKind::Keyword(Keyword::Int))? {
            self.lexer.next()?;
            Ok(Some(ForInit::Declaration(
                self.variable(ConstQualifier::NonConst)?,
            )))
        } else if self.peek_token(TokenKind::Keyword(Keyword::Const))? {
            self.lexer.next()?;
            self.expect_token(TokenKind::Keyword(Keyword::Int))?;
            Ok(Some(ForInit::Declaration(
                self.variable(ConstQualifier::Const)?,
            )))
        } else if self.peek_token(TokenKind::Semicolon)? {
            self.lexer.next()?;
            Ok(None)
        } else {
            let expression = self.expression(0)?;
            self.expect_token(TokenKind::Semicolon)?;
            Ok(Some(ForInit::Expression(expression)))
        }
    }

    fn maybe_expression(&mut self, terminator: TokenKind) -> Result<Option<Expression>, Error> {
        Ok(if self.peek_token(terminator.clone())? {
            self.lexer.next()?;
            None
        } else {
            let expression = self.expression(0)?;
            self.expect_token(terminator)?;
            Some(expression)
        })
    }

    fn loop_body(&mut self) -> Result<(Rc<String>, Box<Statement>), Error> {
        let label = self.environment.enter_loop();
        let body = self.statement()?.into();
        self.environment.exit_loop();
        Ok((label, body))
    }

    fn block(&mut self) -> Result<Block, Error> {
        self.expect_token(TokenKind::OpenBrace)?;
        self.environment.nest();

        let mut items = Vec::new();

        while !self.peek_token(TokenKind::CloseBrace)? {
            items.push(self.block_item()?);
        }

        self.expect_token(TokenKind::CloseBrace)?;
        self.environment.unnest();

        Ok(Block { items })
    }

    fn block_item(&mut self) -> Result<BlockItem, Error> {
        if self.peek_token(TokenKind::Keyword(Keyword::Int))? {
            self.lexer.next()?;
            Ok(BlockItem::VariableDeclaration(
                self.variable(ConstQualifier::NonConst)?,
            ))
        } else if self.peek_token(TokenKind::Keyword(Keyword::Const))? {
            self.lexer.next()?;
            self.expect_token(TokenKind::Keyword(Keyword::Int))?;
            Ok(BlockItem::VariableDeclaration(
                self.variable(ConstQualifier::Const)?,
            ))
        } else {
            Ok(BlockItem::Statement(self.statement()?))
        }
    }

    fn variable(&mut self, r#const: ConstQualifier) -> Result<VariableDeclaration, Error> {
        let r#type = Type::Int;
        let identifier = self.expect_identifier()?;
        let name = self
            .environment
            .declare_variable(identifier, r#const)
            .map_err(|kind| self.err(kind))?;

        let init = if self.peek_token(TokenKind::EqualSign)? {
            self.lexer.next()?;
            let expression = self.expression(0)?;
            self.expect_int(&expression)?;
            Some(expression)
        } else {
            None
        };

        self.expect_token(TokenKind::Semicolon)?;

        Ok(VariableDeclaration {
            name,
            r#type,
            r#const,
            init,
        })
    }

    fn expression(&mut self, min_precedence: u32) -> Result<Expression, Error> {
        let mut left = self.factor()?;
        let mut token = self.lexer.peek()?;

        if let Some(token) = token {
            self.line_number = token.line_number;
            self.column = token.column;
        }

        while let Some(op) = token.map(|t| &t.kind).and_then(op) {
            let precedence = precedence(op);

            if precedence < min_precedence {
                break;
            }

            self.lexer.next()?;

            match op {
                Op::Binary(op) => {
                    let precedence = if is_assignment(op) {
                        self.expect_lvalue(&left)?;
                        precedence
                    } else {
                        self.expect_int(&left)?;
                        precedence.saturating_add(1)
                    };

                    let right = self.expression(precedence)?;
                    self.expect_int(&right)?;
                    left = Expression::int(ExpressionKind::BinaryOp(op, left.into(), right.into()));
                }
                Op::Conditional => {
                    self.expect_int(&left)?;
                    let middle = self.expression(0)?;
                    self.expect_token(TokenKind::Colon)?;
                    let right = self.expression(precedence)?;
                    let r#type = match (middle.r#type, right.r#type) {
                        (Type::Int, Type::Int) => Type::Int,
                        (Type::Void, Type::Void)
                        | (Type::Int, Type::Void)
                        | (Type::Void, Type::Int) => Type::Void,
                    };
                    left = Expression {
                        kind: ExpressionKind::Conditional(left.into(), middle.into(), right.into()),
                        r#type,
                    };
                }
            }

            token = self.lexer.peek()?;

            if let Some(token) = token {
                self.line_number = token.line_number;
                self.column = token.column;
            }
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expression, Error> {
        let token = self.lexer.next()?;
        self.maybe_set_location(token.as_ref());
        match token.map(|t| t.kind) {
            Some(TokenKind::Constant(value)) => {
                Ok(Expression::int(ExpressionKind::Constant(value)))
            }
            Some(TokenKind::Identifier(identifier))
                if self.peek_token(TokenKind::TwoPlusSigns)? =>
            {
                self.lexer.next()?;
                self.environment
                    .resolve_lvalue(identifier)
                    .map(|lvalue| lvalue.into())
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(ExpressionKind::PostfixIncrement)
                    .map(Expression::int)
            }
            Some(TokenKind::Identifier(identifier))
                if self.peek_token(TokenKind::TwoHyphens)? =>
            {
                self.lexer.next()?;
                self.environment
                    .resolve_lvalue(identifier)
                    .map(|lvalue| lvalue.into())
                    .map_err(|kind| self.err(kind))
                    .map(Box::new)
                    .map(ExpressionKind::PostfixDecrement)
                    .map(Expression::int)
            }
            Some(TokenKind::Identifier(identifier)) => self.symbol(identifier),
            Some(TokenKind::TwoPlusSigns) => {
                let variable = self.factor()?;
                self.expect_lvalue(&variable)?;
                let kind = ExpressionKind::PrefixIncrement(variable.into());
                Ok(Expression::int(kind))
            }
            Some(TokenKind::TwoHyphens) => {
                let variable = self.factor()?;
                self.expect_lvalue(&variable)?;
                let kind = ExpressionKind::PrefixDecrement(variable.into());
                Ok(Expression::int(kind))
            }
            Some(TokenKind::Tilde) => self
                .factor()
                .map(Box::new)
                .map(ExpressionKind::BitwiseComplement)
                .map(Expression::int),
            Some(TokenKind::Hyphen) => self
                .factor()
                .map(Box::new)
                .map(ExpressionKind::Negation)
                .map(Expression::int),
            Some(TokenKind::ExclamationPoint) => self
                .factor()
                .map(Box::new)
                .map(ExpressionKind::Not)
                .map(Expression::int),
            Some(TokenKind::OpenParenthesis) => {
                let expression = self.expression(0)?;
                let expression = self
                    .expect_token(TokenKind::CloseParenthesis)
                    .map(|()| expression)?;
                if self.expect_lvalue(&expression).is_ok() {
                    if self.peek_token(TokenKind::TwoPlusSigns)? {
                        self.lexer.next()?;
                        let kind = ExpressionKind::PostfixIncrement(expression.into());
                        return Ok(Expression::int(kind));
                    } else if self.peek_token(TokenKind::TwoHyphens)? {
                        self.lexer.next()?;
                        let kind = ExpressionKind::PostfixDecrement(expression.into());
                        return Ok(Expression::int(kind));
                    }
                }
                Ok(expression)
            }
            token => Err(self.err(ErrorKind::ExpectedExpression(token))),
        }
    }

    fn symbol(&mut self, identifier: Rc<String>) -> Result<Expression, Error> {
        let (name, symbol) = self
            .environment
            .resolve_symbol(identifier)
            .map_err(|kind| self.err(kind))?;

        match symbol {
            Symbol::Global(r#const, _) => {
                Ok(Expression::int(ExpressionKind::Global(name, *r#const)))
            }
            Symbol::Variable(r#const) => {
                Ok(Expression::int(ExpressionKind::Variable(name, *r#const)))
            }
            Symbol::Function(function) => {
                let parameters = function.parameters.values().copied().collect::<Vec<_>>();
                let r#type = function.return_type;
                self.expect_token(TokenKind::OpenParenthesis)?;

                let mut arguments = Vec::new();

                for (index, (param_type, _)) in parameters.iter().enumerate() {
                    let argument = self.expression(0)?;
                    assert!(matches!(param_type, Type::Int));
                    self.expect_int(&argument)?;
                    arguments.push(argument);

                    if index < parameters.len() - 1 {
                        self.expect_token(TokenKind::Comma)?;
                    }
                }

                self.expect_token(TokenKind::CloseParenthesis)?;

                Ok(Expression {
                    kind: ExpressionKind::FunctionCall(name, arguments),
                    r#type,
                })
            }
        }
    }

    fn expect_directive(&mut self, directive: &str) -> Result<(), Error> {
        let identifier = self.expect_identifier()?;

        if identifier.as_str() != directive {
            Err(self.err(ErrorKind::InvalidDirective(identifier)))
        } else {
            Ok(())
        }
    }

    fn expect_identifier(&mut self) -> Result<Rc<String>, Error> {
        let token = self.lexer.next();
        self.maybe_set_location_from_result(token.as_ref());
        token?.map_or_else(
            || Err(self.err(ErrorKind::ExpectedIdentifier(None))),
            |token| match token.kind {
                TokenKind::Identifier(identifier) => Ok(identifier),
                _ => Err(self.err(ErrorKind::ExpectedIdentifier(Some(token.kind)))),
            },
        )
    }

    fn expect_string(&mut self) -> Result<Vec<u8>, Error> {
        let token = self.lexer.next();
        self.maybe_set_location_from_result(token.as_ref());
        token?.map_or_else(
            || Err(self.err(ErrorKind::ExpectedString(None))),
            |token| match token.kind {
                TokenKind::String(identifier) => Ok(identifier),
                _ => Err(self.err(ErrorKind::ExpectedString(Some(token.kind)))),
            },
        )
    }

    fn expect_constant(&mut self) -> Result<i32, Error> {
        let value = self.expression(0)?;
        self.evaluator
            .evaluate_i32(&value, &self.environment)
            .map_err(|e| self.err(e))
    }

    fn expect_token(&mut self, expected_token: TokenKind) -> Result<(), Error> {
        let token = self.lexer.next();
        self.maybe_set_location_from_result(token.as_ref());
        token?.map_or_else(
            || Err(self.err(ErrorKind::ExpectedToken(expected_token.clone(), None))),
            |token| {
                if token.kind == expected_token {
                    Ok(())
                } else {
                    Err(self.err(ErrorKind::ExpectedToken(
                        expected_token.clone(),
                        Some(token.kind),
                    )))
                }
            },
        )
    }

    fn peek_token(&mut self, expected_token: TokenKind) -> Result<bool, Error> {
        Ok(self.lexer.peek()?.is_some_and(|t| t.kind == expected_token))
    }

    fn peek_token_in(&mut self, expected_tokens: &BTreeSet<TokenKind>) -> Result<bool, Error> {
        Ok(self
            .lexer
            .peek()?
            .is_some_and(|t| expected_tokens.contains(&t.kind)))
    }

    fn expect_lvalue(&self, expression: &Expression) -> Result<(), Error> {
        match expression.kind {
            ExpressionKind::Variable(_, ConstQualifier::NonConst)
            | ExpressionKind::Global(_, ConstQualifier::NonConst) => Ok(()),
            _ => Err(self.err(ErrorKind::InvalidLvalue)),
        }
    }

    fn expect_int(&self, expression: &Expression) -> Result<(), Error> {
        match expression.r#type {
            Type::Int => Ok(()),
            Type::Void => Err(self.err(ErrorKind::NonIntegerExpression)),
        }
    }

    fn set_location(&mut self, token: &Token) {
        self.line_number = token.line_number;
        self.column = token.column;
    }

    fn maybe_set_location(&mut self, token: Option<&Token>) {
        if let Some(token) = token {
            self.line_number = token.line_number;
            self.column = token.column;
        }
    }

    fn maybe_set_location_from_result<E>(&mut self, token: Result<&Option<Token>, E>) {
        if let Ok(Some(token)) = &token {
            self.line_number = token.line_number;
            self.column = token.column;
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        Error {
            line_number: self.line_number,
            column: self.column,
            kind,
            severity: Severity::Error,
        }
    }
}

impl From<Lvalue> for Expression {
    fn from(value: Lvalue) -> Self {
        match value {
            Lvalue::Global(name) => {
                Expression::int(ExpressionKind::Global(name, ConstQualifier::NonConst))
            }
            Lvalue::Variable(name) => {
                Expression::int(ExpressionKind::Variable(name, ConstQualifier::NonConst))
            }
        }
    }
}

impl Expression {
    fn int(kind: ExpressionKind) -> Self {
        Self {
            kind,
            r#type: Type::Int,
        }
    }
}

fn op(token: &TokenKind) -> Option<Op> {
    binary_op(token)
        .map(Op::Binary)
        .or_else(|| matches!(token, TokenKind::QuestionMark).then(|| Op::Conditional))
}

fn binary_op(token: &TokenKind) -> Option<BinaryOp> {
    match token {
        TokenKind::PlusSign => Some(BinaryOp::Add),
        TokenKind::Hyphen => Some(BinaryOp::Subtract),
        TokenKind::Asterisk => Some(BinaryOp::Multiply),
        TokenKind::Slash => Some(BinaryOp::Divide),
        TokenKind::Percent => Some(BinaryOp::Remainder),
        TokenKind::Ampersand => Some(BinaryOp::BitwiseAnd),
        TokenKind::Bar => Some(BinaryOp::BitwiseOr),
        TokenKind::Circumflex => Some(BinaryOp::Xor),
        TokenKind::TwoLessThanOps => Some(BinaryOp::LeftShift),
        TokenKind::TwoGreaterThanOps => Some(BinaryOp::RightShift),
        TokenKind::TwoAmpersands => Some(BinaryOp::And),
        TokenKind::TwoBars => Some(BinaryOp::Or),
        TokenKind::TwoEqualSigns => Some(BinaryOp::EqualTo),
        TokenKind::NotEqualSign => Some(BinaryOp::NotEqualTo),
        TokenKind::LessThanOp => Some(BinaryOp::LessThan),
        TokenKind::LessThanOrEqualToOp => Some(BinaryOp::LessThanOrEqualTo),
        TokenKind::GreaterThanOp => Some(BinaryOp::GreaterThan),
        TokenKind::GreaterThanOrEqualToOp => Some(BinaryOp::GreaterThanOrEqualTo),
        TokenKind::EqualSign => Some(BinaryOp::Assignment),
        TokenKind::PlusEqualSign => Some(BinaryOp::AddAssignment),
        TokenKind::HyphenEqualSign => Some(BinaryOp::SubtractAssignment),
        TokenKind::AsteriskEqualSign => Some(BinaryOp::MultiplyAssignment),
        TokenKind::SlashEqualSign => Some(BinaryOp::DivideAssignment),
        TokenKind::PercentEqualSign => Some(BinaryOp::RemainderAssignment),
        TokenKind::AmpersandEqualSign => Some(BinaryOp::BitwiseAndAssignment),
        TokenKind::BarEqualSign => Some(BinaryOp::BitwiseOrAssignment),
        TokenKind::CircumflexEqualSign => Some(BinaryOp::XorAssignment),
        TokenKind::TwoLessThanOpsEqualSign => Some(BinaryOp::LeftShiftAssignment),
        TokenKind::TwoGreaterThanOpsEqualSign => Some(BinaryOp::RightShiftAssignment),
        _ => None,
    }
}

fn precedence(op: Op) -> u32 {
    match op {
        Op::Binary(op) => match op {
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Remainder => 50,
            BinaryOp::Add | BinaryOp::Subtract => 45,
            BinaryOp::LeftShift | BinaryOp::RightShift => 40,
            BinaryOp::LessThan
            | BinaryOp::LessThanOrEqualTo
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqualTo => 35,
            BinaryOp::EqualTo | BinaryOp::NotEqualTo => 30,
            BinaryOp::BitwiseAnd => 25,
            BinaryOp::Xor => 20,
            BinaryOp::BitwiseOr => 15,
            BinaryOp::And => 10,
            BinaryOp::Or => 5,
            BinaryOp::Assignment
            | BinaryOp::AddAssignment
            | BinaryOp::SubtractAssignment
            | BinaryOp::MultiplyAssignment
            | BinaryOp::DivideAssignment
            | BinaryOp::RemainderAssignment
            | BinaryOp::LeftShiftAssignment
            | BinaryOp::RightShiftAssignment
            | BinaryOp::BitwiseAndAssignment
            | BinaryOp::BitwiseOrAssignment
            | BinaryOp::XorAssignment => 1,
        },
        Op::Conditional => 3,
    }
}

fn is_assignment(op: BinaryOp) -> bool {
    match op {
        BinaryOp::Assignment
        | BinaryOp::AddAssignment
        | BinaryOp::SubtractAssignment
        | BinaryOp::MultiplyAssignment
        | BinaryOp::DivideAssignment
        | BinaryOp::RemainderAssignment
        | BinaryOp::BitwiseAndAssignment
        | BinaryOp::BitwiseOrAssignment
        | BinaryOp::XorAssignment
        | BinaryOp::LeftShiftAssignment
        | BinaryOp::RightShiftAssignment => true,
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Remainder
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::Xor
        | BinaryOp::LeftShift
        | BinaryOp::RightShift
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::EqualTo
        | BinaryOp::NotEqualTo
        | BinaryOp::LessThan
        | BinaryOp::LessThanOrEqualTo
        | BinaryOp::GreaterThan
        | BinaryOp::GreaterThanOrEqualTo => false,
    }
}
