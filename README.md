# Handmade C Compiler

A C-to-WebAssembly compiler written in `no_std` Rust and targeting `wasm32-unknown-unknown`. The compiler emits [`WebAssembly Text (WAT)`](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format), which is then compiled to bytecode using the [`wat`](https://crates.io/crates/wat) crate.

Handmade C is intended for use in restricted environments where embedding Clang is not practical or feasible, such as on the web, with standalone Wasm runtimes, or on embedded devices.

The project powers the compilation stage in [_Handmade Studio_](https://mutableclosure.itch.io/handmade-machine) and is based on the wonderful [_Writing a C Compiler_](https://norasandler.com/book/) book by Nora Sandler.

## Status and Goal

The compiler understands a minimal subset of C, aiming to strike a balance between expressiveness and simplicity in its codebase.

While more features may be added in the future, complete compliance with the C standard is not currently a goal. The focus is on having fun writing new C code by hand rather than porting existing libraries.

## Features

The language includes all C statements but supports only the `int` type. This feature set roughly matches _Part I_ of Nora's book (up to chapter 10), including all the _Extra Credit_ features and a few extras.

Notable omissions include strings, pointers, arrays, structs, enums, `typedef`, and `goto`. The compiler processes a single C file with (almost) no preprocessor or standard library. There is also no attempt to optimize the emitted code (although you can run [binaryen](https://github.com/WebAssembly/binaryen) on the final bytecode).

What the language **does** have is:

- The **`int` type**, which is 4 bytes wide (since we are targeting `wasm32`).
- All **C operators** for the `int` type (arithmetic, relational, logical, bitwise, and assignment operators, including increment and decrement).
- **Local and global variables**, with an optional **`const`** qualifier.
- All **C control-flow statements** (`if-else`, `else-if`, `switch`, `while`, `for`, `do-while`).
- **Functions** with `int` or `void` return types and either `void` or one or more `int` parameters.
- A custom `#pragma data` **directive for embedding data** and rudimentary string handling (see below).

Additional features beyond _Part I_ of Nora's book include:

- Constant expressions to initialize global variables, evaluated at compile time.
- Hexadecimal and binary notations for integer literals (`0x` and `0b`).
- The `const` qualifier.
- The `void` type for function return types or parameter lists.
- The `extern` qualifier to import functions from another WebAssembly module (see below).
- The custom `#pragma data` directive (the only form of preprocessing).

## External Functions

Since the compiler targets "pure" WebAssembly, there is no standard library or underlying operating system.

To do anything useful, you typically need to export your own functions from the host environment, such as JavaScript (in a browser) or Rust (or another language) when embedding a WebAssembly runtime in a desktop application.

You can use the `extern` keyword to declare these functions in your C code, for example:

```c
extern int putchar(int ch);
```

This declaration informs the compiler that a function named `putchar`, accepting and returning an `i32` value, exists in the default `env` WebAssembly module. Currently, specifying modules other than `env` is not supported.

You will also need to define `putchar` in `env` when setting up the WebAssembly runtime before executing the program.

For a complete example, refer to the `src/test` folder.

## `main` Function

The compiler expects any program to define a function called `main` and implicitly treats it as if the `extern` qualifier were attached.

Note that this is a convention of Handmade C, not of WebAssembly. You will need to manually call the `main` function from the WebAssembly runtime to start the program.

## Dealing with Strings

Since there is no native support for strings, data embedding relies on the `#pragma data` directive, which specifies an absolute memory address and a string literal. For example:

```c
#pragma data 0x100 "Hello, world!\n"
```

The directive emits WebAssembly `data` sections. When the WebAssembly program is instantiated, the contents of the `data` section will be placed at the specified memory address.

The string is encoded in ASCII, one byte per character. Supported escape sequences are `\t`, `\n`, `\r`, `\"`, `\'`, `\\`, and `\x00` (where `00` is the hexadecimal byte value). The last escape sequence allows encoding of arbitrary data.

To reference the embedded data later in your code, store its address in a global variable, like so:

```c
const int MSG = 0x100;
#pragma data MSG "Hello, world!\n"
```

## Tests

The project includes all relevant tests from the [test suite](https://github.com/nlsandler/writing-a-c-compiler-tests/) accompanying Nora Sandler's book, along with additional tests for new features.

The original test suite uses a Python script to compare output from a reference C compiler on the host machine with the compiler under test. To simplify setup, this project uses a JSON file containing the host compiler's output from the original repository and rewrites the comparison logic in Rust, allowing a simple `cargo test` to run without Python installed.

## Alternatives

For a more feature-rich freestanding C-to-WebAssembly compiler, consider [`xcc`](https://github.com/tyfkda/xcc), written in C and targeting [WASI](https://wasi.dev). It supports the standard library and looks promising, though I haven't tested it.

I started this project because I wanted something really minimal, with clear boundaries on what features are supported defined by the test suite. And, of course, it's fun to build :)

## Using the Compiler

Invoke the compiler as a binary program:

```sh
cargo run <source_file.c> <output.wasm>
```

Add the optional `--wat-only` argument to generate a WAT file (WebAssembly Text) instead of bytecode.

## Embedding in Other Projects

Being originally designed for the Handmade Studio codebase, the compiler can also be used as a library in desktop Rust applications.

To do so, call methods on the [`Compiler`] type. Refer to the test in `src/test` for an example.

## License

The project's source code is licensed under the [Mozilla Public License (MPL) version 2.0](https://www.mozilla.org/en-US/MPL/2.0/).

Tests in the `src/tests/data/tests/chapter_*` folders and the corresponding data in `src/tests/data/expected-results.json` accompany Nora Sandler's book _Writing a C Compiler_ and are provided under the original (MIT) license.
