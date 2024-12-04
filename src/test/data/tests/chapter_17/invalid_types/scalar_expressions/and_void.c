// void expressions are non-scalar, so they can't be used in logical expressions
void f(void){
    return;
}

int main(void) {
    return f() && 2;
}
