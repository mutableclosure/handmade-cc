void f(void){
    return;
}

// Can't perform bitwise operations with void operands
int main(void) {
    int x = 10;
    x & f();
    return 0;
}
