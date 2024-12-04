// Can't apply postfix ++/-- to void lvalue
void f(void){
    return;
}

int main(void) {
    ++(f())--;
    return 0;
}
