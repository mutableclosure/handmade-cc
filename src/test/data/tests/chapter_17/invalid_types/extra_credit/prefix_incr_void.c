// Can't apply prefix ++/-- to void lvalue
void f(void){
    return;
}

int main(void) {
    ++(f());
    return 0;
}
