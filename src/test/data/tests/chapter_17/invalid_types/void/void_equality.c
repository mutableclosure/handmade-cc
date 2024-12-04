void f(void){
    return;
}

int main(void) {
    // you can't compare void expressions
    return f() == f();
}
