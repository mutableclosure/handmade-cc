// Can't perform +=/-= with void lvalue
void f(void){
    return;
}

int main(void) {
    f() += 3;
    return 0;
}
