// Can't perform +=/-= with void lvalue
void f(void){
    return;
}

int main(void) {
    f() -= 0;
    return 0;
}
