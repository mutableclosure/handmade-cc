// Can't apply prefix or postfix ++/-- to void
void f(void){
    return;
}

int main(void) {
    f()++;
    return 0;
}
