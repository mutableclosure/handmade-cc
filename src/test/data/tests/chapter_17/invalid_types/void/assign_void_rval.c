void f(void){
    return;
}

/* can't convert void to another type by assignment */
int main(void) {
  int a = 10;
  a = f();
  return 0;
}
