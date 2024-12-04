void f(void){
    return;
}

int main(void) {
  // you can't compare void expressions
  if (f() < f())
    return 1;
  return 0;
}
