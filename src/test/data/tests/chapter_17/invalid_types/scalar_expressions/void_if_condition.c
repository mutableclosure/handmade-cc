void f(void){
    return;
}

int main(void) {
  int x = 10;

  // void expressions are non-scalar, so they can't be used as controlling conditions
  if (f())
    return 0;
  return 1;
}
