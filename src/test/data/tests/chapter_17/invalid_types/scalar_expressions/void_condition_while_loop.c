void f(void) { return; }
int main(void) {
  int i = 0;
  // void expressions are non-scalar, so they can't be used as controlling conditions
  while (f()) {
    i = i + 1;
  }
  return 0;
}
