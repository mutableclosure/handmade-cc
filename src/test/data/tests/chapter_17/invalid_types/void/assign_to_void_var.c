int main(void) {
  void v1;
  // in our implementation, you can't declare void variables
  // the standard is ambiguous about whether this is legal,
  // but you definitely can't declare a void variable and assign to it
  void v1 = (void)0;
  return 0;
}
