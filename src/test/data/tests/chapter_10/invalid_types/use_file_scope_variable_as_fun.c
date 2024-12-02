/* This declares a global variable */
int foo;

int main(void) {
    /* Treating a variable as a function is a type error. */
    return foo();
}
