int foo = 10;

/* Since this declaration has external linkage,
 * it refers to the same entity as the declaration
 * of foo above. But the earlier declaration declares
 * a variable and this one declares a function,
 * so they conflict.
 */
int foo(void) {
    return 0;
}

int main(void) {
    return 0;
}
