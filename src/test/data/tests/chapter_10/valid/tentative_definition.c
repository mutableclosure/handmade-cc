/* Verify that if variable is tentatively defined one or more times,
 * but not explicitly initialized, we'll initialize it to 0.
 */

/* A tentative definition of foo */
int foo;

/* Another tentative definition of foo */
int foo;

int main(void) {
    for (int i = 0; i < 5; i = i + 1)
        foo = foo + 1;
    return foo;
}

/* Yet another tentative definition of foo */
int foo;
