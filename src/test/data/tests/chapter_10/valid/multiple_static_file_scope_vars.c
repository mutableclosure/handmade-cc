/* A variable with internal linkage may be tentatively defined
 * and declared multiple times, but defined only once
 */

/* A tentative definition */
int foo;

int main(void) {
    return foo;
}

/* A declaration */
int foo;

/* A non-tentative definition */
int foo = 4;
