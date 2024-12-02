// This test case verifies that we correctly rewrite expressions
// that use static variables; i.e. we recognize that they are memory operands

int i = 2;
int j = 3;

int main(void) {
    int cmp = i < j;

    if (!cmp)
        return 1;
    return 0;
}
