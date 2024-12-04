/* This is similar to chapter_9/valid/stack_arguments/test_for_memory_leaks.c
 * except that it calls a void function; make sure we restore stack frame correctly
 * after a void function call
 * */

int sum = 0;

void lots_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o) {
    sum = l + o;
}

int main(void) {
    for (int i = 0; i < 100000; i = i + 1) {
        lots_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    }
    return sum == 1500000;
}
