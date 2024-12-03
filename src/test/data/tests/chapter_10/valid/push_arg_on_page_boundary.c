// use an int that's within 8 bytes of a page boundary as a stack argument
// this makes sure we don't use 8-byte push to push 4-byte values in memory

int zed = 0;
int foo(int a, int b, int c, int d, int e, int f, int g) {
    return g + 1;
}

int main(void) {
    return foo(0, 0, 0, 0, 0, 0, zed);
}
