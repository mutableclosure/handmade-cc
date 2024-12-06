// This is valid C, but we don't support it.
const int a() {
    return 0;
}

int main(void) {
    int b = a();
    return 0;
}
