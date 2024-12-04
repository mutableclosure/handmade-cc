// in our implementation, you can't declare void variables
// the standard may let you _declare_ void variables
// but you can't define initialize them (or define any incomplete object)

int main(void) {
     void v = 0;
     return 0;
}
