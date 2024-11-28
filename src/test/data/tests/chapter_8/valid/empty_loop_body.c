int main(void) {
    int i = 21474836;
    do ; while ((i = i - 5) >= 256);

    return i;
}
