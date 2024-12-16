extern int putchar(int ch);
extern int read_u8(int addr);

const int MSG = 0x100;

#pragma data MSG "Hello, world!\n"

int main(void) {
    for (int i = MSG; read_u8(i) != 0; i++) {
        putchar(read_u8(i));
    }

    return 0;
}
