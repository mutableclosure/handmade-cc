extern int putchar(int ch);
extern int read_u8(int addr);

const int MSG = 0x100;

#pragma data MSG "\t\n\r\"\'\\\x20\x00"

int main(void) {
    for (int i = MSG; read_u8(i) != 0; i++) {
        putchar(read_u8(i));
    }

    return 0;
}
