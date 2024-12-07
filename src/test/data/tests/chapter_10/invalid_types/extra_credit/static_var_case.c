// Can't use a static variable as a case in a switch statement

int i = 0;

int main(void) {
    switch(0) {
        case i: return 0;
    }
    return 0;
}
