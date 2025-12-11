// Array test for RetroShield Z80

int main() {
    char buf[10];
    int i;
    
    // Fill array with 'A' to 'J'
    i = 0;
    while (i < 10) {
        buf[i] = 'A' + i;
        i = i + 1;
    }
    
    // Print array
    i = 0;
    while (i < 10) {
        putchar(buf[i]);
        i = i + 1;
    }
    putchar('\n');
    
    return 0;
}
