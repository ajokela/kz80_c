// String copy and length functions

int strlen(char *s) {
    int len = 0;
    while (s[len] != 0) {
        len = len + 1;
    }
    return len;
}

void strcpy(char *dst, char *src) {
    int i = 0;
    while (src[i] != 0) {
        dst[i] = src[i];
        i = i + 1;
    }
    dst[i] = 0;
}

int main() {
    char buf[20];
    
    strcpy(buf, "Hello, World!");
    puts(buf);
    putchar('\n');
    
    puts("Length: ");
    int len = strlen(buf);
    putchar('0' + len / 10);
    putchar('0' + len % 10);
    putchar('\n');
    
    return 0;
}
