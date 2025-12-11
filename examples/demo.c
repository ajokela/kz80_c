// Demo of kz80_c compiler features

int counter = 0;

void print_digit(int d) { putchar('0' + d); }
void print_num(int n) {
    if (n >= 10) { print_num(n / 10); }
    print_digit(n % 10);
}
void print(int n) { print_num(n); putchar(' '); }

int strlen(char *s) {
    int len = 0;
    while (s[len] != 0) { len = len + 1; }
    return len;
}

void strcpy(char *dst, char *src) {
    int i = 0;
    while (src[i] != 0) { dst[i] = src[i]; i = i + 1; }
    dst[i] = 0;
}

int fibonacci(int n) {
    int a = 0;
    int b = 1;
    int i = 0;
    while (i < n) {
        int temp = a + b;
        a = b;
        b = temp;
        i = i + 1;
    }
    return a;
}

int main() {
    puts("=== kz80_c Demo ===\n\n");
    
    // Fibonacci
    puts("Fibonacci(0-9): ");
    int i = 0;
    while (i < 10) {
        print(fibonacci(i));
        i = i + 1;
    }
    putchar('\n');
    
    // String operations
    puts("\nString test:\n");
    char buf[32];
    strcpy(buf, "Hello, Z80!");
    puts("  String: ");
    puts(buf);
    puts("\n  Length: ");
    print_num(strlen(buf));
    putchar('\n');
    
    // Global counter
    puts("\nGlobal counter: ");
    while (counter < 5) {
        print(counter);
        counter = counter + 1;
    }
    puts("\n\n=== Done! ===\n");
    
    return 0;
}
