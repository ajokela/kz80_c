// Fibonacci sequence for RetroShield Z80

void print_digit(int d) {
    putchar('0' + d);
}

void print_num(int n) {
    if (n >= 10) {
        print_num(n / 10);
    }
    print_digit(n % 10);
}

void print(int n) {
    print_num(n);
    putchar(' ');
}

int main() {
    int a = 0;
    int b = 1;
    int i = 0;
    
    while (i < 15) {
        print(a);
        int temp = a + b;
        a = b;
        b = temp;
        i = i + 1;
    }
    
    putchar('\n');
    return 0;
}
