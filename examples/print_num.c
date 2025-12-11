// Print numbers for RetroShield Z80

void print_digit(int d) {
    putchar('0' + d);
}

void print_num(int n) {
    if (n < 0) {
        putchar('-');
        n = -n;
    }
    if (n >= 10) {
        print_num(n / 10);
    }
    print_digit(n % 10);
}

void println(int n) {
    print_num(n);
    putchar('\n');
}

int main() {
    println(0);
    println(42);
    println(123);
    println(1000);
    return 0;
}
