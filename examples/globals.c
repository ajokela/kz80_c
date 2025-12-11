// Global variable test for RetroShield Z80

int counter = 0;
int max_count = 5;

void print_digit(int d) {
    putchar('0' + d);
}

void print_num(int n) {
    if (n >= 10) {
        print_num(n / 10);
    }
    print_digit(n % 10);
}

void increment() {
    counter = counter + 1;
}

int main() {
    puts("Counting: ");
    while (counter < max_count) {
        print_num(counter);
        putchar(' ');
        increment();
    }
    putchar('\n');
    puts("Done!\n");
    return 0;
}
