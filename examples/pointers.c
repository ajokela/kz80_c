// Pointer test for RetroShield Z80

void print_digit(int d) {
    putchar('0' + d);
}

void print_num(int n) {
    if (n >= 10) {
        print_num(n / 10);
    }
    print_digit(n % 10);
}

void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int main() {
    int x = 5;
    int y = 10;
    
    puts("Before: x=");
    print_num(x);
    puts(" y=");
    print_num(y);
    putchar('\n');
    
    swap(&x, &y);
    
    puts("After:  x=");
    print_num(x);
    puts(" y=");
    print_num(y);
    putchar('\n');
    
    return 0;
}
