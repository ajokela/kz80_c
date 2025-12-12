// BCD Float test for RetroShield Z80

void print_digit(int d) {
    putchar('0' + d);
}

void print_num(int n) {
    if (n >= 10) {
        print_num(n / 10);
    }
    print_digit(n % 10);
}

int main() {
    // BCD float is 6 bytes: [sign][exp][mant0][mant1][mant2][mant3]
    char f1[6];
    char f2[6];
    char result[6];

    puts("BCD Float Test\n");

    // Convert 42 to BCD float
    bcdf_from_int(f1, 42);
    puts("42 as bcdf: ");
    bcdf_print(f1);
    putchar('\n');

    // Convert 100 to BCD float
    bcdf_from_int(f2, 100);
    puts("100 as bcdf: ");
    bcdf_print(f2);
    putchar('\n');

    // Add them
    bcdf_add(result, f1, f2);
    puts("42 + 100 = ");
    bcdf_print(result);
    putchar('\n');

    // Subtract: 100 - 42
    bcdf_sub(result, f2, f1);
    puts("100 - 42 = ");
    bcdf_print(result);
    putchar('\n');

    // Multiply: 6 * 7
    bcdf_from_int(f1, 6);
    bcdf_from_int(f2, 7);
    bcdf_mul(result, f1, f2);
    puts("6 * 7 = ");
    bcdf_print(result);
    putchar('\n');

    // Multiply: 12 * 11
    bcdf_from_int(f1, 12);
    bcdf_from_int(f2, 11);
    bcdf_mul(result, f1, f2);
    puts("12 * 11 = ");
    bcdf_print(result);
    putchar('\n');

    // Divide: 42 / 6
    bcdf_from_int(f1, 42);
    bcdf_from_int(f2, 6);
    bcdf_div(result, f1, f2);
    puts("42 / 6 = ");
    bcdf_print(result);
    putchar('\n');

    // Divide: 100 / 4
    bcdf_from_int(f1, 100);
    bcdf_from_int(f2, 4);
    bcdf_div(result, f1, f2);
    puts("100 / 4 = ");
    bcdf_print(result);
    putchar('\n');

    // Convert larger number
    bcdf_from_int(f1, 12345);
    puts("12345 as bcdf: ");
    bcdf_print(f1);
    putchar('\n');

    // Convert max 16-bit
    bcdf_from_int(f1, 65535);
    puts("65535 as bcdf: ");
    bcdf_print(f1);
    putchar('\n');

    // Zero
    bcdf_from_int(f1, 0);
    puts("0 as bcdf: ");
    bcdf_print(f1);
    putchar('\n');

    // Comparison tests
    puts("\nComparisons:\n");

    bcdf_from_int(f1, 100);
    bcdf_from_int(f2, 50);
    puts("cmp(100, 50) = ");
    print_num(bcdf_cmp(f1, f2));
    putchar('\n');

    puts("cmp(50, 100) = ");
    print_num(bcdf_cmp(f2, f1));
    putchar('\n');

    bcdf_from_int(f2, 100);
    puts("cmp(100, 100) = ");
    print_num(bcdf_cmp(f1, f2));
    putchar('\n');

    // Negation and absolute value
    puts("\nSign operations:\n");

    bcdf_from_int(f1, 42);
    puts("42 = ");
    bcdf_print(f1);
    putchar('\n');

    bcdf_neg(f1);
    puts("-42 = ");
    bcdf_print(f1);
    putchar('\n');

    bcdf_abs(f1);
    puts("abs(-42) = ");
    bcdf_print(f1);
    putchar('\n');

    puts("Done!\n");
    return 0;
}
