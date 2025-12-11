// Simple test program for kz80_c

int add(int a, int b) {
    return a + b;
}

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    int x = 5;
    int y = 3;
    int sum = add(x, y);
    int fact = factorial(5);
    return sum + fact;  // Should be 8 + 120 = 128
}
