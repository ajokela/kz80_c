// Array passed to function test

void print_array(char *arr, int len) {
    int i = 0;
    while (i < len) {
        putchar(arr[i]);
        i = i + 1;
    }
}

int main() {
    char msg[6];
    msg[0] = 'H';
    msg[1] = 'E';
    msg[2] = 'L';
    msg[3] = 'L';
    msg[4] = 'O';
    msg[5] = '\n';
    
    print_array(msg, 6);
    return 0;
}
