// Minimal string.h for kz80_c

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

int strcmp(char *a, char *b) {
    int i = 0;
    while (a[i] != 0 && b[i] != 0) {
        if (a[i] != b[i]) {
            return a[i] - b[i];
        }
        i = i + 1;
    }
    return a[i] - b[i];
}
