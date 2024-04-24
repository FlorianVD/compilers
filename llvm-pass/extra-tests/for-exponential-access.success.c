int main() {
    int foo[32];
    int i = 1;
    for (i = 0; i < 32; i = i + 1) {
        foo[i] = 0;
    }
    for (i = 1; i < 32; i = i * 2) {
        foo[i] = i;
    }
    for (i = 0; i < 32; i = i + 1) {
        print(foo[i]);
    }

    return 0;
}