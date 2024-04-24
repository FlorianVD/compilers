int main() {
    int fibonacci[10];
    fibonacci[0] = 1;
    fibonacci[1] = 1;

    // Should fail because we try to access fibonacci[-2] and fibonacci[-1]
    // if i = 0
    int i = 0;
    for (i = 0; i < 10; i = i + 1) {
        fibonacci[i] = fibonacci[i - 1] + fibonacci[i - 2];
    }
    for (i = 0; i < 10; i = i + 1) {
        print(fibonacci[i]);
    }

    return 0;
}