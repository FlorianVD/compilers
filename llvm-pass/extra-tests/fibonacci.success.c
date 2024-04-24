int main() {
    int fibonacci[10];
    fibonacci[0] = 1;
    fibonacci[1] = 1;

    int i = 2;
    for (i = 2; i < 10; i = i + 1) {
        fibonacci[i] = fibonacci[i - 1] + fibonacci[i - 2];
    }
    for (i = 0; i < 10; i = i + 1) {
        print(fibonacci[i]);
    }

    return 0;
}