int main() {
    int indices[10];
    int foo[10];

    int i = 0;
    for (i = 0; i < 10; i = i + 1) {
        indices[i] = 10 - i - 1;
    }

    for (i = 0; i < 10; i = i + 1) {
        foo[indices[i]] = i * 5;
    }

    for (i = 0; i < 10; i = i + 1) {
        print(foo[i]);
    }

    return 0;
}