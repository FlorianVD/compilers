int main() {
    int foo[10];
    int i = 0;
    for (i = 0; i < 20; i = i + 1) {
        foo[i] = i;
        print(i);
    }

    return 0;
}