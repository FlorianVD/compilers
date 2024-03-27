void print_value(int a) { print(a); }

void print_double_sum(int a, int b) {
    int x = 2 * (a + b);
    print(x);
}

int sum(int a, int b) { return a + b; }

int mult3(int a, int b, int c) { return a * b * c; }

int my_print8(int a, int b, int c, int d, int e, int f, int g, int h) {
    print8(a, b, c, d, e, f, g, h);
}

int main() {
    int i;
    int j;
    int k;

    i = 1;
    j = 5;
    k = 4;

    k = mult3(j, j, j);
    k = sum(k, k);
    print_value(k);

    i = 3;
    j = 4;
    print_double_sum(i, j);

    return 0;
}