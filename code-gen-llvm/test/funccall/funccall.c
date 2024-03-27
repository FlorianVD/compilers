void do_print() { print_s("Test"); }

int main() {
    print(1);
    print_f(1.0);
    print8(1, 2, 3, 4, 5, 6, 7, 8);

    print_s("Hello, world!");
    do_print();

    print(read());
    print_f(read_f());

    return 0;
}
