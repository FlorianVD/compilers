// Rewrite of control-flow to not use a function
int main() {
    int i = read();
    int res;
    int first = 0;
    int second = 1;
    int tmp;
    int n;

    while (i >= 0) {
        n = i;
        first = 0;
        second = 1;

        if (n < 0) {
            res = -1; // invalid argument
        } else {
            if (n == 0) {
                res = first;
            } else if (n == 1) {
                res = second;
            } else {
                for (i = 0; i < n; i = i + 1) {
                    tmp = first; // remember old value of first

                    first = second;
                    second = tmp + second;
                }
                res = first;
            }
        };

        print(res);

        i = read();
    }

    return 0;
}
