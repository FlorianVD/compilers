/**
 * Test the scenario where a user defines a clamp/clip function to handle incorrect
 * index accesses. Because these accesses' indices are clipped, all access should be safe.
 */

int clamp(int value, int min, int max) {
    if (value < min)
        return min;
    if (value > max)
        return max;
    return value;
}

int main() {
    int foo[10];
    int i = -10;
    for (i = -10; i < 20; i = i + 1) {
        int index = clamp(i, 0, 9);
        foo[index] = i;
        print(i);
    }

    return 0;
}