int main(void) {
    int count = 0;

    for (int i = 0; i < 10; i = i + 1) {
        if (i > 5)
            continue;

        for (int j = 0; j < 10; j = j + 1) {
            count = count + 1;
        }
    }

    return count;
}
