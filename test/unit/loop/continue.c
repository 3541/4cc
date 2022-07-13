int main(void) {
    int count = 0;

    for (int i = 0; i < 10; i = i + 1) {
        if (i == 3 || i == 5)
            continue;
        count = count + 1;
    }

    return count;
}
