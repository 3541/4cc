int main(void) {
    int i = 0;
    for (; i < 10; i = i + 1) {
        if (i > 5)
            break;
    }

    return i;
}
