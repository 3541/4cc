union U {
    int  i;
    char c;
};

int main(void) {
    union U x;
    x.i = 22;

    union U y = x;
    return y.c;
}
