union U {
    int  i;
    char c;
};

int main(void) {
    union U x;

    x.i = 0;
    x.c = 2;
    return x.i;
}
