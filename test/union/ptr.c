union U {
    int  i;
    char c;
};

int main(void) {
    union U  x;
    union U* y = &x;

    x.i  = 0;
    y->c = 7;
    return x.i;
}
