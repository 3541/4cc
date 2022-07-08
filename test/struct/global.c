struct S {
    int a;
    int b;
};

struct S x;

int main(void) {
    x.a = 4;

    return sizeof(x) + x.a;
}
