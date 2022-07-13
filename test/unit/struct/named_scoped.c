int main(void) {
    struct S {
        int a;
        int b;
    };

    {
        struct S {
            char a;
            char b;
        } x;

        struct S y;

        return sizeof(x) + sizeof(y);
    }
}
