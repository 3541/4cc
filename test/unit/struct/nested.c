int main(void) {
    struct {
        struct {
            struct {
                struct {
                    char e;

                    struct {
                        char c;
                        char d;
                    } s;
                } s;
            } s;
        } s;
    } x;

    x.s.s.s.e   = 1;
    x.s.s.s.s.c = 2;
    x.s.s.s.s.d = 3;

    char* p = &x.s.s.s.e;
    return p[0] + p[1] + p[2];
}
