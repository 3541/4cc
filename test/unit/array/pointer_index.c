int main(void) {
    int  a[4];
    int* p = &a[2];
    *p     = 43;
    return *(a + 2);
}
