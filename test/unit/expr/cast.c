#include "test.h"

int main(void) {
    (void)0;

    __isize num = 1234567891011;
    ASSERT_EQ((__i32)num, 1912277059);
    ASSERT_EQ((__i16)num, 2115);
    ASSERT_EQ((__i8)num, 67);

    ASSERT_EQ((__isize)num, num);
    ASSERT_EQ((__u8)0, 0);

    __usize x  = 1073741824;
    *(__u8*)&x = 1;
    ASSERT_EQ(x, 1073741825);
}
