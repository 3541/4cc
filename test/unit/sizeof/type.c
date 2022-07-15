#include "test.h"

int main(void) {
    ASSERT_EQ(sizeof(int), 4);
    ASSERT_EQ(sizeof(long), 8);
    ASSERT_EQ(sizeof(int[2]), 8);
    ASSERT_EQ(sizeof(struct {
                  char a;
                  int  b;
                  long c;
              }),
              16);
    ASSERT_EQ(sizeof(int* [2]), 16);
    ASSERT_EQ(sizeof(int(*)[2]), 8);
    ASSERT_EQ(sizeof(struct {
                  int a;
                  int b;
                  int c;
              }[20]),
              240);
}
