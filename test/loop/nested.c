int main(void) { int count = 0; for (int i = 0; i < 4; i = i + 1) { int j = 0; while (j < i) j = j + 1; count = count + j; } return count; }
