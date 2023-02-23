#include <am.h>
#include <klib.h>

volatile int load_test_buffer1 = 0;
volatile int load_test_buffer2 = 0;

int main() {
  int i = 1;
  asm(
    "sw %1, 0(%0)\n"
    "sw %1, 0(%2)\n"
    "fence\n"
    "lw %1, 0(%0)\n"
    "lw %1, 0(%2)\n"
    :
    :"r"(&load_test_buffer1), "r"(i), "r"(&load_test_buffer2)
  );
  assert(0);
  return 0;
}