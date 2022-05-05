
#include <stdio.h>

int main() {
  printf("Read some ints...\n");
  fflush(stdout);

  for (int i = 1; i <= 3 ; i++) {
    int x;
    printf("Prompt(%d) : \n", i);
    fflush(stdout);
    scanf("%d",&x);
    printf("I just read(%d) an int: %d\n", i, x);
    fflush(stdout);
  }
  printf("I am done now\n");
  fflush(stdout);
  return 0;
}
