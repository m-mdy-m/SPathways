#include <stdio.h>
// Declaration of Variable
extern int p, q;
extern int c;
extern float f;
int main () {
 /* variable definition: */
 int p, q,r;
 float i;
 /* actual initialization */
 p = 10;
 q = 20;
 r = p + q;
 printf("the value of r : %d \n", r);
 i = 70.0/3.0;
 printf("the value of i : %f \n", i);
 return 0;
}