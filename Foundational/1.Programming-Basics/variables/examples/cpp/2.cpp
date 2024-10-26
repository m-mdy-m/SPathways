#include <stdio.h>
void foo(){
int x;
printf("The address of x in foo() is: %d\n", &x);
}
void bar(){
printf("Called from bar(),");
foo();
}
// ---------- main ----------s
int main()
{
int i = 0; 
foo();
bar();
return 0;
}