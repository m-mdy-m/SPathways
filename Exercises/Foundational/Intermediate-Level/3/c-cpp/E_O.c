#include <stdio.h>
int main(){
    float num;
    char *result;
    printf("Enter a number: ");
    scanf("%f",&num);
    result = (int)num%2==0?"even":"odd";
    printf("%.2f is %s\n",num,result);
}