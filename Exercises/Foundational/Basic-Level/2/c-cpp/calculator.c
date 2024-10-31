#include <stdio.h>
int main(){
    float first_num,second_num;
    float sum,subtraction,multiplication;
    float division;
    printf("Enter first number: ");
    scanf("%f",&first_num);
    printf("Enter second number: ");
    scanf("%f",&second_num);
    sum = first_num + second_num;
    subtraction = first_num - second_num;
    multiplication = first_num * second_num;
    division = first_num / second_num;
    printf("%.2f + %.2f = %.2f\n",first_num,second_num,sum);
    printf("%.2f - %.2f = %.2f\n",first_num,second_num,subtraction);
    printf("%.2f * %.2f = %.2f\n",first_num,second_num,multiplication);
    printf("%.2f / %.2f = %.2f\n",first_num,second_num,division);
    return 0;
}