import java.util.Scanner;
class Calculator{
    public static void main(String[] args){
        Scanner myObj = new Scanner(System.in);
        System.out.println("Enter first number: ");
        Float first_num = myObj.nextFloat();
        System.out.println("Enter second number: ");
        Float second_num = myObj.nextFloat();
        Float sum,mul,div,sub;
        sum = first_num + second_num;
        sub = first_num - second_num;
        mul = first_num * second_num;
        div = first_num / second_num;
        System.out.println(first_num+ " + " +second_num+" = "+sum);
        System.out.println(first_num+ " - " +second_num+" = "+sub);
        System.out.println(first_num+ " * " +second_num+" = "+mul);
        System.out.println(first_num+ " / " +second_num+" = "+div);
    }
}