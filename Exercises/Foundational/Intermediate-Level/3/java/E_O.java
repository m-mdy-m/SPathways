import java.util.Scanner;
class E_O{
    public static void main(String args[]){
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter a number: ");
        Float num = scanner.nextFloat();
        String result = (num % 2 == 0) ? "even" : "odd";
        System.out.println(num + " is " + result);
        scanner.close();
    }
}