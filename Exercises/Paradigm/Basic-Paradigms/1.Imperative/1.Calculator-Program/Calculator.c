#include <stdio.h>
#include <math.h>

// Function prototypes
void printMenu();
double add(double a, double b);
double subtract(double a, double b);
double multiply(double a, double b);
double divide(double a, double b);
double factorial(int n);
double performTrigonometry(char operation, double value);

int main() {
    int choice;
    double num1, num2, result;
    char trigFunc;
    int factInput;

    while (1) {
        printMenu();
        printf("Enter your choice: ");
        scanf("%d", &choice);
        
        switch (choice) {
            case 1: // Addition
                printf("Enter two numbers: ");
                scanf("%lf %lf", &num1, &num2);
                result = add(num1, num2);
                printf("Result: %.2lf\n", result);
                break;

            case 2: // Subtraction
                printf("Enter two numbers: ");
                scanf("%lf %lf", &num1, &num2);
                result = subtract(num1, num2);
                printf("Result: %.2lf\n", result);
                break;

            case 3: // Multiplication
                printf("Enter two numbers: ");
                scanf("%lf %lf", &num1, &num2);
                result = multiply(num1, num2);
                printf("Result: %.2lf\n", result);
                break;

            case 4: // Division
                printf("Enter two numbers: ");
                scanf("%lf %lf", &num1, &num2);
                if (num2 == 0) {
                    printf("Error: Division by zero is undefined.\n");
                } else {
                    result = divide(num1, num2);
                    printf("Result: %.2lf\n", result);
                }
                break;

            case 5: // Factorial
                printf("Enter an integer: ");
                scanf("%d", &factInput);
                if (factInput < 0) {
                    printf("Error: Factorial of a negative number is undefined.\n");
                } else {
                    result = factorial(factInput);
                    printf("Result: %.0lf\n", result);
                }
                break;

            case 6: // Trigonometric functions
                printf("Enter trigonometric function (s = sin, c = cos, t = tan): ");
                scanf(" %c", &trigFunc);
                printf("Enter the value in radians: ");
                scanf("%lf", &num1);
                result = performTrigonometry(trigFunc, num1);
                printf("Result: %.2lf\n", result);
                break;

            case 0: // Exit
                printf("Exiting the calculator. Goodbye!\n");
                return 0;

            default:
                printf("Invalid choice! Please try again.\n");
        }

        printf("\n");
    }
    return 0;
}

// Print the menu
void printMenu() {
    printf("========== Calculator Menu ==========\n");
    printf("1. Add\n");
    printf("2. Subtract\n");
    printf("3. Multiply\n");
    printf("4. Divide\n");
    printf("5. Factorial\n");
    printf("6. Trigonometric Functions\n");
    printf("0. Exit\n");
    printf("=====================================\n");
}

// Arithmetic operations
double add(double a, double b) { return a + b; }
double subtract(double a, double b) { return a - b; }
double multiply(double a, double b) { return a * b; }
double divide(double a, double b) { return a / b; }

// Factorial function
double factorial(int n) {
    double result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}

// Trigonometric functions
double performTrigonometry(char operation, double value) {
    switch (operation) {
        case 's': return sin(value);
        case 'c': return cos(value);
        case 't': return tan(value);
        default:
            printf("Error: Invalid trigonometric function.\n");
            return 0;
    }
}
