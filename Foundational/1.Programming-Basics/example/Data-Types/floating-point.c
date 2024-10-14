#include <stdio.h>
#include <math.h>

int main() {
    float a = 0.1f;
    float b = 0.2f;
    float sum = a + b;

    // Demonstrating rounding error
    printf("Sum: %.20f\n", sum);  // Output might not be exactly 0.30000000000000000000

    // Demonstrating equality comparison with epsilon
    float epsilon = 0.00001f;
    if (fabs(sum - 0.3f) < epsilon) {
        printf("a + b is approximately 0.3\n");
    } else {
        printf("a + b is not 0.3\n");
    }

    // Using built-in function (sin) and max function
    double angle = 0.5;
    double sineValue = sin(angle);
    double maxValue = fmax(3.14, 2.718);

    printf("Sine of 0.5: %f\n", sineValue);
    printf("Maximum value: %f\n", maxValue);

    return 0;
}