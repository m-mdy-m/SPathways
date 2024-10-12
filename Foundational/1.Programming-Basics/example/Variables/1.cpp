#include <iostream>
int main(){
    std::cout << "the size of an int is:\t\t";
    std::cout << sizeof(int) << "bytes.\n";
    std::cout << "The size of a short int is:\t";
    std::cout << sizeof(short) << "bytes.\n";
    std::cout << "The size of a long int is:\t";
    std::cout << sizeof(long) << "bytes.\n";
    std::cout << "The size of a char is:\t\t";
    std::cout << sizeof(char) << "bytes.\n";
    std::cout << "The size of a bool is:\t\t";
    std::cout << sizeof(bool) << "bytes.\n";
    std::cout << "The size of a float is:\t\t";
    std::cout << sizeof(float) << "bytes.\n";
    std::cout << "The size of a double is:\t";
    std::cout << sizeof(double) << "bytes.\n";
    return 0;
}