package main
import(
	"fmt"
)
func main(){
	var (
		first_num, second_num float64
		sum,sub,div,mul float64
	) 
	fmt.Print("Enter first number: ")
	fmt.Scan(&first_num)
	fmt.Print("Enter second number: ")
	fmt.Scan(&second_num)
	sum = first_num + second_num
	sub = first_num - second_num
	mul = first_num * second_num
	div = first_num / second_num
	fmt.Println(first_num,"+",second_num,"=",sum)
	fmt.Println(first_num,"-",second_num,"=",sub)
	fmt.Println(first_num,"*",second_num,"=",mul)
	fmt.Println(first_num,"/",second_num,"=",div)
}