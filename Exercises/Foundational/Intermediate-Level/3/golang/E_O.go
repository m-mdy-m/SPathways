package main;
import (
	"fmt"
)
func main(){
	var(
		result string
		num float64
	)
	fmt.Print("Enter a number: ")
	fmt.Scan(&num)
	if int(num)%2 == 0 {
		result = "even"
	} else {
		result = "odd"
	}

	fmt.Println(num,"is",result)
}