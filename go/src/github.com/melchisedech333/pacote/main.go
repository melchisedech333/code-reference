package main

import (
	"fmt"
	"github.com/melchisedech333/pacote/utils"
)

func main() {
	utils.Version()
	fmt.Printf("10 + 10 = %v\n", utils.Sum(10, 20))
	fmt.Printf("config = %v\n", utils.UCONFIG)
}


