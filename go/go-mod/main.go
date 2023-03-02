package main

import (
	"fmt"
	"github.com/google/uuid"
)

func main(){
	uuid := uuid.New()
	fmt.Printf("UUID: %v\n", uuid)
}


