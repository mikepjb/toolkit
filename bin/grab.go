package main

// A translation of selecta into go.

import (
  "fmt"
  "bufio"
  "os"
)

func main() {
  reader := bufio.NewReader(os.Stdin)
  input, _ := reader.ReadString('\n')

  fmt.Printf("Input Char Is : %v", string([]byte(input)[0]))
}
