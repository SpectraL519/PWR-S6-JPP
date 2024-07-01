package main

import (
	"fmt"
	"os"
	"sync"

	"github.com/akamensky/argparse"
)

// type definitions

type EmptyStruct struct{}

// global variables

var (
	numPhilosophers  PhilosopherId
	iNumPhilosophers int
	maxMeals         uint16

	diningPhilosopherList DinigPhilosophersList
	cutlerySemaphoreList  []Semaphore

	waitGroup sync.WaitGroup
)

const (
	MAX_EATING_TIME      int = 1000
	EATING_TIME_OFFSET   int = 200
	MAX_THINKING_TIME    int = 1000
	THINKING_TIME_OFFSET int = 500
)

// main

func main() {
	parser := argparse.NewParser("philosophers", "Dining philosophers problem simulation")

	numPhilosophersParam := parser.Int("p", "philosophers", &argparse.Options{Default: 5})
	maxMealsParam := parser.Int("m", "max-meals", &argparse.Options{Default: 10})

	if err := parser.Parse(os.Args); err != nil {
		fmt.Fprintln(os.Stderr, "Error: Invalid arguments!")
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}

	numPhilosophers = PhilosopherId(*numPhilosophersParam)
	iNumPhilosophers = int(numPhilosophers)
	maxMeals = uint16(*maxMealsParam)

	fmt.Println("Args parsed")
	fmt.Printf("\tphilosophers = %d\n", numPhilosophers)
	fmt.Printf("\tmax-meals = %d\n", maxMeals)
	fmt.Println("Starting execution!\n")

	cutlerySemaphoreList = make([]Semaphore, 0)
	for i := 0; i < iNumPhilosophers; i++ {
		cutlerySemaphoreList = append(cutlerySemaphoreList, NewBinarySemaphore())
	}

	diningPhilosopherList = newDinigPhilosophersList()

	for i := 0; i < iNumPhilosophers; i++ {
		waitGroup.Add(1)
		go startPhilosopher(newPhilosopher(PhilosopherId(i)))
	}

	fmt.Println("\nAll philosophers have finished eating!")

	waitGroup.Wait()
}
