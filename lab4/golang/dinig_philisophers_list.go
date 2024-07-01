package main

import (
	"fmt"
	"sync"
)

type CutleryId uint8

type DinigPhilosophersList struct {
	philosophers []Philosopher
	mutex        *sync.Mutex
}

func newDinigPhilosophersList() DinigPhilosophersList {
	return DinigPhilosophersList{
		philosophers: make([]Philosopher, 0),
		mutex:        &sync.Mutex{},
	}
}

func (dpl *DinigPhilosophersList) addPhilosopher(p *Philosopher) {
	dpl.mutex.Lock()
	defer dpl.mutex.Unlock()

	dpl.philosophers = append(dpl.philosophers, *p)
	fmt.Println("> ", dpl.philosophers)
}

func (dpl *DinigPhilosophersList) removePhilosopher(p *Philosopher) {
	dpl.mutex.Lock()
	defer dpl.mutex.Unlock()

	for i, dp := range dpl.philosophers {
		if dp.id != p.id {
			continue
		}

		dpl.philosophers = append(dpl.philosophers[:i], dpl.philosophers[i+1:]...)
		break
	}

	fmt.Println("> ", dpl.philosophers)
}
