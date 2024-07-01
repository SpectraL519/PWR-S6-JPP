package main

import (
	"fmt"
	"math/rand"
	"time"
)

type PhilosopherId uint8

type Philosopher struct {
	id         PhilosopherId
	mealsEaten uint16
}

func newPhilosopher(id PhilosopherId) Philosopher {
	return Philosopher{
		id:         id,
		mealsEaten: 0,
	}
}

func (p Philosopher) String() string {
	return fmt.Sprintf("(C%d-P%d-C%d:M%d)", p.leftCutleryId(), p.id, p.rightCutleryId(), p.mealsEaten)
}

func (p *Philosopher) leftCutleryId() CutleryId {
	return CutleryId(p.id)
}

func (p *Philosopher) rightCutleryId() CutleryId {
	return CutleryId((p.id + 1) % numPhilosophers)
}

func (p *Philosopher) acquireCutlery() {
	cutlerySemaphoreList[p.leftCutleryId()].Acquire()
	cutlerySemaphoreList[p.rightCutleryId()].Acquire()

	p.mealsEaten++

	diningPhilosopherList.addPhilosopher(p)
}

func (p *Philosopher) releaseCutlery() {
	diningPhilosopherList.removePhilosopher(p)

	cutlerySemaphoreList[p.leftCutleryId()].Release()
	cutlerySemaphoreList[p.rightCutleryId()].Release()
}

func (p *Philosopher) think() {
	time.Sleep(time.Millisecond * time.Duration(rand.Intn(MAX_THINKING_TIME)+THINKING_TIME_OFFSET))
}

func (p *Philosopher) eat() bool {
	p.acquireCutlery()
	time.Sleep(time.Millisecond * time.Duration(rand.Intn(MAX_EATING_TIME)+EATING_TIME_OFFSET))
	p.releaseCutlery()

	return p.mealsEaten == maxMeals
}

func startPhilosopher(p Philosopher) {
	defer waitGroup.Done()

	for {
		p.think()
		if finishedEating := p.eat(); finishedEating {
			fmt.Printf("[Finished eating: id = %d]\n", p.id)
			return
		}
	}
}
