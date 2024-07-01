namespace DiningPhilosophers;

using System;
using System.Threading;

class Program {
    static void Main(string[] args) {
        const byte numPhilosophers = 5;
        const byte maxMeals = 10;

        Console.WriteLine($"Starting execution with {numPhilosophers} philosophers and {maxMeals} max meals\n");

        List<Semaphore> cutlerySemaphoreList = new List<Semaphore>();
        for (byte pId = 0; pId < numPhilosophers; pId++)
            cutlerySemaphoreList.Add(new Semaphore(1, 1));

        DiningPhilosophersList diningList = new DiningPhilosophersList();

        List<Thread> philosopherThreads = new List<Thread>();
        for (byte pId = 0; pId < numPhilosophers; pId++) {
            byte philosopherId = pId;
            philosopherThreads.Add(new Thread(() => {
                Philosopher.StartThread(philosopherId, numPhilosophers, maxMeals, ref cutlerySemaphoreList, ref diningList);
            }));
        }

        foreach (Thread thread in philosopherThreads)
            thread.Start();

        foreach (Thread thread in philosopherThreads)
            thread.Join();

        Console.WriteLine("\nAll philosophers have finished eating!");
    }
}
