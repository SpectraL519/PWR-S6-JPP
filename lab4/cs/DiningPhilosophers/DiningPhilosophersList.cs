namespace DiningPhilosophers;

using System;
using System.Threading;

public class DiningPhilosophersList {
    public void AddPhilosopher(Philosopher p) {
        this._mutex.WaitOne();
        this._philosophers.Add(p);
        this.Print();
        this._mutex.ReleaseMutex();
    }

    public void RemovePhilosopher(ulong pId) {
        this._mutex.WaitOne();
        this._philosophers.RemoveAll(p => p.Id == pId);
        this.Print();
        this._mutex.ReleaseMutex();
    }

    private void Print() {
        Console.Write("> ");
        foreach (var philosopher in this._philosophers)
            Console.Write($"{philosopher} ");
        Console.WriteLine();
    }

    private Mutex _mutex = new Mutex();
    private readonly List<Philosopher> _philosophers = new List<Philosopher>();
}
