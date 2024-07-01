namespace DiningPhilosophers;

using System;
using System.Threading;

public class Philosopher {
    public Philosopher(byte id, byte numPhilosophers = 1) {
        this._id = id;
        this._leftCutleryId = id;
        this._rightCutleryId = (byte)((id + 1) % numPhilosophers);
    }

    public ulong Id {
        get { return this._id; }
    }

    public override string ToString()
        => $"(C{this._leftCutleryId}-P{this._id}-C{this._rightCutleryId}:M{this._mealsEaten})";

    public static void StartThread(
        byte pId,
        byte numPhilosophers,
        byte maxMeals,
        ref List<Semaphore> cutlerySemaphoreList,
        ref DiningPhilosophersList diningList
    ) {
        Philosopher p = new Philosopher(pId, numPhilosophers);

        while (true) {
            // think
            Thread.Sleep(Philosopher.s_random.Next(Philosopher.MaxRandTime) + Philosopher.ThinkingTimeOffset);

            // acquire cutlery
            cutlerySemaphoreList[p._leftCutleryId].WaitOne();
            cutlerySemaphoreList[p._rightCutleryId].WaitOne();

            // add to dining list
            p._mealsEaten++;
            diningList.AddPhilosopher(p);

            // eat
            Thread.Sleep(Philosopher.s_random.Next(Philosopher.MaxRandTime) + Philosopher.EatingTimeOffset);

            // remove from dining list
            diningList.RemovePhilosopher(p._id);

            // release cutlery
            cutlerySemaphoreList[p._leftCutleryId].Release();
            cutlerySemaphoreList[p._rightCutleryId].Release();

            if (p._mealsEaten == maxMeals) {
                Console.WriteLine($"[Finished eating: id = {p._id}]");
                return;
            }
        }
    }

    private readonly byte _id;
    private readonly byte _leftCutleryId;
    private readonly byte _rightCutleryId;
    private byte _mealsEaten = 0;

    private static Random s_random = new Random();
    private static readonly int MaxRandTime = 1000;
    private static readonly int EatingTimeOffset = 200;
    private static readonly int ThinkingTimeOffset = 500;
}
