class DiophantineSolution:
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

    def __str__(self):
        return f"(x={self.x}, y={self.y})"

    def check(self, a: int, b: int, c: int) -> bool:
        return a * self.x + b * self.y == c
