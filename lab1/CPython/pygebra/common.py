import ctypes


class DiophantineSolution(ctypes.Structure):
    _fields_ = [
        ('x', ctypes.c_int64),
        ('y', ctypes.c_int64),
    ]

    def __str__(self):
        return f"(x={self.x}, y={self.y})"

    def check(self, a: int, b: int, c: int) -> bool:
        return a * self.x + b * self.y == c
