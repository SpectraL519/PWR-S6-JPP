import abc


class AbstractGaloisField(abc.ABC):
    @abc.abstractclassmethod
    def order(cls) -> int:
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.order.__name__)

    @abc.abstractstaticmethod
    def type_order() -> int:
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.type_order.__name__)

    @abc.abstractmethod
    def __int__(self):
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.__int__.__name__)

    @abc.abstractmethod
    def __add__(self, other):
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.__add__.__name__)

    @abc.abstractmethod
    def __sub__(self, other):
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.__sub__.__name__)

    @abc.abstractmethod
    def __mul__(self, other):
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.__mul__.__name__)

    @abc.abstractmethod
    def __truediv__(self, other):
        AbstractGaloisField._raise_not_implemented_func_error(AbstractGaloisField.__truediv__.__name__)

    @staticmethod
    def _raise_not_implemented_func_error(func_name: str) -> None:
        raise NotImplementedError(f"No implementation provided for AbstractGaloisField.{func_name}")
