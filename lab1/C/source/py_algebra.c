#include "algebra.h"
#include "pyenv.h"

#include <stdbool.h>
#include <stdlib.h>

PyObject* algebra_module = NULL;

void initialize_pyenv() {
    Py_Initialize();

    PyObject* sys_path = PySys_GetObject("path");
    PyObject* additional_paths = Py_BuildValue("s", PY_PKG_PATH);
    PyList_Append(sys_path, additional_paths);

    printf("Initializing module: %s\n", PY_MODULE);
    algebra_module = PyImport_ImportModule(PY_MODULE);
    if (algebra_module == NULL) {
        fprintf(stderr, "Failed to import module: %s\n", PY_MODULE);
        exit(1);
    }

    Py_DECREF(additional_paths);
}

void finalize_pyenv() {
    Py_XDECREF(algebra_module);
    Py_Finalize();
}

void assert_pyenv_initialized() {
    if (algebra_module == NULL) {
        fprintf(stderr, "Module: %s not initialized\n", PY_MODULE);
        exit(1);
    }
}

uint64_t factorial(const uint64_t n) {
    assert_pyenv_initialized();

    uint64_t result = 0;

    PyObject* py_func = PyObject_GetAttrString(algebra_module, "factorial");
    if (py_func == NULL) {
        fprintf(stderr, "Failed to load function: factorial\n");
        exit(1);
    }

    PyObject* py_args = PyTuple_Pack(1, PyLong_FromUnsignedLongLong(n));
    if (py_args == NULL) {
        fprintf(stderr, "Failed to initialize arguments for function: factorial\n");
        exit(1);
    }

    PyObject* py_result = PyObject_CallObject(py_func, py_args);
    if (py_result == NULL) {
        fprintf(stderr, "Failed to initialize capture of function: factorial\n");
        exit(1);
    }

    result = PyLong_AsUnsignedLongLong(py_result);

    Py_DECREF(py_result);
    Py_DECREF(py_args);
    Py_DECREF(py_func);

    return result;
}

uint64_t gcd(uint64_t a, uint64_t b) {
    assert_pyenv_initialized();

    uint64_t result = 0;

    PyObject* py_func = PyObject_GetAttrString(algebra_module, "gcd");
    if (py_func == NULL) {
        fprintf(stderr, "Failed to load function: gcd\n");
        exit(1);
    }

    PyObject* py_args = PyTuple_Pack(2, PyLong_FromUnsignedLongLong(a), PyLong_FromUnsignedLongLong(b));
    if (py_args == NULL) {
        fprintf(stderr, "Failed to initialize arguments for function: gcd\n");
        exit(1);
    }

    PyObject* py_result = PyObject_CallObject(py_func, py_args);
    if (py_result == NULL) {
        fprintf(stderr, "Failed to initialize capture of function: gcd\n");
        exit(1);
    }

    result = PyLong_AsUnsignedLongLong(py_result);

    Py_DECREF(py_result);
    Py_DECREF(py_args);
    Py_DECREF(py_func);

    return result;
}

diophantine_solution solve_diophantine(int64_t a, int64_t b, int64_t c) {
    assert_pyenv_initialized();

    diophantine_solution solution = { .x = 0, .y = 0 };

    PyObject* py_func = PyObject_GetAttrString(algebra_module, "solve_diophantine");
    if (py_func == NULL) {
        fprintf(stderr, "Failed to load function: solve_diophantine\n");
        exit(1);
    }

    PyObject* py_args = PyTuple_Pack(3, PyLong_FromLongLong(a), PyLong_FromLongLong(b), PyLong_FromLongLong(c));
    if (py_args == NULL) {
        fprintf(stderr, "Failed to initialize arguments for function: solve_diophantine\n");
        exit(1);
    }

    PyObject* py_result = PyObject_CallObject(py_func, py_args);
    if (py_result == NULL) {
        fprintf(stderr, "Failed to initialize capture of function: solve_diophantine\n");
        exit(1);
    }

    PyObject* py_x = PyObject_GetAttrString(py_result, "x");
    PyObject* py_y = PyObject_GetAttrString(py_result, "y");

    if (py_x == NULL || py_y == NULL || !PyLong_Check(py_x) || !PyLong_Check(py_y)) {
        fprintf(stderr, "Failed to parse solution attributes\n");
        exit(1);
    }

    solution.x = PyLong_AsLongLong(py_x);
    solution.y = PyLong_AsLongLong(py_y);

    Py_DECREF(py_result);
    Py_DECREF(py_args);
    Py_DECREF(py_func);

    return solution;
}
