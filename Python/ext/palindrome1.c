#include <Python.h>
#include <string.h>

/**
 * 定义的目标函数，函数的返回值必须是*PyObject，self指向模块本身，args参数包含所有传入的参数
 */
static PyObject *is_palindrom(PyObject *self, PyObject *args){
    int i, n;
    const char *text;
    // 传入的参数可以使用PyArg_ParseTuple按一定的格式解析到对应的变量中，其中s表示是字符串
    if (!PyArg_ParseTuple(args, "s", &text)){
        return NULL;
    }
    n = strlen(text);
    // 这次将返回值改成返回True和False
    for (i = 0; i<= n/2; i++){
        if (text[i] != text[n-i-1]){
            Py_INCREF(Py_False);
            return Py_False;
        }
    }
    // 需要增加Py_True的引用计数的数量
    Py_INCREF(Py_True);
    // 如果是要返回整数1的话可以使用Py_BuildValue("i", 1);
    return Py_True;
}

// 导出的方法列表定义，在PyModuleDef中使用，而PyModuleDef又在PyInit_modulename上使用
static PyMethodDef PalindromeMethods[] = {
    // 名称(这影响在python中调用时的名字)、具体的函数、参数类型、文档
    {"is_palindrom1", is_palindrom, METH_VARARGS, "Dected palindromes"},
    // 列表结束标志
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef palindrome = 
{
    PyModuleDef_HEAD_INIT,
    "palindrome1", // 模块名，好像改完之后没有啥效果，只是在help时会在NAME里体现
    "", // 文档
    -1, // 存储在全局变量中的信号状态
    PalindromeMethods // 方法列表
};

// 初始化模块的函数，PyInit_modulename，名字必须是这样的命名规则
PyMODINIT_FUNC PyInit_palindrome1(void){
    return PyModule_Create(&palindrome);
}


// 编译: gcc -I/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/include/python3.8/ -shared  -lpython3.8 -L/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/lib/ palindrome1.c -o palindrome1.so -v

// >>> import palindrome1
// >>> palindrome1.is_palindrom1("FD")
// False
// >>> palindrome1.is_palindrom1("ABBA")
// True