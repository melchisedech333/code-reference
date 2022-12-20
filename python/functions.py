#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# Iesus Hominum Salvator s2
#

def func1():
    print("func1() called!")

def func2(param1, param2):
    print("func2(): {} {}".format(param1, param2))

func1()
func2("Iesus", "Salvator")

# Args
def func3(*arg):
    size = len(arg)
    print("func3(): len={}, arg={}".format(size, arg))

func3("a", "b")
func3("a", "b", "c")

# kwargs
def func4(**karg):
    size = len(karg)
    print("func4(): len={}, karg={}".format(size, karg))


func4(l1="a", l2="b")
func4(l1="a", l2="b", l3="c")

# Valor padrão.
def func5(value = "default value"):
    print("Value: "+ value)

func5()
func5("Hello xD")

# Função vazia.
def func6():
    pass

func6()

# Lambda functions.

x = lambda a : a + 10
print("Value: {}".format( x(10) ))

x = lambda a, b : a + b
print("Value: {}".format( x(10, 5) ))

def func7(x):
    return lambda a : a + x

func8 = func7(10)
print("Value: {}".format( func8(10) ))


