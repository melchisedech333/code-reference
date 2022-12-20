#!/usr/bin/python
# -*- coding: utf-8 -*-

class Animal:

    # Constructor
    def __init__(self, p1, p2):
        self.param1 = p1
        self.param2 = p2

animal = Animal("str1", 10)

print("Values: {}, {}".format(animal.param1, animal.param2))

# Herança.

class Dog(Animal):
    def __init__(self, p1, p2):
        Animal.__init__(self, p1, p2)

    def print_values(self):
        print("Dog: {}, {}".format(self.param1, self.param2))

dog = Dog("a", "b")
dog.print_values()


