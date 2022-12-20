#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# Iesus Hominum Salvator s2
#

# Condicionais.

a = 20
b = 10

if a < b:
    print("A menor que B")

if a > b:
    print("A maior que B")
elif a == b:
    print("A igual a B")
else:
    print("Opcao padrao")

# While

i = 1
while i < 3:
    print(i)
    i += 1

# For

fruits = ["apple", "banana", "cherry"]
for x in fruits:
    if x == "banana":
        break
    print(x)

for x in range(2, 6):
    print(x)


