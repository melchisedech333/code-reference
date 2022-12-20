#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
    Iesus Hominum Salvator s2
    String literais
"""

print("Hello world!")

# ***
# Tipos de dados fundamentais.

# String
my_str = "Iesus"
print("Value: "+ my_str)
print("Value: ", my_str)
print("Value: {}".format(my_str))

# Bool
my_bool = True
print("Value: {}".format(my_bool))

# Integer
a = 10
b = 20
print("Value: {}". format(a + b))

# Float
c = 1.333
print("Value: {}". format(c))

# List
lst = ["apple", "banana", "cherry"]
print("Value: {}". format(lst))
print("list len: {}". format(len(lst)))
print("List item: {}". format(lst[1]))
print("List item: {}". format(lst[1:3]))

# Check exists in list.
if "banana" in lst:
    print("Yes, exists in list!")

# Change item.
lst[1] = "uvinha"
print("List item: {}". format(lst[1]))

# Add item.
lst.append("pera")
print("Value: {}". format(lst))

lst.insert(2, "abacate")
print("Value: {}". format(lst))

# Remove
lst.remove("pera")
print("Value: {}". format(lst))
lst.pop(1)
print("Value: {}". format(lst))

# Loop list
for x in lst:
    print("Item: {}". format(x))

for i in range(len(lst)):
    print("item[{}]: {}".format(i, lst[i]))

i = 0
while i < len(lst):
    print(lst[i])
    i = i + 1

newlist = []

for x in lst:
    if "a" in x:
        newlist.append(x)

print("Value: {}". format(newlist))

# Join list.

lst2 = lst + newlist
print("Value: {}". format(lst2))

# Dictionary.

dic = {
    "porta": "aberta",
    "year": 2022
}

dic['color'] = 'blue'

print("Value: {}". format(dic))


