import os.path
from os import path
import math

# Name : Chitra Pirthyani
# Date : 04/16/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/6wkS6CUo4rE

Dict = {}

def ReadFile(filename, Dict):
    f = open(filename, "r")
    line = f.read().split()
    Dict.clear()
    for value in line:
        if len(value)== 2:
            stem = int(value[0])
            leaf = int(value[1])
        elif len(value) == 3:
            stem = int(value[0:2])
            leaf = int(value[2])
        elif len(value)== 4:
            stem = int(value[0:3])
            leaf = int(value[3])
        if stem in Dict:
            Dict[stem].append(leaf)
        else:
            Dict[stem] = [leaf]
    plot()

def plot():
    if len(Dict) == 0:
        print("-")
        return
    for k,v in Dict.items():
        leaves= " | "
        for x in sorted(v):
            leaves = leaves + " " + str(x)
        print(k, leaves)
    print()


greeting=print("Welcome!\n Please enter your datafile to create a stem and leaf plot")
    


while(True):
    ans = input("Enter 1,2,3 or exit: ")
    if ans== "1":
        filename= "/Users/chitrapirthyani/Documents/DSC430/DSC430_Week2/StemAndLeaf1.txt"
    elif ans== "2":
        filename = "/Users/chitrapirthyani/Documents/DSC430/DSC430_Week2/StemAndLeaf2.txt"
    elif ans == "3":
        filename = "/Users/chitrapirthyani/Documents/DSC430/DSC430_Week2/StemAndLeaf3.txt"
    elif ans == "exit":
        break
    ReadFile(filename, Dict)



