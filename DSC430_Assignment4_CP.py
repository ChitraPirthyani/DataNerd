# Name : Chitra Pirthyani
# Date : 05/02/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/z33DptgEZDs

import random
def AskUser(): # Function to ask user inpot
    i= int(input("Enter number to create a random list of said numbers: "))
    n= int(input("Enter number an number range 0 to 100 : "))
    if n < 1 or n > 100:
        print("Number out of range")
    else: 
        Find(i, n) # Passing input to main function
    

def Find(i, n):
    m = random.sample(range(0, 100), i)
    m.sort() # creating random list of i numbers
    temp1= 0
    temp2=0
    for x in m:  # search function
        low = 0
        high = len(m)-1
        while low <= high:
            mid =(low+high)//2
            y=m[mid]
            temp = x+y
            if temp == n:
                temp1 = x
                temp2 = y
                break
            elif temp < n:
                high =mid-1
            elif temp >n:
                low =mid+1
            else: 
                print ("No numbers add up to", n) 
    print (temp1,"and", temp2, "add up to", n)




loop =  True
while (loop):
# A condition applied to ask the user to try a new set of numbers. 
    AskUser()
    ask = input(" Do you want to test another number? Enter Y for yes:")
    if ask== "y" or ask== "Y":
        loop= True
    else:
        break




        





