# Name : Chitra Pirthyani
# Date : 05/02/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/X1la9wmd1KU

def AskUser():                                          # Funtion that asks user for row and column input
    row = int(input("Enter Row Number: "))
    column = int(input("Enter Column Number: "))
    humanPyramid(row, column)                                 # Assigning input to weight function

def weight(row,column):                                 # Recursive Function that calculates total weight of each person
    if row <0 :
        return 0
    if column <0 or column >row :
        return 0
    return 128 + (weight(row-1, column-1) + weight (row-1, column))/2   # Adding the half of weight of the person above, and self weight 128

def humanPyramid(row,column):                           # Main Function to calculate weight bore by person
    x= weight(row,column) - 128                          # Subtracting self weight to calculate weight on back
    
    print("The weight on their back is", x)

loop =  True
while (loop):
# A conditioned applied to ask the user to try a new set of numbers. 
    AskUser()
    ask = input("Do you want to test another number? Enter Y for yes:")
    if ask== "y" or ask== "Y":
        loop= True
    else:
        break








