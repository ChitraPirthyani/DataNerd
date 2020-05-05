# Name : Chitra Pirthyani
# Date : 04/23/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/sejbHpf_rRw


def AskUser():
    n = int(input("Enter number:  ")) # Asking the user for input
    isHappy(n) # transferring the input to the main function

def isPrime(n):
    for i in range(2,n): # Checking if the number is Prime or Non-Prime
        if (n%i) == 0:
            return False
    return True 

def happy(n):
    # function to check if the number is Happy or Sad
    l= [] # creating empty list
    while n!= 1: 
        n = sum(int(i)**2 for i in str(n)) # Sqauring the digit of the number and then adding them
        if n in l:
            return False
        l.append(n) # appending to list
    return True

            
def isHappy(n):
    # function to check if the input is Happy prime, Sad prime, Happy non-prime, sad non_prime
    if isPrime(n) == True and happy(n) == True:
        print(n, "is a Happy Prime")
    elif isPrime(n) == True and happy(n) == False:
        print(n, "is a Sad Prime")
    elif isPrime(n) == False and happy(n) == True:
        print(n, "is a Happy Non-Prime")
    else:
        print(n, "is a Sad Non-Prime")

while(True):
    AskUser() # while loop to ask user for another input or to exit
    ask = input("Would you like to test another number? Y or N: ")
    if ask == "y" or ask == "Y":
        loop = True
    else:
        break

