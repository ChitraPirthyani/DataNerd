# Name : Chitra Pirthyani
# Date : 04/23/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/YwDbMpjTtgk

def AskUser():
    Ans = int(input("Enter number in range 4 to 101: ")) # Asking user for input
    if Ans < 4 or Ans > 100 :
        print("Number out of range") # limiting input to the range in question
    else:
        Goldback(Ans) # transferring input to main function

def isPrime(num):
    if num in range(2,101):  # Calculate if a number is prime or not
        for i in range(2,num): # limiting range to the 100
            if (num%i) == 0:
                return False
                break
        else:
            return True

def Goldback(Ans):
    for i in range(2,Ans):
        if isPrime(i) == True: # getting the first prime number
            for x in range(i, Ans):
                if isPrime(x) == 1: # getting the second prime number
                    if Ans == (i+x): # checking if they sum up to the input number
                        print( Ans, "=", i, "+", x) 

while(True):
    AskUser() # creating loop to ask user to either retry, or exit
    Ask = (input("Would you like to try another number? Y or N?: "))
    if Ask== "y" or  Ask=="Y":
        loop= True
    else:
        break


    

                        





                        



