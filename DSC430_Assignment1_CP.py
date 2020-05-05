# Name : Chitra Pirthyani
# Date : 04/08/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/w2XvU0C_jcY

def coprime_test_loop():
#Function that demands two integars from the user.
    a= int(input("Enter first number:"))
    b= int(input("Enter second number:" ))
    coprime(a,b) #passing the input onto the coprime function

def coprime(a,b):
# Function that checks whether two numbers are co-prime or not.
    while b != 0:
        a,b= b, a%b  #Basically evaluate the gcd of two numbers.
    if a==1: # If the largest gcd is 1, it prints co-prime, or else it prints not co-prime
        print("Co-prime")
    else:
        print ("Not Co-prime")

loop =  True
while (loop):
# A conditioned applied to ask the user to try a new set of numbers. 
    coprime_test_loop()
    ask = input(" Do you want to test another pair? Enter Y for yes:")
    if ask== "y" or ask== "Y":
        loop= True
    else:
        break
    
        
    








