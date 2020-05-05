# Name : Chitra Pirthyani
# Date : 04/08/20
# Honor Statement : "I have not given or received any unauthorized assistance on this assignment"
# YouTube Link : https://youtu.be/qyvFGKMvX7Q

import math
def name():
    # Fuction to check if the file has student name.
    ans = input('Does the file have the students name on it? Yes or No: ')
    if (ans == 'Yes'):
        return True
    return False

def Date():
    # Fuction to check if the file has date.
    ans = input('Does the file have the date on it? Yes or No: ')
    if (ans == 'Yes'):
        return True
    return False

def Honor():
    # Fuction to check if the file has honor statement.
    ans= input("Does the file have the honor statement in it? Yes or No? :")
    if (ans == 'Yes'):
        return True
    return False


def YouTube():
    # Fuction to check if the file has Youtube link attached.
    ans= input('Does the file have the youtube link it? Yes or No? :')
    if (ans == 'Yes'):
        return True
    return False

def correct():
    # Fuction to grade on correctness of code. 
    ans= input('Out of 10 points, how would you evaluate the correctness of the code:')
    return int(ans)

def elegance():
    # Fuction to grade on elegance, algorithm efficiency, fucntion implementation.
    ans= input('Out of 10 points, how would you evaluate the elegance of the code:')
    return int(ans)

def hygiene():
    # Fuction to grade on cleanliness, whitespace, docstrings of the code.
    ans= input('Out of 10 points, how would you evaluate the hygiene of the code:')
    return int(ans)

def discussion():
    # Fuction to grade on quality of discusiion on the Youtube video.
    ans= input('Out of 10 points, how would you evaluate the quality of discussion on Youtube:')
    return int(ans)

def late():
    # Fuction to evaluate late penalty. 
    ask = input('Was the assignment submitted late? Yes or No? :')
    if ask == 'Yes':
        ask = input('How many hours late was it submitted?:')
        return int(ask)
    return False

def TotalScore():
    if not (name()): 
        return 0
    if not (Date()):
        return 0
    if not (Honor()):
        return 0
    if not (YouTube()):
        return 0 # these conditions ensure that the four requirements are met. If not, the total score becomes 0, and the user is not asked any more questions. 
    
    correctness =correct()
    codeelegance=elegance()
    codehygiene=hygiene()
    videodiscussion=discussion()
    iflate=late()   
    return correctness + codeelegance + codehygiene+ videodiscussion - iflate*0.4
    # 0.4 is calculated by the 1% penalty on the total 40 points. 

    
print (" Total score is " + str(TotalScore()))

