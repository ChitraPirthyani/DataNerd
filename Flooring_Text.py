import urllib
from urllib.request import urlopen
from bs4 import BeautifulSoup, SoupStrainer
import requests
import re
import ssl
import collections
from collections import Counter
import string
from string import punctuation
import time
import nltk
nltk.download('punkt')
nltk.download('stopwords')
from nltk import ngrams
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
ssl._create_default_https_context = ssl._create_unverified_context

link= []
text = []
link2 =[]
link3 =[]
records=[]

# Getting Google Search Page URL Links
try: 
    from googlesearch import search 
except ImportError:  
    print("No module named 'google' found") 

query = "flooring Chicago"

for j in search(query, tld="com", num=10,start=0, stop=10, pause=2.0): 
    link.append(j)
    
# Google search Page 2

try: 
    from googlesearch import search 
except ImportError:  
    print("No module named 'google' found") 
 
query = "flooring Chicago"

for a in search(query, tld="com", num=10,start=10, stop=20, pause=2.0): 
    link2.append(a)

# Google search Page 3

try: 
    from googlesearch import search 
except ImportError:  
    print("No module named 'google' found") 
 
query = "flooring Chicago"

for b in search(query, tld="com", num=10,start=20, stop=20, pause=2.0): 
    link3.append(b)

## Common Words for Page 1 ##

for i in link:
    url = (i)
    page= requests.get(url)
    data = page.text
    soup = BeautifulSoup(data, features='lxml')

    page = BeautifulSoup(requests.get(url).text, "lxml")
    htag_list =[]
    htag_listoflist =[]
    for headlines in page.find_all(["h1", "h2", "h3","h4","h5","h6"]):
        htag = (headlines.name + ' ' + headlines.text.strip())
        htag_list.append(htag)
        htag_listoflist.append(htag_list)

    ### Print below two statements to get page-1 h-tags ###
        ("\n List of all the headers in", i)
        (headlines.name + ' ' + headlines.text.strip())

    joint_words = ' '.join(text)
    separated_words = word_tokenize(joint_words)

    stop_words = stopwords.words('english')

    separated_words1 = [''.join(c for c in s if c not in punctuation) for s in separated_words]
    filtered_words = [word for word in separated_words1 if word not in stopwords.words('english')]

    c = Counter(filtered_words)
    common = c.most_common(15)
    records.append((i, htag_listoflist, common))

### Print below statement to get page-1 common words ###
    ("\nMost Common words (pg1) are:\n", common)


    ### Top 5 phrases for EACH of the Top 10 Websites ####
    
    for i in link[0:10]:
        url = (i)
        phrase0=[]
        phrase = BeautifulSoup(requests.get(i).text, "lxml")
        for headlines in phrase.find_all(["h1", "h2", "h3","h4","h5","h6"]):
            phrase0.append(headlines.text.strip())

            joining = word_tokenize(''.join(phrase0))
            joining1 = [''.join(c for c in s if c not in punctuation) for s in joining]
            joining11 = [word for word in joining1 if word not in stopwords.words('english')]
            sentence1 = nltk.ngrams(joining11, 6)
        
            phrase_counter1 = Counter(sentence1)
            common_phrases1 = phrase_counter1.most_common(6)

### Print below statement to get phrases ###
        ("For the link:",i ,'\n',common_phrases1)


###############################################################################################

# Extracting P-tags for each website on page 1 

try:

    p_text =[]

    for i in link:
        url = (i)
        page= requests.get(url)
        data = page.text
        soup = BeautifulSoup(data, features='lxml')

        page = BeautifulSoup(requests.get(url).text, "lxml")
        
        for p_tags in page.find_all("p"):
            (p_tags.name + ' ' + p_tags.text.strip())
            p_text.append(p_tags.text.strip())

### Print below statements to get p-tags ###
            ("\n List of all the p- tags in", i)
            (p_tags.name + ' ' + p_tags.text.strip())


    #### Top 5 phrases in P-tags from each website #########

            P_joining = word_tokenize(''.join(p_text))
            p_joining1 = [''.join(c for c in s if c not in punctuation) for s in P_joining]
            p_joining11= [word for word in p_joining1 if word not in stopwords.words('english')]
            p_sentence = nltk.ngrams(p_joining11, 6)

            p_phrase_counter = Counter(p_sentence)
            p_common_phrases2 = p_phrase_counter.most_common(6)

### Print below statement to get p-tag phrases ###
            ("For the link:",i ,'\n',p_common_phrases2)




except requests.exceptions.ConnectionResetError:
    print('Handle Exception')

except requests.exceptions.ConnectionError:
    print('Handle Exception')

except ConnectionResetError:
    print('Handle Exception')    


## Common words for Page 2 ## 
    text2=[]
    records2=[]

    for x in link2:
        url = (x)
        page= requests.get(url)
        data = page.text
        soup = BeautifulSoup(data, features='lxml')

        page = BeautifulSoup(requests.get(url).text, "lxml")
        htag_list2 =[]
        htag_listoflist2 =[]
        for headlines in page.find_all(["h1", "h2", "h3","h4","h5","h6"]):
            htag2 = (headlines.name + ' ' + headlines.text.strip())
            htag_list2.append(htag2)
            htag_listoflist2.append(htag_list2)
            text2.append(headlines.name + ' ' + headlines.text.strip()) 
                    
### Print below two statements to get page-2 h-tags ###
            ("\n List of all the headers in", x)
            (headlines.name + ' ' + headlines.text.strip())

        joint_words2 = ''.join(text2)
        separated_words2 = word_tokenize(joint_words2)
        stop_words = stopwords.words('english')
        separated_words2 = [''.join(c for c in s if c not in punctuation) for s in separated_words2]
        filtered_words2 = [word for word in separated_words2 if word not in stopwords.words('english')]

        c2 = Counter(filtered_words2)
        common2 = c2.most_common(15)
        records2.append((x, htag_listoflist2, common2))
    
### Print below statement to get page-2 common words ###
    ("\nMost Common words (pg2) are:\n", common2)


## Common words for Page 3 ##

    text3=[]
    records3=[]

    for y in link3:
        url = (y)
        page= requests.get(url)
        data = page.text
        soup = BeautifulSoup(data, features='lxml')

        page = BeautifulSoup(requests.get(url).text, "lxml")
        htag_list3 =[]
        htag_listoflist3 =[]
        
        for headlines in page.find_all(["h1", "h2", "h3","h4","h5","h6"]):
            htag3=(headlines.name + ' ' + headlines.text.strip())
            htag_list3.append(htag3)
            htag_listoflist3.append(htag_list3)
            text3.append(headlines.name + ' ' + headlines.text.strip()) 

         ### Print below two statements to get page-3 h-tags ###
            ("\n List of all the headers in", y)
            (headlines.name + ' ' + headlines.text.strip())                                     

        joint_words3 = ' '.join(text3)
        separated_words3 = word_tokenize(joint_words3)
        stop_words = stopwords.words('english')

        separated_words3 = [''.join(c for c in s if c not in punctuation) for s in separated_words3]
        filtered_words3 = [word for word in separated_words3 if word not in stopwords.words('english')]

        c3 = Counter(filtered_words3)
        common3 = c3.most_common(15)
        records3.append((y, htag_listoflist3, common3))

### Print below statement to get page-3 common words ###
    ("\nMost Common words (pg3) are:\n", common3)




####### Translating all extracted data into a dataframe  ######

import pandas as pd

## Page 1 dataframe

df1 = pd.DataFrame(records, columns=['url', 'h-tags', 'Common Words'])
print(df1)

## Page 2 dataframe

df2 = pd.DataFrame(records2, columns=['url', 'h-tags', 'common words'])
print(df2)

## Page 3 dataframe

df3 = pd.DataFrame(records3, columns=['url', 'h-tags', 'common words'])
print(df3)                                 

    


              









                                      
       

    

        
    

   
        

    
    








    
   





