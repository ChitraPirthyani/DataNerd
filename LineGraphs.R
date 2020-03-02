
library(ggplot2)

######################################################
# Now, let's try a built-in datset.  The EuStockMarkets
# has four columns for various stock markets and the 
# value is the closing.  We don't have date information
# but we will just use the number of the record.  The
# Melt function collapses oall of these fields into a 
# single "Market" field and a "Value" field.  That 
# way we can more easily plot by market
######################################################

head(EuStockMarkets)

# we want to add fields, so make a copy
ds = as.data.frame(EuStockMarkets)
ds$day = seq(nrow(ds))    # Add a sequential count of the records to indicate the "day"
                          # No, it is not totally correct, but in the absence of this information ...
library(reshape)

head(ds)

# Melt collapses columns into one field like the "pivot" feature in Tableau
# The id parameter here tells what fields to keep (i.e. not "melt")
EuStock = melt(ds, id=c("day"))
names(EuStock)[2] = "Index"
head(EuStock)

######################################################
# Now, plot the stock values with the color based on 
# the stock "index" ... i.e. which country.  We exercise
# a few properties here to give a feel for the range
# of geom_line's capabilities
######################################################

ggplot(EuStock, aes(day, value, color=Index)) + geom_line()
ggplot(EuStock, aes(day, value, color=Index)) + geom_line(size=3)
ggplot(EuStock, aes(day, value, color=Index)) + geom_line(size=1.5, alpha=.5)
yes
yes
######################################################
# There are several ways to load excel files in R
# Two of them require you to have external software
# installed.
#
#   library(gdata), requires "Perl" to be installed
#   library(xlsx), requires Java to be installed 
#                  with a proper JAVA_HOME environment
#                  variable
#
# Because of this, we will just use excel to "SaveAs"
# a "csv" file any Excel spreadsheet that we want.
#####################################################

priceIndex = read.table("ConsumerPriceIndex.csv", sep=",", header = TRUE)
head(priceIndex)

#####################################################
# Now, we need to convert the date.  R is a little 
# better at dealing with date names than Tableau here
# as it can use "abbrevated" month names very similar
# to what was in the column heads of the original 
# excel file.  The only thing I have done is to remove
# the "." at the end of Jan., Feb., etc. and have 
# abbreviated Jun and Jul
#
# The first thing we need to do is to "pivot" or "reshape"
# the data so that the month is a column and there is
# a single "value" column.
# 
# We can do this with the "melt" function in the 
# "reshape" package.  In the melt function, you just 
# need to specify which properties you wish to KEEP
# note that this is kind of the opposite to tableau
#####################################################

library(reshape)
cIndex = melt(priceIndex, c("Year", "Avg.", "Dec.1", "Avg"))
head(cIndex)

#####################################################
# Now, the only problem is that we have the month 
# field named "variable".  We can rename it by setting
# an element of the "names" array for the dataset.
# Notice that "variable" is the 5th column in the data
#####################################################

names(cIndex)
names(cIndex)[5] = "Month"
head(cIndex)

# Let's get rid of a few extra fields, the (Avg. and Dec.1 fields)
cIndex = cIndex[-c(2, 3)]
head(cIndex)

#####################################################
# Now we are ready to convert.  We do this with the 
# "as.Date" function. This function takes strings and
# converts the string to a "Date" type which is better 
# for graphing.  The only thing it needs is the "format"
# of the date string
#
# So, first, we need to make a single date "string" 
# out of the "Year" and "Month" fields.  We do this 
# with the "paste" function.  This function "pastes"
# strings together with a specified separator character.
# We will use a "-" character, so that our dates will
# look like 
#
#   Jan-1915
#
# Then the as.Date function will take a second argument
# that is the date format.  The date format tells which
# parts of the date will be in what order, and tells
# what characters will be separating them (dahes or 
# slashes, for example)
#
#   Jan-1915 ---->  Year-MonthAbbrev-Day ----> %Y-%b-%d
#
# %b stands for the abbreviated month name and #Y 
# says that it will be followed a dash and then
# the full four digit year. For a full list of the 
# options, check R's documentation on as.Date
#####################################################

# Notice that we are putting in "01" for the day here since we don't have one
cIndex$DateString = paste(cIndex$Year, cIndex$Month, "01", sep="-")
head(cIndex$DateString)

cIndex$Date = as.Date(cIndex$DateString, "%Y-%b-%d")
head(cIndex$Date)

#####################################################
# Now we can use this date to plot the data.  I've
# also added a theme modifier here to change the 
# style of the graph to a more traditional "black and 
# white" style.  The white background offsets the 
# curve better.
#####################################################

ggplot(cIndex, aes(Date, value)) + geom_line() + theme_bw()

#####################################################
# Now, let's compare trhee years of the data by pulling 
# out 1990, 2000 and 2010.  To do this we use the 
# subset function.  It takes a datset as the first
# parameter, and a condition as the second.  For 
# those of you who are not as familiar with the 
# syntax here, the "==" tests for equality and the
# | indicates an "or".  
#####################################################

# Select records where (Year IS 1990) OR (Year IS 2000) 
cIndexThreeYears = subset(cIndex, Year == 1990 | Year == 2000 | Year == 2010)

library(lubridate)  # contains a function to extract a month from a date
                    # Unfortunately, it is just a number, but it gives 
                    # us a readable graph
ggplot(cIndexThreeYears, aes(month(Date), value, color=factor(Year))) + 
              geom_line() + theme_bw() + 
              scale_x_continuous(limits=c(1,12), breaks=1:12)

