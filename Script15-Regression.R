#------------------------------------------------------------------------------
#BIOL4004 The Practising Scientist Workshop 5
#R Studio Regression (Bruce Riddoch, Saad Arif)
#March 2022
#Analysing the Blood Pressure data
#Using the BloodPressure.csv dataset that you created
#This script is easily adapted to other data sets you may generate during your studies
#-------------------------------------------------------------------------------
# Remember to work through this script file one line at a time

#housekeeping 
rm(list = ls())
setwd ("E:/Documents/DocumentsOct16/U15503 Study Skills/2019-20/Workshops/Workshop5")
#REMEMBER TO setwd FOR YOUR OWN DEVICE; and ensure that the date is stored there
getwd ()

#run the next command and then select BloodPressure.csv file
#you prepared earlier
blood.dat = read.csv("BloodPressure.csv")

#Check the data to confirm it is what you expected
names(blood.dat)  #returns names of the columns
head(blood.dat)  #returns first six rows, including labels

#First we need a scatterplot
par(mfrow = c(1,1))

#Now let's plot Blood Pressure against Age again
#note that the y-axis variable is to the left of the tilde(~) and x is on the right
plot (BP..mm.Hg.~Age..y., data=blood.dat)

#Now we want to run a regression analysis

summary(lm(BP..mm.Hg.~Age..y., data = blood.dat))

#Now we have some values we can return to our plot and fit a line
#Using intercept and slope from the output
plot (BP..mm.Hg.~Age..y., data=blood.dat)
abline(89.9846,0.8725)  #look at the output in the console to see where these came from

#We can put the regression results into an object "m"
#and make various plots of the data and residuals:

m<-lm(BP..mm.Hg.~Age..y., data = blood.dat)
#notice that this object has appeared in the Environment window (top right)
#First we need room to display all the plots
par(mfrow = c(2,2))
plot (m)
#Look at the top left plot onlu and slide 18 of the lecture

#Awesome!!

