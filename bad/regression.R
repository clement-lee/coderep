# Two examples of regression
rm(list = ls())
getwd()
setwd ("E:/Documents/DocumentsOct16/U15503 Study Skills/2019-20/Workshops/Workshop5")
#REMEMBER TO setwd FOR YOUR OWN DEVICE; and ensure that the date is stored there
getwd ()
install.packages("dplyr")
library(dplyr)


#################################
##### Blood pressure data #######
#################################

blood.dat = read.csv("BloodPressure.csv")
blood.dat

#Check the data to confirm it is what you expected
names(blood.dat)  #returns names of the columns
head(blood.dat)  #returns first six rows, including labels
nrow(blood.dat) # retursn number of rows

#First we need a scatterplot
par(mfrow = c(1,1))

#Now let's plot Blood Pressure against Age again
#note that the y-axis variable is to the left of the tilde(~) and x is on the right
plot (BP..mm.Hg.~Age..y., data=blood.dat)
dev.copy2pdf(file = "data.pdf") # save pdf for report writing

#Now we want to run a regression analysis

summary(lm(BP..mm.Hg.~Age..y., blood.dat))


png(filename = "datawithline.png") # save png for presentation slides; have to run png command first
#Now we have some values we can return to our plot and fit a line
#Using intercept and slope from the output
plot (BP..mm.Hg.~Age..y., data=blood.dat)
abline(89.9846,0.8725)  #look at the output in the console to see where these came from
dev.off() # close the device for png

#We can put the regression results into an object "m"
#and make various plots of the data and residuals:

m<-lm(BP..mm.Hg.~Age..y., data = blood.dat)
#notice that this object has appeared in the Environment window (top right)
#First we need room to display all the plots
par(mfrow = c(2,2))
plot (m)
#Look at the top left plot onlu and slide 18 of the lecture

#Awesome!!

ls() # look at what we have defined

#################################
##### breast cancer data ########
#################################
# The data is from Royston and Altman (2013): External validation of a Cox prognostic model: principles and methods, in BMC Medical Research Methodology

library(survival)
rotterdam # will print a lot of lines
?rotterdam # help page

# look at data
head(rotterdam);tail(rotterdam)
nrow(rotterdam)
names(rotterdam)

# glm
m = glm(death~pid+year+age+meno+size+grade+nodes+pgr+er+hormon+chemo+rtime+recur+dtime, data=rotterdam)
m =glm(death~. ,data = rotterdam) # the same
m = glm(death~., data = rotterdam, family = "binomial") # why different?
summary(m) # some variables should not be there
plot(m) #DONT KNOW HOW TO INTERPRET THEM

#rename and change
rotterdam$PatientIdentifier <- rotterdam$pid
rotterdam$pid=NULL
rotterdam$meno = as.factor(rotterdam$meno)
rotterdam$hormon= as.factor(rotterdam$hormon)
rotterdam$grade =as.factor(rotterdam$grade)
rotterdam$chemo =as.factor(rotterdam$chemo)
rotterdam$recur = as.factor(rotterdam$recur)
rotterdam$death = as.factor(rotterdam$death)

## contingency table
table(rotterdam$meno, rotterdam$death) # potential effect
table(rotterdam$hormon, rotterdam$death) # unclear
table(rotterdam$recur, rotterdam$death) # clear effect

# glm again
glm0 <- glm(death ~ age + meno + size + grade + nodes + pgr + er + hormon + chemo + rtime + recur + dtime, "binomial", rotterdam)
summary(glm0) # age,size20-50,size>50,nodes,er,hormon,rtime,recur,dtime significant
step(glm0) # choose the variables that matter
step(glm0, direction = "both") # same result
glm1 = glm(death~age + size + nodes + er + hormon + rtime + recur + dtime, rotterdam, family = binomial("logit"))
summary(glm1)
AIC(glm1) # 2121.666
AIC(glm0) # 2127.538

# plots for report and presentation
plot(glm1)
plot(glm1, 1) # residualts vs fitted
plot(glm0, 1) # to comparse
plot(glm1, 2)# normal qq
plot(glm1, 3)#scale-location
plot(glm1, 4)#Cook's distance
plot(glm1, 5)# residuals vs leverage
plot(glm1, 6)#cook's dist vs leverage

# different link function?

save.image(file = "lmandglm.Rdata") # save workspace
remove(list = ls())
