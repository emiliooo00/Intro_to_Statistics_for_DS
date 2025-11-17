#An introduction to residual diagnostics

#We've seen in Video 6.5 how to check whether our four assumptions hold or not
#Two can be done with residual plots. A third can be sort-of done with them, though checking autocorrelation with a scatter diagram is more effective. 
#The fourth assumption is checkable with a QQ-plot.
#It would also be useful to have a way to check for leverage.

#We can do 80% of these tasks using the plot function, asking R to plot not points on a graph, but the entirety of a linear model

#I'll demonstrate this using the Orange tree data

Orange
names(Orange)

#This data plots the age (days) and trunk circumference (mm) of five orange trees.
#We will disregard the fact there are five different trees for today.

#Let's do a bit of exploratory data analysis (EDA). What does the relationship between age and circumference look like?

plot(Orange$age,Orange$circumference)
cor(Orange$age,Orange$circumference)

#It's strongly positive. Perhaps not surprising - trees get wider around the trunk as they get older - at least up to a certain point.

#We might decide we're interested in age in years, though, and the diameter rather than the circumference.

years<-Orange$age/365
diameter<-Orange$circumference/(pi)

#What does that do to our EDA?

plot(years,diameter)
cor(years,diameter)

#We get a similar scatter diagram, and exactly the same correlation. This tracks with what was said in the videos - a linear transformation of either or both variables does not change the correlation.

#Exercise - try changing the time variable from age in years, to how long until a tree reaches 5 years of age.   What does making that change do to the correlation?


#We'll use age as a predictor variable, with circumference as the response variable

orangemod<-lm(circumference~age,data=Orange)

#Note the syntax here: response goes first
#What is the simple linear model we have generated?

orangemod

#Not much information there! Good to know b_0 and b_1, though.
#We're estimating each day of life adds about 0.1mm (100 micrometres) to a tree's circumference
                  
summary(orangemod)

#That's better! There's so much info there it'll take us this week and next week to cover it all!
#Important things to note now: R^2=0.8345. Over 83% of variance of circumference explained by variance in age.

#More on this in the worksheet - what I want to focus on are "residual diagnostics".

plot(orangemod)

#Plot 1: Residual vs fitted. Clear pattern? If so, linearity broken
#Not seeing a pattern here. Looks like a right-opening fan, but we'll come back to that

#Plot 2: QQ plot for residuals. Don't look normally distributed? Then the assumption of normally distributed errors doesn't hold.

#Plot 3: Scale-location. One thing I didn't discuss in videos - even when errors are normally distributed, residuals may not be.
#Reasons for this are theoretically complicated - hence why I didn't discuss them.
#We can make the residuals look more like the errors should be through a standardisation process.
#R does this in Plot 3. It also finds the postive square root, which helps us look for heteroscedasticity.
#Now, rather than looking for a left/right opening fan, we look for an increase or decrease in general height of residuals.
#We have this here - note that we also had a right-opening fan in Plot 1. Many statisticians just use Plot 1, ignoring Plot 3.

#Plot 4. Cook's distance. A measure of influence. A very simple rule of thumb (and there are more than one for Cook's distance)
#is that any outliers with a value of more than 0.5 should be checked to see whether something has gone wrong.

#Conclusion: Assumptions 1 and 4 hold. 2 does not. 

#But what about 3? Need the residual values

orangemod$residuals

#Need to check these for autocorrelation

or<-orangemod$residuals

plot(or[1:34],or[2:35])

#Doesn't look great!

cor(or[1:34],or[2:35])

#EEK!

#So, Assumptions 1 and 4 are fine. 2 and 3 are not. What do we do?

#Assumption 3 issue: could try randomising values.

a<-sample(seq(1,35,by=1),replace=FALSE,prob=rep(1/35,35))

orangemodrandom<-lm(circumference[a]~age[a],data=Orange)
plot(orangemodrandom)

ora<-orangemodrandom$residuals
plot(ora[1:34],ora[2:35])
cor(ora[1:34],ora[2:35])
summary(orangemodrandom)$r.squared

#Looks like things have improved! But can we check whether that is the case with a statistical test?

#Yes! Can use the Durbin-Watson test in the lmtest package

library(lmtest)
dwtest(orangemod)
dwtest(orangemodrandom)

#A DW value of 2 is what we expect if there is no autocorrelation. Values below 1 or above 3 suggest a real problem.




#Assumption 2: Can we improve by transforming predictor variable?
#Suggested three approaches in Videos

orangemodlog<-lm(log(circumference)~age,data=Orange)
orangemodsqrt<-lm(sqrt(circumference)~age,data=Orange)
orangemodcbrt<-lm((circumference)^(1/3)~age,data=Orange)

summary(orangemod)$r.squared
summary(orangemodlog)$r.squared
summary(orangemodsqrt)$r.squared
summary(orangemodcbrt)$r.squared

#Not much change. What do plots look like:

plot(orangemod)
plot(orangemodlog)
plot(orangemodsqrt)
plot(orangemodcbrt)

#What do you think?
