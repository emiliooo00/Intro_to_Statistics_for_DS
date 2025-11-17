#Question 1 

#A company claim their fun-size packets of sweet contain an average of 29g of sweets.
# A statistics student buys a bag of the company's sweets. The bag contains 15 fun-size packets.
#The student weighs the contents of each packet, getting the following weights in grammes (g):
#27.3, 30.9, 25.9, 31.2, 29.7, 28.8, 29.4, 28.5, 28.9, 31.6, 27.8, 27.8, 28.6, 27.3, 27.6
# Is the company's claim defensible?

#We check with a hypothesis test

sweets<-c(27.3, 30.9, 25.9, 31.2, 29.7, 28.8, 29.4, 28.5, 28.9, 31.6, 27.8, 27.8, 28.6, 27.3, 27.6)

#Firstly, this is a small sample size (n=15<30), so we can only proceed with the hypothesis test if we believe the data is normally distributed

qqnorm(sweets)
qqline(sweets)
shapiro.test(sweets)

#No strong evidence normality doesn't hold, so can proceed (if we couldn't, would have to use a *non-parametric test*)

#Our null hypothesis H0 is that the population mean mu=29. What should our alternative hypothesis be?

#Two possible arguments. One is that we've not been told to test in a particular direction, so we should default to a two tailed test.
#It may be relevant to check whether the bags are overall lighter than claimed, *or* heavier than claimed
#Second argument: the bags being lighter than claimed is the bigger problem, as it implies false advertising. There is less of an issue if the bags are heavier than claimed.

#We will take the two-tailed approach here, with the alternative hypothesis H1: mu != 29.

#We will also need a significance level, which in deference to tradition I shall set at 5%.

#To find the test statistic, we need the sample mean x bar, and the sample standard deviation s:

xbar<-mean(sweets)
s<-sd(sweets)

#We can now find the test statistic, which equals (xbar - mu)/(s/sqrt(n)). Under H0, mu=29, which is what we use.

Tstat<-(xbar-29)/(s/sqrt(15))
Tstat

#Note that the negative value of Tstat tells us that the sample mean is less than 29, so there is at least some evidence the bags are lighter on average than claimed.

#We now have two options: calculate the p-value, or directly compare the value of Tstat against the critical value.

#We'll start with the critical value. We have n<30, so we have to use a t-distribution. 15 data points means using t_14. 

#We find the critical value t_14,0.025 (sometimes expressed as t_0.025,14) using the qt function.

#Good practice to draw a quick sketch of the t-distribution and the areas that would result in rejecting H0 - these areas are what we call the *critical region*.

qt(0.975,14)

#So a statistically significant result here requires Tstat be either above 2.145, or below -2.145. Neither are the case, so H0 is *not* rejected.

#Now let's find the p-value for the test. Remember that qt and pt do essentially opposite jobs

pt(-0.5987803,14)

#The probability of a realisation from t_14 being less than the test statistic is 0.2794. 
#The probability of a realisation from t_14 being more than the test statistic times -1 (giving a positive value) must also be 0.2794.
#The probability of a realisation from t_14 lying outside either value is therefore 0.2794*2.

0.2794*2

#This is therefore the p-value of the test. 
#Note that we already knew the p value must be above 0.05, because the test did not result in us rejecting H0.

#We now know how to perform a hypothesis test step by step in R. There is a much, much faster way, so long as we're OK using the t-distribution to find critical values and p-values

t.test(sweets,alternative="two.sided",mu=29,conf.level=0.95)

#Question 2

#a)Finding realisations from a standard normal distribution is something we've done before.

onereal<-rnorm(1,0,1)

#Note that every time you run that line of code through R, the value of onereal will (almost certainly) be different, because R is doing its best to represent realisations from a random variable (and its best is pretty good, all things considered). In particular, you will get a value of onereal that lies outside the range [-1.96,1.96] approximately 5% of the time.

 #b) To find the sample mean required, we just find 100 realisations using rnorm, then sum them and divide by 100.
 
 sampmean<-sum(rnorm(100,0,1))/100
 
 #Again, each time you enter that code, you should expect to get a different value of sampmean. Note that the Central Limit Theorem tells us that, while each realisation has a variance of 1, the sample mean has a sampling variance of 1 over t 100, which is 0.1. This means you will get a value of sampmean that lies outside the range [-1.96,1.96] with the same probability that an individual realisation of a standard normal lies outside the range [-19.6,19.6], which is almost impossible (I tried this 100,000,000 times in R when writing this worksheet, and not one of the sample means lay outside [-1.96,1.96]).
 

#Question  3

#We've seen already how to get realisations from various distributions using e.g. rnorm, runif, rbinom, etc.
#One thing we haven't covered is how to get realisations from a distribution we define ourselves. 

#We'll do this now (for the discrete case)

#Example: An environmental agency wants to track the number of 
#polar bear cubs in Churchill, Canada.
#Polar bears have more cubs when the surrounding environment is favourable,
#so checking cub numbers is a good way to check on the bearpopulations as a whole.
#Female bears are shy when they have cubs, though, and can turn aggressive quickly, so it can be hard to find them.
#A 2013 survey only found thirty-five female bears with cubs. A 2023 survey only found thirty-three.
#The environmental agency wants to check whether the mean number of polar bear cubs has changed between 2013 and 2023.

#We're going to sample our own data to represent cub numbers, and then perform hypothesis tests on those sample values.
#This might seem silly - if we sample the data, don't we already know the mean values?
#Sure, but this is a good way to look at the difference between what is actually true, and what we can tell from our data.
#We'll start with the following. Let's say the 2013 and 2023 have *identical* distributions for the number of cubs:

cubnum<-c(0,1,2,3,4,5)
prob<-c(0.3,0.4,0.2,0.05,0.035, 0.015)

#Here cubnum is the vector of cub number, and prob is the probability mass function for the cubs
#We can generate samples for the 2013 and 2023 surveys as follows:

survey2013<-sample(cubnum,35,replace=T,prob)
survey2023<-sample(cubnum,33,replace=T,prob)

#If you try to enter those two lines into R, you will (almost certainly) get different vectors survey2013 and survey2023 to me. Again, this is because the sample function is trying its best to behave in a random manner. As a result, everything in the code below is very likely to give you different answers to mine, and - as we'll see - I get different answers from the same RStudio session each time I enter this code.

#We're telling R to sample from the cubnum elements, 35 or 33 times, with replacement, using the probability vector prob.
#Using replace=T here is very important. If you use replace=F, each element in cubnum can only be used once, which means you'll run out ofvalues and R will sulk!

wrongapproach<-sample(cubnum,35,replace=F,prob)

#We'll now take these two samples, and run a two-sample t-test at the 5% significance level to see whether the population means are different.
#Now, we KNOW the population means are the same, so our null hypothesis will be true. But each time I run this workshop there is a five percent 
#probability I'll reject the null hypothesis (which is what we call a Type I error).
 

#Our null hypothesis is that mu_2013 = mu_2023. Our alternative hypothesis will be that mu_2013 != mu_2023, as we haven't been given a reason to focus on just one tail.
#Before we can go further, we need to decide which test approach to take. In this example, we not only know the variances are equal, but we could directly compute them
#using the prob vector. We'll pretend we don't know this, though, for the sake of doing this example.

#Our first step should be to check whether we believe the variances are equal. For that we need Levene's test.

?leveneTest

#Disaster! R doesn't have the Levene test!

#At least, it doesn't have it *yet*. We need to load a package which contains it, such as "car".

#We do this by going to "Packages", selecting a mirror (it never seems to make a difference which one you pick), and selecting "car". 
#This will download the package, which we then add into our library.

library(car)

?leveneTest

#If we want to compare two vectors against each other, this is easily done, once our data is in a data frame

dat2013<-rbind(survey2013,rep(2013,35))
dat2023<-rbind(survey2023,rep(2023,33))
bearsdat<-rbind(t(dat2013),t(dat2023))
bears<-data.frame(bearsdat)
names(bears)<-c("Cubs","Year")

#Note my use of the rbind, which join row vectors together (along with matrices of compatible column numbers), and of t() (the transpose function).

#We now use Levene's Test

leveneTest(Cubs~Year,data=bears)

#Another error! R isn't happy; it thinks the year values are numbers to use in calculations. Easily fixed:

bears$Year<-as.factor(bears$Year)
leveneTest(Cubs~Year,data=bears)

#What can we conclude from this p-value?

#We now perform one of two two-sample t-tests. If we concluded the two unknown variances were equal, we can use:
  
t.test(survey2013, survey2023, var.equal = TRUE)

#If we weren't able to conclude the two unknown variances were equal, we use:

t.test(survey2013, survey2023, var.equal = FALSE)

#What does the test we have chosen tell us about whether the population mean in 2013 equals that in 2023?