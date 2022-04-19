#Loaded the data into R script.
load("C:/Users/Priyanshu/Downloads/killersandmotives.Rdata")

#Created my own sample.
x = 70
createsample(x)

#checked for data values in AgeFirstKill where value is 99999.
library("VIM")
aggr(mysample, numbers = TRUE, labels = names(mysample), prop = c(FALSE), cex.axis= .9)
t2 <- mysample[mysample$AgeFirstKill == 99999, ]
t2

mysample[1:10, ]
mean(mysample$AgeFirstKill)
mysample[1:10, ]
nrow(mysample)
ncol(mysample)

#Removed the rows where AgeFirstKill was equal to 99999.
for(i in 1:nrow(mysample)){
  if(mysample$AgeFirstKill[i] == 99999){
    mysample <- mysample[-i,]
  }
}
mysample
mean(mysample$AgeFirstKill) #804 observations until this point.

# Removed data where year born is greater than or equal to 1900.
mysample <- mysample[((mysample$YearBorn >= 1900)),]

mysample #785 observations until this point.

#Calculating the mean, max, min, and standard deviation for AgeFirstKill coulmn.
sd(mysample$AgeFirstKill)
max(mysample$AgeFirstKill)
min(mysample$AgeFirstKill)

#Visualization.
quantile(mysample$AgeFirstKill, type = 1)
boxplot(mysample$AgeFirstKill)

boxplot(mysample$AgeFirstKill ~ mysample$Motive, data = mysample)

boxplot(mysample$AgeFirstKill ~ mysample$Sex, data = mysample)

boxplot(mysample$AgeFirstKill ~ mysample$Race, data = mysample)


# Z holds the value for active years of killers before they were caught.
z <- c(mysample$AgeLastKill - mysample$AgeFirstKill)
mean(z)
max(z)
min(z)
z[z == 0]

mysample <- mysample[((mysample$AgeFirstKill < mysample$AgeLastKill)),] #558 observations until this point

#Mean, and variance for Z.
mean(mysample$AgeFirstKill)
mean(mysample$AgeLastKill)
mean(z)
var(mysample$AgeFirstKill)
var(mysample$AgeLastKill)
var(z)

#Visualization for AgeFirstKill histogram given a normal.
hist(mysample$AgeFirstKill, freq = FALSE, ylim = c(0, 0.07))

y <- seq(from = min(mysample$AgeFirstKill), to = max(mysample$AgeFirstKill), by = 0.1) 
     
lines(y, dnorm(y, mean = 28.45351, sd = 6.938063), lwd = 2, col = "darkgreen")

# Histogram for Active Years gives an exponential graph.
mean(z)
b <- 1/mean(z)
z <- z[z>=0]
hist(z, freq = FALSE, ylim = c(0, 0.30), right = FALSE)
m<- seq(from = min(z), to = max(z), by = 0.1) 
lines(sort(z), dexp(sort(z), rate = b), col ="darkgreen", lwd = 2)

install.packages("corrgram")
library("corrgram")
corrgram(mysample)
summary(mysample)

#Checking the data types that we have in our data.
str(mysample)

#Distributions for different motives according to Age of First Kill.
Motive_EnjoymentofPower <- mysample[mysample$Motive == "Enjoyment or power",  "AgeFirstKill"]
hist(Motive_EnjoymentofPower, freq = FALSE, ylim = c(0, 0.05))
low <- seq(from = min(Motive_EnjoymentofPower), to = max(Motive_EnjoymentofPower), by = 0.1)
lines(low, dnorm(low, mean = 30.0668, sd = 8.390664), lwd = 2, col = "darkgreen")
Motive_Convenience <- mysample[mysample$Motive == "Convenience (didn't want children/spouse)", "AgeFirstKill"]
hist(Motive_Convenience, freq = FALSE, ylim = c(0, 0.06))
tt <- seq(from = 20, to = 60, by = 0.1)
lines(tt, dnorm(tt, mean = 37.14286, sd = 11.03674), col = "darkgreen", lwd = 2)
Motive_Unknown <- mysample[mysample$Motive == "Unknown", "AgeFirstKill"]
hist(Motive_Unknown, freq = FALSE)
gg <- seq(from = min(Motive_Unknown), to = max(Motive_Unknown), by = 0.1)
lines(gg, dnorm(gg, mean = 29.5283, sd = 9.155922), lwd = 2, col = "darkgreen")
#ADD THE LINES SECTION IN HERE DURING THE LAB.

#Some Caluculations.
Motive_EnjoymentofPower <- na.omit(Motive_EnjoymentofPower)
Motive_EnjoymentofPower
mem <- mean(Motive_EnjoymentofPower)
mes <- sd(Motive_EnjoymentofPower)
Motive_Convenience <- na.omit(Motive_Convenience)
mcm <- mean(Motive_Convenience)
mcs <- sd(Motive_Convenience)
cb <- 1/mean(Motive_Convenience)
Motive_Unknown <- na.omit(Motive_Unknown)
mum <- mean(Motive_Unknown)
mus <- sd(Motive_Unknown)

#Checking the Cdf of respective age first kill according to different motives.
fn <- ecdf(Motive_EnjoymentofPower)
fn(27)
t1 <- sum(Motive_EnjoymentofPower <= 27)/length(Motive_EnjoymentofPower)
t1
fn1 <- ecdf(Motive_Convenience)
fn(27)
t2 <- sum(Motive_Convenience <=27)/length(Motive_Convenience)
t2
fn2 <- ecdf(Motive_Unknown)
fn(27)
t3 <- sum(Motive_Unknown <= 27)/length(Motive_Unknown)
t3

#Hypothesis Testing.
#Kolmogorov-Smirnov test.
ks.test(x = Motive_EnjoymentofPower, y = "pnorm", mean = mem, sd = mes)
ks.test(x = Motive_Convenience, y = "pnorm", mean = mcm, sd = mcs)
ks.test(x = Motive_Unknown, y = "pnorm", mean = mum, sd = mus)

#Some normal quantile plots.
sort(Motive_EnjoymentofPower)
sort(Motive_Convenience)
sort(Motive_Unknown)

#For enjoyment of power motive:
kl <- (sort(Motive_EnjoymentofPower) - mem)/mes
h1 <- length(Motive_EnjoymentofPower)
f1 <- (1:h1)
q1 <- qnorm(p = f1/(h1 + 1), mean = 0, sd = 1)
plot(q1,kl)
abline(a=0, b=1, col='red')
qqnorm(kl)
abline(a=0, b=1, col='red')

#For convenience motive:
jl <- (sort(Motive_Convenience) - mcm)/mcs
h2 <- length(Motive_Convenience)
f2 <- (1:h2)
q2 <- qnorm(p = f2/(h2 + 1), mean = 0, sd = 1)
plot(q2,jl)
abline(a=0, b=1, col='red')
qqnorm(jl)
abline(a=0, b=1, col='red')

#For Unknown Motives:
gl <- (sort(Motive_Unknown) - mum)/mus
h3 <- length(Motive_Unknown)
f3 <- (1:h3)
q3 <- qnorm(p = f3/(h3 + 1), mean = 0, sd = 1)
plot(q3,gl)
abline(a=0, b=1, col='red')
qqnorm(gl)
abline(a=0, b=1, col='red')

#Shapiro-Wilk Test:
shapiro.test(Motive_EnjoymentofPower)
shapiro.test(Motive_Convenience)
shapiro.test(Motive_Unknown)

