### QUestion 1 ####

### Reading the GPA data into R ###
gpa_data = read.csv("C:\\Users\\hxt210018\\Downloads\\6313_Prob\\HW\\gpa.csv")


### Drawing scatter plots ###
gpa = as.numeric(gpa_data$gpa)
act = as.numeric(gpa_data$act)

plot(gpa,act,main="Scatterplot of GPA and ACT",xlab = "GPA",ylab = "ACT")
abline(lm(act~gpa))

## Correlation calculation
cor(gpa,act)

##Import/attach the boot library ##
library(boot)

## Statistic function for correlation ##

covariance_npar = function(gpa_act_data,index) {
  agpa = gpa_act_data$gpa[index]
  gact = gpa_act_data$act[index]
  result = cor(agpa,gact)
  return (result)
}

## Execute boot function with statistical function ##
covariance_npar.boot = boot(gpa_data,covariance_npar, R = 999, sim = "ordinary",stype = "i")
covariance_npar.boot


## Point estimate of bootstrap value ##
mean(covariance_npar.boot$t)


##Calculating confidence interval using boot.ci ##
boot.ci(boot.out = covariance_npar.boot)


## Verifying the confidence intervals using the percentile bootstrap ##
sort(covariance_npar.boot$t)[c(25, 975)]





### Question 2 ###
## Reading the voltage data into R ##
voltage_data = read.csv("C:\\Users\\hxt210018\\Downloads\\6313_Prob\\HW\\VOLTAGE.csv")
voltage_data.remote = voltage_data$voltage[which(voltage_data$location == 0)]
voltage_data.local = voltage_data$voltage[which(voltage_data$location == 1)]

## Box plots to conduct the exploratory analysis ##
boxplot(voltage_data.local,voltage_data.remote, names = c("Local","Remote"),main = "Boxplot of voltage values at Local and Remote 
                                                          locations",range = 1.5)

summary(voltage_data.remote)
summary(voltage_data.local)

## Drawing QQ plots for the datasets ##

par(mfrow=c(1,2))
qqnorm(voltage_data.local,main= "Local")
qqline(voltage_data.local)
qqnorm(voltage_data.remote, main = "Remote")
qqline(voltage_data.remote)


## Calculating mean, variance, standard error and confidence intervals ##
var_l = var(voltage_data.local)
var_l

var_r = var(voltage_data.remote)
var_r


standard_error = sqrt(var_l/30 + var_r/30)
standard_error


difference_of_means = mean(voltage_data.remote) - mean(voltage_data.local)
CI = difference_of_means + c(-1,1) * qnorm(0.975) * standard_error
CI


## Calculating the confidence interval using the t test ##
t.test(voltage_data.remote,voltage_data.local,alternative = "two.sided",paired = FALSE, var.equal = FALSE, conf.level = 0.95)



### Question 3 ###

## Read the vapor data for the file into R ##
vapor = read.csv("C:\\Users\\hxt210018\\Downloads\\6313_Prob\\HW\\VAPOR.csv")

## Drawing the QQ plots ##
par(mfrow = c(1,2))
qqnorm(vapor$theoretical, main = "Theoretical")
qqline(vapor$theoretical)
qqnorm(vapor$experimental, main = "Experimental")
qqline(vapor$experimental)


## Drawing the boxplots and summaries ##
par(mfrow = c(1,1))
boxplot(vapor$theoretical,vapor$experimental,names = c("Theoretical","Experimental"),
        main = "Boxplot of Theoretical and Experimental readings")

diff = vapor$theoretical - vapor$experimental

boxplot(diff, main = "Boxplot of difference of Theoretical and Experimental readings")

summary(vapor$theoretical)

summary(vapor$experimental)

summary(diff)


## Calculating mean, standard error and confidence intervals ##

mean_d = mean(diff)
mean_d

sd_d = sd(diff)
sd_d

qt(0.975,15)

cI = mean_d + c(-1,1) * qt(0.975,15) * sd_d/sqrt(16)
cI

## Using t test calculating confidence intervals ##

t.test(vapor$theoretical, vapor$experimental, alternative = "two.sided", paired = TRUE, var.equal = FALSE, conf.level = 0.95)

















