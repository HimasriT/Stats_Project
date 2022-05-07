#### Question 1 ####

### Reading the body temperature and heart rate data from the csv data ###

bodytemp_hrate = read.csv("C:\\Users\\hxt210018\\Downloads\\6313_Prob\\HW\\bodytemp-heartrate.csv");

## Separating the male and female data ##

male_data = subset(bodytemp_hrate,bodytemp_hrate$gender == 1)
female_data = subset(bodytemp_hrate, bodytemp_hrate$gender == 2)

## Getting the summary for the male_data and female data ##

summary(male_data$body_temperature)
summary(female_data$body_temperature)

## Drawing the boxplots ##

boxplot(male_data$body_temperature,female_data$body_temperature, main = "Boxplots of body temperatures", names = c('Male','Female'), 
        ylab = "Body Temperature")

## Drawing the QQ plots for the body temperatures ##

par(mfrow = c(1,2))

qqnorm(male_data$body_temperature, main = 'QQ Plot for Male Body Temperature')
qqline(male_data$body_temperature)

qqnorm(female_data$body_temperature, main = 'QQ Plot for Female Body Temperature')
qqline(female_data$body_temperature)

### Calculating confidence intervals using the t-test for the body temperture ###
t.test(male_data$body_temperature, female_data$body_temperature,alternative = 'two.sided',var.equal = FALSE)


#### Question 1 part (b) ####

## Getting the summary for the male_data and female data heart_rate ##

summary(male_data$heart_rate)
summary(female_data$heart_rate)

## Drawing the boxplots ##

boxplot(male_data$heart_rate,female_data$heart_rate, main = "Boxplots of Heart rate", names = c('Male','Female'), 
        ylab = "Heart rate")

## Drawing the QQ plots for the Heart rate ##

par(mfrow = c(1,2))

qqnorm(male_data$heart_rate, main = 'QQ Plot for Male Heart rate')
qqline(male_data$heart_rate)

qqnorm(female_data$heart_rate, main = 'QQ Plot for Female Heart rate')
qqline(female_data$heart_rate)

### Calculating confidence intervals using the t-test for the heart rate  ###
t.test(male_data$heart_rate, female_data$heart_rate,alternative = 'two.sided',var.equal = FALSE)

## Question 1 Part (c) ##

##Finding the correlation between the body temperature and heart rate values irrespective of gender ##

par(mfrow=c(1,1))
cor(bodytemp_hrate$body_temperature, bodytemp_hrate$heart_rate)

plot(bodytemp_hrate$heart_rate, bodytemp_hrate$body_temperature, pch = 1, main = 'Scatter plot for all data')
abline(lm(bodytemp_hrate$body_temperature ~ bodytemp_hrate$heart_rate))
       
linear_model = lm(bodytemp_hrate$body_temperature ~ bodytemp_hrate$heart_rate)

print(linear_model)
summary(linear_model)

## Finding the correlation between the body temperature and heart rate values for male and female ##

cor(male_data$body_temperature,male_data$heart_rate)

cor(female_data$body_temperature, female_data$heart_rate)

## Drawing the scatter plots for the body temperatures and heart rate values for male and female ##

par(mfrow = c(1,2))

plot(male_data$heart_rate, male_data$body_temperature, pch = 1, main = 'Scatter plot for male')
abline(lm(male_data$body_temperature ~ male_data$heart_rate))

linear_model = lm(male_data$body_temperature ~ male_data$heart_rate)

print(linear_model)
summary(linear_model)

plot(female_data$heart_rate, female_data$body_temperature, pch = 1, main = 'Scatter plot for Female')
abline(lm(female_data$body_temperature ~ female_data$heart_rate))

linear_model = lm(female_data$body_temperature ~ female_data$heart_rate)

print(linear_model)
summary(linear_model)


### Question 2 (a) ###

## Creating function to check if true mean exists within the confidence interval ##
checkz_func = function(n,lambda) {
  u = rexp(n,lambda)
  
  lower_bound = mean(u) - qnorm(0.975) * sd(u) / sqrt(n)
  upper_bound = mean(u) + qnorm(0.975) * sd(u) / sqrt(n)
  
  sm = 1/lambda
  
  if(upper_bound > sm  & lower_bound < sm){
    return (1)
  }
  else
  {
    return (0)
  }
}

## calling the function 5000 times and checking the probability ##
zproportion = function(n, lambda) {
  values = replicate(5000, checkz_func(n, lambda))
  no_ones = values[which(values == 1)]
  return (length(no_ones)/5000)
}

## checking for n = 10 and lambda = 0.1 ##
zproportion(10, 0.1)


## Creating a function to return the mean ##
myFunc_mean = function(n, lambda){
  u = rexp(n, lambda)
  return (mean(u))
}

## Calls the myFunc_mean 1000 times and forms the confidence intervals and returns whether the true mean is present in the constructed interval ##


checkb_func = function(n, lambda){
  u = rexp(n, lambda)
  sm = 1/lambda
  lambda_temp = 1/mean(u)
  
  values = replicate(1000,myFunc_mean(n,lambda_temp))
  bounds = sort(values)[c(25, 975)]
  
  if(bounds[2] > sm & bounds[1] < sm){
    return (1)
  }
  else
  {
    return (0)
  }
}

## Creating a function for the parametric bootstrap sample and calls the checkb_func 5000 times to calculate the coverage probabilities ##

bproportion = function(n, lambda){
  values = replicate(5000, checkb_func(n, lambda))
  no_ones = values[which(values == 1)]
  return (length(no_ones)/5000)
}

## Checking the bproportion for n = 10 and lambda = 0.1 ##

bproportion(10, 0.1)

  

## Question 2 (b) ###

### For various values of n and lambda calculating the zproportion and bproportion ###

n_values = c(5,10,30,100)
lambda_values = c(0.01, 0.1, 1, 10)

n_len = length(n_values)
lambda_len = length(lambda_values)

zMatrix = matrix(NA, nrow = n_len, ncol = lambda_len)
bMatrix = matrix(NA, nrow = n_len, ncol = lambda_len)

for(i in 1:n_len){
  for(j in 1:lambda_len){
    zMatrix[i,j]= zproportion(n_values[i],lambda_values[j])
    bMatrix[i,j] = bproportion(n_values[i],lambda_values[j])
  }
}
  
zMatrix

bMatrix


## Plotting the results graphically ##

par(mfrow = c(2,2))
for(i in 1:lambda_len){
  plot(n_values,zMatrix[,i],type = 'b',lty = 1,xlab = 'n', ylab = 'Proportions ',col ='red',xlim=c(1,100),ylim=c(0.7,1),main =paste0("lambda =",lambda_values[i]))
  lines(n_values, bMatrix[,i],lty = 2,col = 'blue',type = 'b')
  legend("bottomright",legend = c("zInterval","bootStrapInterval"),col = c('red','blue'),text.col = c('black','black'),lty = 1, pch =
           1, inset = 0.01,ncol = 1, cex = 0.6,bty = 'n')
}

par(mfrow = c(2,2))
for(i in 1:n_len){
  plot(lambda_values,zMatrix[i,],type = 'b',lty = 1,xlab = 'lambda', ylab = 'Proportions ',col ='red',xlim=c(0.01,10),ylim=c(0.8,1),main =paste0("N=",n_values[i]))
  lines(lambda_values, bMatrix[i,],lty = 2,col = 'blue',type = 'b')
  legend("topright",legend = c("zInterval","bootStrapInterval"),col = c('red','blue'),text.col = c('black','black'),lty = 1, pch =
           1, inset = 0.01,ncol = 1, cex = 0.6,bty = 'n')
}
