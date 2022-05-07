######## n = 1,2,3,5,10,30 ; theta = 1,5,50,500 #########

### Function to calculate mean squared error (MSE) for MLE and MOM ###

Estimatorfunction = function(n, theta) {
  sample = runif(n,min = 0, max = theta);
  mme = 2 * mean(sample);
  mle = max(sample);
  return (c(mme,mle));
}

MSEfunction = function(n , theta) {
  sampleestimators = replicate(nsim,Estimatorfunction(n,theta));
  error1 = (sampleestimators - theta)^2;
  return(rowMeans(error1));
}

## For 1000 simulations and for n = 10, theta = 50 ##

MSEfunction = function(n , theta) {
  sampleestimators = replicate(1000,Estimatorfunction(n,theta));
  error1 = (sampleestimators - theta)^2;
  return(rowMeans(error1));
}
theta_hat = MSEfunction(10, 50);
theta_hat

## For various combinations of n and theta calculating mse ##
n = c(1,2,3,5,10,30)
theta = c(1,5,50,100)
nsim = 1000

n_len = length(n)
theta_len = length(theta)
#set.seed(1000)
mse_mme_theta1 = matrix(NA,nrow = n_len, ncol = theta_len)
mse_mle_theta2 = matrix(NA, nrow = n_len, ncol = theta_len)

for(i in 1:n_len) {
  for(j in 1:theta_len) {
    val = MSEfunction(n[i],theta[j])
    mse_mme_theta1[i,j] = val[1]
    mse_mle_theta2[i,j] = val[2]
  }
}

mse_mme_theta1

mse_mle_theta2

## Plotting the results graphically ###

par(mfrow = c(2,2))
for(i in 1:theta_len){
  plot(n,mse_mme_theta1[,i],type="l",lty=1,ylim = c(0,max(mse_mme_theta1[,i],mse_mle_theta2[,i])),main=paste0("theta=",theta[i]),
       ylab = "MSE",col='red')
  lines(n,mse_mle_theta2[,i],lty = 2,col = 'blue')
  legend("topright",legend = c("MLE","MOM"),col = c('blue','red'),text.col = c('black','black'),lty = 1, pch = 1,
         inset = 0.01,ncol = 1, cex = 0.6,bty = 'n')
}

legend("topright",legend = c("MLE","MOM"),col = c('red','blue'),text.col = c('black','black'),lty = 1, pch = 1,
       inset = 0.01,ncol = 1, cex = 0.6,bty = 'n')



## Plotting the results graphically with different values of n ###

par(mfrow = c(3,3))
for(i in 1:n_len){
  plot(theta,mse_mme_theta1[i,],type="l",lty=1,ylim = c(0,max(mse_mme_theta1[i,],mse_mle_theta2[i,])),main=paste0("n=",n[i]),
       ylab = "MSE",col='red')
  lines(theta,mse_mle_theta2[i,],lty = 2,col = 'blue')
  legend("topleft",legend = c("MLE","MOM"),col = c('blue','red'),text.col = c('black','black'),lty = 1, pch = 1,
         inset = 0.01,ncol = 1, cex = 0.6,bty = 'n')
}

#### Question 2###########

## defining the function obtained in question 2(a) ##
negative_log_function = function(theta, value){
  result = length(value) * log(theta) - (theta + 1) * sum(log(value));
  return (-result);
}

## executing the optim function to minimize the negative log function ##

given_data = c(21.72,14.65,50.42,28.78,11.23)

mle_estimator = optim(par = 0.3, fn = negative_log_function, method = "L-BFGS-B",lower = (10 ^ (-10)), hessian = TRUE,value =given_data)

mle_estimator$par


#### To get the approximate standard error and 95% confidence interval for theta #####

standar_error = sqrt( 1/mle_estimator$hessian )
standar_error

confidence_interval = mle_estimator$par + c(-1,1)*standar_error*qnorm(0.975)
confidence_interval
