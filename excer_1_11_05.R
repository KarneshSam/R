#Variation in the data is typically described by the variance (typically denoted σ2) or 
#its square root, the
#standard deviation (√σ2 = σ)
#standard errors (SE = SD/√n)
#standard deviation should not be given with the ± sign.
#coefficient of variation (CV ), defined as
#CV = σ/μ
x = rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
#las for the labeling of the x and y axis;
#main for the title;
hist(x, las=1, col='blue',main="")

#standard error;
#(
#seed will help us fix the same order;
#for everytime the rnorm produce different oders;
set.seed(1)
x=rnorm(10,10,2)
x
mean(x) #10.26
se_x = sqrt(var(x)/length(x))
se_x #0.49
#take the values in different oders
sample(x,replace = T)
mean(sample(x,replace = T)) #10.15; 10.44

#replace 'T' for having repetations;
#weight we can have to say which number have high prob
#sample(x, size = 3, replace = TRUE, prob = weights)
#)

#talks about the value;
#in what range the value lies for 95% interval;
quantile(x, c(0.025, 0.975))
qnorm(c(0.025, 0.975))


#for references;
x=c(2,3,5,7)
x
sd(log(x))
log_x=c()
for (i in x) {
  log_x=c(log_x,log(i))
}
print(log_x)
sd(log_x)
CV_x=sd(x)/mean(x)
CV_x
#Question 2:
cv = function(x) {
  sd(x) / mean(x)
}

SD = function(x){
  log_x=c()
  for (i in x) {
    log_x=c(log_x,log(i))
  }
  sd(log_x)
}

set.seed(1)
x = rnorm(50, 10, 2)
Cv_x=c()
sd_x=c()
for(i in 1:100){
  sample = sample(x, replace=TRUE)
  Cv_x[i]=cv(sample)
  sd_x[i]=SD(sample)
}
Cv_x 
sd_x

plot(Cv_x,sd_x,xlab='CV(x)',ylab='SD(log[x])',pch=1)+abline(coef = c(0,1))
length(Cv_x)
length(sd_x)











