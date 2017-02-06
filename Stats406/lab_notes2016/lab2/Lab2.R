# Lab2 code

#### Density function of normal distributions
normal_density_function <- function(x, mean, sd){
  d = 1 / sqrt(2*pi*sd^2) * exp(-(x - mean)^2 / (2*sd^2))
  return(d)	
}
# Generate a grid with 30 points between -3 and 3
x = seq(-3, 3, length=30)
# Compute the corresponding densities
y1 = normal_density_function(x=x, mean=0, sd=1)
# Compute the densities from R internal function dnorm()
y2 = dnorm(x=x, mean=0, sd=1)
# Plot the result of function normal_density_function() with points
plot(x, y1, main='Density of normal distribution', xlab='x', ylab='Density', type='p')
# Plot the result of function normal_density_function() with curve
lines(x, y2, lty=1, col="red")

#### Coin flipping example
m = 10
for(k in 1:m){
  # initialize count n and result B
  n = 0
  B = 0
  # stop at the first time that 1 occurs
  while(B != 1){
    n = n + 1
    # generate a fair coin flip
    B = rbinom(1,1,0.5)
  }
  print(n)
}

#### Another coin flipping example
m = 100
# save the times that A wins
count = 0
for(k in 1:m){
  # generates the first flip
  coin_old = rbinom(1,1,0.5)
  # generates the second flip
  coin = rbinom(1,1,0.5)
  # stops when two consecutive results appear
  while(coin_old != coin){ 
    # save the last flip 
    coin_old = coin 
    # generate a new flip
    coin = rbinom(1,1,0.5)
  }
  # check the results
  if(coin==1){
    print("A wins")
    count = count + 1
  }else{
    print("B wins")
  }
}
# print the proportion of times A wins
print(count/m)


#### Student performance data set
# read the data
student = read.table("student-mat.csv",header=T,sep=";")
n = nrow(student)
# create a new data frame and specify the column names
student_new = data.frame(matrix(0,nrow=n,ncol=2))
names(student_new) = c("avg","weightedAvg")

# use for loop
for(i in 1:n){
  tmp = as.numeric(student[i,c("G1","G2","G3")])
  student_new$avg[i] = mean(tmp)
  student_new$weightedAvg[i] = sum(tmp*c(0.25,0.25,0.5))
}

# use apply
student_new$avg = apply(student[,c("G1","G2","G3")],1,mean)
# define the weighted sum function
student_new$weightedAvg = apply(student[,c("G1","G2","G3")],1,function(x) {sum(x*c(0.25,0.25,0.5))})

# write the new data frame into csv file
write.table(student_new,file="student_new.csv",sep=",",col.names = T,row.names = F)



