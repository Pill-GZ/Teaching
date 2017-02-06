##########
## Read data
Traffic <- read.table('flow-occ-table.txt', header=T, sep=",")
## Check the column names of the data
colnames(Traffic)
## Set new column names
colnames(Traffic) = c("O1", "F1", "O2", "F2", "O3", "F3")
## Check it again
colnames(Traffic)
## Initialize the new matrix to record the final result
newTraffic <- matrix(0, nrow(Traffic), 2)
## Read the rows one by one
for(i in 1:nrow(Traffic))
{
  ## Extract the i-th row
  traffic.i <- Traffic[i, ]
  ## Find the maximal value of the three
  maxflow <- max( traffic.i[c(2,4,6)] )
  ## Find which is the maximal value of the three
  idx_maxflow <- which.max(traffic.i[c(2,4,6)])
  ## Extract the Occ columns 1, 3, 5
  traffic.i.occ <- traffic.i[c(1,3,5)]
  ## Extract the Occ value correponding to the maximal flow
  maxocc <- as.numeric(traffic.i.occ[idx_maxflow])
  ## Record the resutl
  newTraffic[i,] <- c(maxflow, maxocc)
}
## Convert matrix to data frame
newTraffic <- as.data.frame(newTraffic) ## Difference between matrix and data
##frame? All entries in matrices are of the same type. Data frames let you
##refer to column by names.
## Set column names
colnames(newTraffic) = c("maxflow", "maxocc")
## Write the data frame into a text file
write.table(newTraffic, file="flow-occ-table_clean.txt", sep="\t", col.names=T)
##################
#Initialize the vector to store winnings
niter<-1000
winnings <- NULL
# Game loop
for (i in 1:niter)
{
  #initialize first winning, last flip result, and first flip result
  lastflip <- 0
  x <- (runif(1)<=.5)
  winning <- (x==1)-(x==0)
  while((lastflip!=1)|(x!=1))
  {
    # last flip becomes this flip
    lastflip <- x
    # a new flip
    x <- (runif(1)<.5)
    winning <- winning+(x==1)-(x==0)
  }
  winnings <- c(winnings,winning)
}
mean(winnings)
###############
Y <- read.table('flow-occ-table.txt', sep=",", header=T)
Y <- Y[1:30,]

# only a single input
v <- seq(0, 1, length=10)
plot(v)
# two inputs
v <- seq(0, 1, length=10)
u <- seq(3, 4, length=10)
plot(u,v)

plot(Y$Occ1, col=2, ylim=c(0, .04))
points(Y$Occ2, col=3)
points(Y$Occ3, col=4)

# type this before you make the plot to save it as a pdf
pdf("plot.pdf")
plot(Y$Occ1, col=2, ylim=c(0, .04))
points(Y$Occ2, col=3)
points(Y$Occ3, col=4)
plot(Y$Occ1, col=2, ylim=c(0, .04), pch=3) ## pch=3 makes + signs
points(Y$Occ2, col=3, pch=0) ## pch=0 makes squares
points(Y$Occ3, col=4, pch=16) ## pch=16 makes filled in circles
lines(Y$Occ1, col=2)
lines(Y$Occ2, col=3)
lines(Y$Occ3, col=4)
# The points correspond to "Occ1", "Occ2", and "Occ3"
# The colors we used were 2,3,4
# The plotting characters where 3, 0, 16
legend(25, .035, c("Occ1", "Occ2", "Occ3"), pch=c(3,0,16), col=c(2:4), lty=1)
# type this after youve made the plot
dev.off()

## Define the normal density function
normal_density_function <- function(x, mean, sd) 
{
  d <- 1 / sqrt(2*pi*sd^2) * exp(-(x - mean)^2 / (2*sd^2))
  return(d)	
}

## Generate a grid with 30 points between -3 and 3
x <- seq(-3, 3, length=30)
## Compute the corresponding densities
y1 <- normal_density_function(x=x, mean=1, sd=2)
## Compute the densities from R internal function dnorm()
y2 <- dnorm(x=x, mean=1, sd=2)
## Plot the result of function normal_density_function() with points
plot(x, y1, main='Density of normal distribution', xlab='x', ylab='Density', type='p')
## Plot the result of function normal_density_function() with curve
lines(x, y2, lty=1)

# f to calculate the 3rd smallest entry of a vector x
f <- function(x) sort(x)[3]
# Storage for the function output
OUT <- rep(0, 200)
# Using a loop
for(i in 1:200)
{
  OUT[i] <- f( A[i,] )
}

# Using apply
OUT <- apply(A, 1, f)


#Model a SIRS model
# to estimate the average proportion
p_recover = 0.5;
p_loose_immunity = 0.2;
P_ever = 0
## Initialize the population.
Q = numeric(1000)
Q[1:10] = 1
Q_ever = Q
p_transmit = 0.3

## Figure out which infected people recover and become immune.
infected = which(Q == 1)
for (j in infected) {
  if (runif(1) < p_recover) { Q[j] = 2 }
}
## Figure out which immune people lose their immunity.
immune = which(Q == 2)
for (j in immune) {
  if (runif(1) < p_loose_immunity) { Q[j] = 0 }
}

