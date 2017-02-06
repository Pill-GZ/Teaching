############# Lab 1 Sept. 17, 2015 ####################

### Variables and Functions ###

x <- 6
y <- x
print(y)
x <- 8
y <- x ^ 2 - 2 * x + 1
print(y)

y = function(n, x)
{
  return (x ^ n)
}
print(y(2, 7))

### Vectors and Matrices ###

## Exercise ##
#1
v1 = seq(from = 1.0, to = 3.0, length = 5)
print(v1)

#2
x1 = 1.0 ; x2 = seq(from = 1.5, to = 3.0, by = 0.5)
v2 = c(x1, x2);
print(v2)

##
n <- seq(0, 5, by = 1) ## this is the same as n <- c(0:5) or n <- 0:5
print(n)
x <- seq(1, 10, length = 10)
print(x)
## compute the values of 2 raised to the powers from 0 to 5;
y1 = y(n, 2)
## compute the squares of 1 to 10;
y2 = y(2, x)
## quote the function $exp()$ in R that computes $e^x$ for x from 1 to 10;
y3 = exp(x)
print(y1)
print(y2)
print(y3)

##
v <- c(4, 5, 10, 19, 23, 4, 16)
## print the 6th element of v
print(v[6])
## print the 1st, 3rd, and 4th elements of v
index <- c(1, 3, 4)
print(v[index])
index <- c(2,5,6,7)
v[-index]

## Exercise ##
vec = function(m)
  seq(-m, m, by = 1) ^ 2
print(vec(5))

## Assign all the loading values as a vector to a matrix, specify dimensions with nrow and ncol, note
## that R locate the values column by column.
A <- matrix(c(1:9), nrow = 3, ncol = 3)
print(A)

## Create A by concatenating vectors
v1 <- seq(1, 3, by = 1) ## same as c(1:3)
v2 <- seq(4, 6, by = 1) ## same as c(4:6)
v3 <- seq(7, 9, by = 1) ## same as c(7:9)
A <- cbind(v1, v2, v3)
colnames(A) = NULL

# get the second column of A
A[,2]
# get the third row of A
A[3,]
# get the entry in third row and 2nd column of A
A[3,2]
# get the entries in first and third rows, and 2nd and third column of A
A[c(1,3),c(2,3)]

B = matrix(rnorm(25) , 5 , 5)
u = B[,1] ## First column of B
indx = which.max(u) ## OR indx = which(u == max(u))
v = B[indx,]
print(indx)
print(v)

prop = mean(v > 0) ## v>0 is a vector of logicals.

t(A)

AA = t(A) %*% A
print(AA)

print(det(AA))

### Data Frames ###

N <- c(1, 2, 3)
C <- c("a", "b", "c")
L <- c(TRUE, FALSE, TRUE)
dt0 <- data.frame(N, C, L)       # dt0 is a data frame
class(dt0) 		# type of the object `dt0'
names(dt0) 	# attribute (column vector) names of `dt0'
print(dt0)

dt0mat = data.matrix(dt0) 	# convert a data frame to a matrix object
print(dt0mat) 		# notice the change

# import the data from your current working directory
airq = read.table('airquality.txt', header = T, stringsAsFactors = F)

# check the type of the object imported
class(airq)

# check the attribute (each column vector) names
names(airq)

# have a look of the data frame
head(airq)

# suppose we are only interested in complete cases without `NA' records
cpl.cases = complete.cases(airq)
aircompl = airq[cpl.cases,]    # create a sub data frame accordingly

# we only look at data from the month 7 here, assume it's July
July.id = which(aircompl[,'Month'] == 7)
July.air = aircompl[July.id,]	   # create a further sub data frame accordingly

# find the day in July with the highest Ozone recording
ozmax.id = which.max(July.air[,'Ozone'])
day.ozmax = July.air[ozmax.id, 'Day']
print(day.ozmax)


### Simulating Random variables ###

# u contains n random variables uniformly distributed on (a,b)
n <- 10
a <- 1
b <- 5
u <- runif(n, min = a, max = b)
print(u)

# u contains n normally distributed variables with mean mu and variance sigmasq
n <- 10
mu <- 0
sigmasq <- 2
u <- rnorm(n, mean = mu, sd = sqrt(sigmasq))
print(u)

# u contains n exponentially distributed variables with parameter lambda
n <- 10
lambda <- 5
u <- rexp(n, rate = lambda)
# u contains n binomial random variables with parameters k, p
n <- 10
k <- 20
p <- .5
u <- rbinom(n, size = k, prob = p)
print(u)


## Calculate P(X > 3)
n <- 10
p <- .35
#### exact probability
# using the built-in binomial CDF function
1 - pbinom(3, size = n, prob = p)

# calculating it ourselves
prob <- 0
k = 4:10
prob <- choose(n, k) * (p ^ k) * ((1 - p) ^ (n - k))
sum(prob)

#### calculate by sampling
# nreps: number of simulated i.i.d. variables-- nreps larger --> more accurate
nreps = 10000
X = rbinom(nreps, size = n, prob = p)
mean(X > 3)

############## END of Lab1 ###################