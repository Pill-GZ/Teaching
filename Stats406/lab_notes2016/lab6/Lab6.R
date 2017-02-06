#### Vectors
v = c(3,1,5,9,7)
print(v)
length(v)
# access elements by []
v[1]
v[1:3]
v[-1]
# use logic values to select subset
v[v>4]
# get the index satisfying certain conditions
which(v>4)
# maximum of a vector
max(v)
# index that maximizes the vector
which.max(v)
# average of a vector
mean(v)
# calculation on logical vectors
sum(v>4)
# sort increasingly
sort(v) 
# sort decreasingly
sort(v, decreasing = T)
# order of elements increasingly
order(v)
# order of elements decreasingly
order(v,decreasing = T)

#### Lists
list1 = list(c(2,5,3),21.3,"hello")
print(list1)
# use [[]] to subset lists
list1[[1]]

list2 = list(c(1,2,3,4),c(5,6,7),c(8,9))
print(list2)
# use lapply and sapply to run a function on every element of the list
lapply(list2,sum)
sapply(list2,sum)

#### Matrices
A = matrix(1:6,nrow=3,ncol=2)
# In R, the default is by column
print(A)
B = matrix(1:6,nrow=3,ncol=2,byrow=T)
print(B)
# subset of a matrix
A[1,2]
A[2,]
A[,1]

# elementwise operations
A+B
A*B

# matrix multiplication
A%*%t(B)

# apply a function to every row/column
apply(A,1,max)
apply(A,2,max)

#### Dataframes
student = read.table("student-mat.csv",header=T,sep=";")
# subset of a dataframe
student[1,3]
student$age[1]
student[1,]
# select subset satisfying certain conditions
tmp = student[student$age==18,]
# operations
sum(student$age==18)
which(student$age>18)
# average of three grades G1, G2, G3
avg_G = apply(student[,c("G1","G2","G3")],1,mean)

#### For loops
s = 0
for(i in 1:100){
  s = s + i
}
print(s)

# another way without using for loop
sum(1:100)

#### While loops
s = 0
i = 0
while(s<=200){
  i = i + 1
  s = s + i
}
K = i - 1
print(K)

#### Functions
f <- function(n){
  return(sum(1:n))
}
f(100)


