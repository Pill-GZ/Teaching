####################################################
## Taking HW 2 Question 3 as an example,          ##
## I'm demonstraing how loops can be avoided in R ##
## Runs a LOT faster than a for-loop version !    ##
####################################################

# read dataset
senmat <- read.table('senmatrix.txt', header = FALSE, sep = ',')

# calculate numeber of cosponsors by a bill
bill_cosponsors <- apply(senmat, 2, function(x){sum(x==2)})

# a matrix of bill sponsorship: [i,j]=1 if senator i sponsored bill j
sponsorship <- (senmat==1)

# total number of co-sponsors a senator gets
total_cosponsors <- sponsorship %*% bill_cosponsors

# total number of bills sponsored a senator
total_sposored <- rowSums(senmat==1)

# V is the ratio of number of co-sponsors to number of bills sponsored
V <- total_cosponsors / total_sposored

# take care of NaN's
V <- replace(V,is.na(V),0)

# read the file that contains names
senname <- read.table('senators.txt',header = FALSE,sep = ',',as.is = TRUE)

# Most influential
(senname[,1])[order(V,decreasing = T)][1:5]

# Least influential
(senname[,1])[order(V,decreasing = F)][1:5]
