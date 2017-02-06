## data frames
V = c('A','A','C','B','B');
X = 1:5;
Y = rnorm(n=5,mean=0,sd=1);
dt1 = data.frame(X,Y,V);
names(dt1) = c('meas1','meas2','meas3')

## Working with airquality
dt2 = airquality
names(dt2)
dim(dt2)
mean(dt2$Ozone)
mean(dt2$Ozone,na.rm = T)
ind = complete.cases(dt2);
dt = dt2[ind,];
mean(dt$Ozone);
plot(dt$Ozone,type='l',col='blue')
###########
## For loops
A = matrix(c(1,8,2,2,0,0,9,1,6,-2,1,-3,2,-2,3,10),ncol=4)
mean_A = numeric(nrow(A))
for( i in 1:nrow(A)){
	mean_A[i] = mean(A[i,])
}
##Apply
x = rnorm(100,mean=-5,sd=1);
y = rnorm(100,mean=5,sd=1);
X = cbind(x,y)
apply(X=X,MARGIN=2,FUN=mean)

###Timing
n=5000; p=5000;
A = matrix(rnorm(n*p),ncol=p);
time = system.time(rowSums(A));
time_1 = time[3]
r_sum = numeric(n)
time = system.time(for( i in 1:n ) r_sum[i]=sum(A[i,])));
time_2 = time[3];
time = system.time(apply(X=A,MARGIN=1,FUN=sum));
time_3 = time[3]
print(c(time_1,time_2,time_3))

###(l/s)apply
lst = list(x1=rnorm(10),x2=rnorm(1000))
lapply(lst,mean)
sapply(lst,mean)

###while loop
x= 0 ; j=1;
while(x<=5){
	x = x + 1/j;
	j = j+1;
}











