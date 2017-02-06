####################################
# Lab 03
####################################

##### 1. Plotting

### 1.1 
rm(list=ls());
set.seed(2015);
# Recap: plot()
a_vec = seq(from=0, to=3, by=0.1);
b_vec = a_vec + rnorm(length(a_vec));
plot(b_vec~a_vec);

### 1.2
# Recap: hist()
# continued from the previous example...
hist(b_vec);

### 1.3
# Recap: boxplot();
rm(list=ls());
set.seed(2015);
# 3 groups of data, each group contains 20 random numbers
groupsize = 20; ngroup = 3;
a_vec = rnorm(ngroup*groupsize); a_vec = a_vec + seq(from=0, to=5, length.out=length(a_vec));
b_vec = as.factor( rep(letters[1:ngroup], rep(groupsize, ngroup)) );
# side-by-side group-wise boxplots
test_data_frame = data.frame(a_vec, b_vec);
boxplot( with(test_data_frame, a_vec ~ b_vec) );



### 2.1
# par(mfrow=...)
# Open a new screen.
x11(); # Not applicable under commandline
# Split screen
par(mfrow = c(2, 1));
# Plot 1
plot(rnorm(1000));
# Plot 2
qqnorm(runif(1000));
#
# When we're done...
dev.off();

### 2.2
# layout(splitmatrix)
x11();
# Prepare the split matrix
SplitMat = array(0, c(5, 5));
SplitMat[1:3, 1:3] = 1;
SplitMat[4:5, 1:3] = 2; SplitMat[1:3, 4:5] = 3;
SplitMat[4:5, 4:5] = 4;
# Split the screen
layout(SplitMat, 2, 2);
layout.show(4); # just to confirm, not required
# Plots
set.seed(2015);
plot(rnorm(1000));
qqnorm(runif(1000));
qqnorm(rt(1000, df=3));
hist(rnorm(1000));
#
# When we're done:
dev.off(); # or click to close the window



### 3.1
# Prepare an integer multiply chart within 100
x_mat = (1:10) %*% t(rep(1, 10));
y_mat = t(x_mat);
z_mat = x_mat*y_mat;
# Contour plot
contour(z_mat);

### 3.2
# Continuing the last example
require(lattice);
print(levelplot(z_mat));

### 3.3
# Continuing the last example
image(z_mat);

### 4.1
# Illustrate the update track of one light bulb
# Prepare data
set.seed(20151001);
p = 0.10; q = 0.05;
nround = 50;
UpdateTrack = integer(nround);
UpdateTrack[1] = 1;
for(i in 2:nround){
  if(UpdateTrack[i-1]==0){
    UpdateTrack[i] = rbinom(1, 1, p);
  } else{
    UpdateTrack[i] = rbinom(1, 1, 1-q);
  }
}
# Plot
plot(UpdateTrack,
    type='o', lty=3, pch=25, col='blue',
    xlim=c(-1, 52), ylim=c(-0.2, 1.3),
    main=paste('Light bulb status update within ', nround, ' rounds', sep=''), sub='An on-off process illustration', xlab='Index', ylab='Status',
    cex.lab=1.5, cex.main=1.25, cex.sub=1.25, col.lab='darkgreen',
    axes=FALSE, # to be specified later
    )
axis(1, at=c(1, 25, 50), cex.axis=1.5);
axis(2, at=c(0, 1), cex.axis=1.5);
box();



### 5.1
# Continuing the last example
Points_x = c(13,15,17,30,31,39);
Points_y = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8);
points(Points_x, Points_y, type='o', col='magenta');

### 5.2
# Continuing the last example
abline(0.2, 0.5/nround, lty=2, col='red');

### 5.3
# Continuing the last example
legend('topright', c('LightBulb 1'), lty=3, pch=25, col=c('blue'));



##### EXERCISES
# 1
# Run the following preparatory code
x_mat = (1:10) %*% t(rep(1, 10));
y_mat = t(x_mat);
z_mat = x_mat*y_mat;
# Question 1: draw a contour plot, in which axes do not display.

# 2
# Run the following preparatory code
# Fancied yearly income of three households in a small neighborhood of rich people in million dollars
Curve_A = c(0.30, 0.50, 0.70, 0.80, 0.90);
Curve_B = c(0.25, 0.45, 0.65, 0.75, 0.75);
Curve_C = c(0.35, 0.40, 0.60, 0.90, 1.00);
Year    = c(1970, 1980, 1990, 2000, 2010);
Curves  = cbind(Curve_A, Curve_B, Curve_C);
# Now I draw a raw plot containing multiple curves as follows
matplot(x=Year, y=Curves, type='b');
# Question 2: refine or modify the plot in the following perspectives:
# a. Use symbolic markers for three curves rather than the current '1', '2' and '3'.
# b. Unify the line types of all three curves to solid lines.
# c. Change the colors Curves A, B and C to purple, cyan and blue respectively.
# d. Add a legend at the top-left corner, which should present correct curve names, line types and line colors.

# 3
# We shall use the airquality data. You can run
?airquality
# To check its background, and to see which variables are available, run
names(airquality)
# Here, to simplify the exercise, remove all rows with missing values(should be careful if in real world data analysis!)
# Question 3.1: Remove all rows with missing values and store the result in `airquality.cleaned'
# Question 3.2: Then draw a side-by-side boxplot to obtain a very rough summary of ozone levels in different months. Add axes and main title using title(). You can do:
# first, plot(...) or boxplot(...) or ...
# then, title(...), to add title and axis labels
?title # check the documentation of title()
# Question 3.3: Compute the mean levels of Ozone, Solar.R, Wind and Temp in each month. You can write a for-loop here. Then plot each of them respectively versus time(month, from May to September), thus generating four plots. Set type='o' and write main titles. No need to further refine these plots. Put them in one single figure by splitting the screen. Practice saving your plots to a pdf file by coding(do not right-click-and-save).
# These commands may come handy
?dim
?unique



##### 2. LLNo's and CLT

### 2.1
### Illustrations of SLLNo's and WLLNo's
set.seed(2015);
nsamples = 250; samplesize = 20000;
# Generate data
BigRVMatrix = matrix(runif(nsamples*samplesize, min=-1, max=1), c(nsamples, samplesize));
# Compute sample means at different sample sizes
samplesizevector = seq(from=nsamples, to=samplesize, by=nsamples);
SampleMeans = array(0, c(nsamples, length(samplesizevector)));
for(i in 1:length(samplesizevector)){
  SampleMeans[, i] = apply(BigRVMatrix[, 1:samplesizevector[i]], 1, mean);
}
# SLLNo's
matplot(x=samplesizevector, y=t(SampleMeans), type='l', lty=1, col=1);
# WLLNo's
WLLN = function(SampleMeans, epsilon){
  Deviations = abs(SampleMeans);
  largedeviationproportion = as.vector( apply(Deviations, 2, function(y) mean(y>epsilon) ) );
  return(largedeviationproportion);
}
plot(x=samplesizevector, y=WLLN(SampleMeans, 0.005), type='o');



### 2.2
### Illustration of CLT
# For completeness here we copy part of the code from ### 2.1
set.seed(2015);
nsamples = 250; samplesize = 20000;
samplesizevector = c( round(0.03*samplesize), round(0.1*samplesize), round(0.3*samplesize), samplesize );

# Generate data: Unif(0,1) distribution
BigRVMatrix_unif = matrix(runif(nsamples*samplesize, min=-1, max=1), c(nsamples, samplesize));
#BigRVMatrix_unif = sign(BigRVMatrix_unif) * sqrt(sqrt(abs(BigRVMatrix_unif)));
# Compute sample means at different sample sizes
SampleMeans_unif = array(0, c(nsamples, length(samplesizevector)));
for(i in 1:length(samplesizevector)){
  SampleMeans_unif[, i] = apply(BigRVMatrix_unif[, 1:samplesizevector[i]], 1, mean);
}

# Generate data: N(0,1) distribution
BigRVMatrix_normal = matrix(rnorm(nsamples*samplesize), c(nsamples, samplesize));
# Compute sample means at different sample sizes
SampleMeans_normal = array(0, c(nsamples, length(samplesizevector)));
for(i in 1:length(samplesizevector)){
  SampleMeans_normal[, i] = apply(BigRVMatrix_normal[, 1:samplesizevector[i]], 1, mean);
}

# CLT for both
pdf('CLT.pdf');
# page 1
par(mfrow=c(2, 2));
hist(SampleMeans_unif[, 1]*sqrt(12), xlim=c(-0.1, 0.1));
hist(SampleMeans_unif[, 2]*sqrt(12), xlim=c(-0.1, 0.1));
hist(SampleMeans_normal[, 1], xlim=c(-0.1, 0.1));
hist(SampleMeans_normal[, 2], xlim=c(-0.1, 0.1));
# page 2
par(mfrow=c(2, 2));
hist(SampleMeans_unif[, 3]*sqrt(12), xlim=c(-0.1, 0.1));
hist(SampleMeans_unif[, 4]*sqrt(12), xlim=c(-0.1, 0.1));
hist(SampleMeans_normal[, 3], xlim=c(-0.1, 0.1));
hist(SampleMeans_normal[, 4], xlim=c(-0.1, 0.1));
dev.off();

