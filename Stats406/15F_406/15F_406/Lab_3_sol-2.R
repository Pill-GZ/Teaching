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
# Plot with changing color
plot(UpdateTrack,
     type='b', lty=3, pch=25, col=rgb(seq(0, 1, length.out=nround), seq(1, 0, length.out=nround), 1),
     xlim=c(-1, 52), ylim=c(-0.2, 1.3),
     main=paste('Light bulb status update within ', nround, ' rounds', sep=''), sub='An on-off process illustration', xlab='Index', ylab='Status',
     cex.lab=1.5, cex.main=1.25, cex.sub=1.25, col.lab='darkgreen',
     axes=FALSE, # to be specified later
)
axis(1, at=c(1, 25, 50), cex.axis=1.5);
axis(2, at=c(0, 1), cex.axis=1.5);
box();


##### EXERCISES
# 1
# Run the following preparatory code
x_mat = (1:10) %*% t(rep(1, 10));
y_mat = t(x_mat);
z_mat = x_mat*y_mat;
# Question: draw a contour plot, in which axes do not display.
image(z_mat, axes=FALSE);


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
# Question: refine or modify the plot in the following perspectives:
# 1. Use symbolic markers for three curves rather than the current '1', '2' and '3'.
# 2. Unify the line types of all three curves to solid lines.
# 3. Change the colors Curves A, B and C to purple, cyan and blue respectively.
# 4. Add a legend at the top-left corner, which should present correct curve names, line types and line colors.
matplot(x=Year, y=Curves, type='b', lty=1, pch=1:3, col=c('purple', 'cyan', 'blue'));
legend('topleft', c('Curve A', 'Curve B', 'Curve C'), lty=1, pch=1:3, col=c('purple', 'cyan', 'blue'));

# 3
# We shall use the airquality data. You can run
?airquality
# To check its background, and to see which variables are available, run
names(airquality)
# Here, to simplify the exercise, remove all rows with missing values(should be careful if in real world data analysis!)
# Question 1: Remove all rows with missing values and store the result in `airquality.cleaned'
airquality.cleaned = airquality[apply(airquality, 1, function(x) !any(is.na(x))), ];
# Question 2: Then draw a side-by-side boxplot to obtain a very rough summary of ozone levels in different months. Add axes and main title using title(). You can do:
# first, plot(...) or boxplot(...) or ...
# then, title(...), to add title and axis labels
?title # check the documentation of title()
with(airquality.cleaned, boxplot(Ozone~as.factor(Month)));
title(main='Ozone levels at different months', xlab='Month', ylab='Ozone level(ppb)');
# Question 3: Compute the mean levels of Ozone, Solar.R, Wind and Temp in each month. You can write a for-loop here. Then plot each of them respectively versus time(month, from May to September), thus generating four plots. Set type='o' and write main titles. No need to further refine these plots. Put them in one single figure by splitting the screen. Practice saving your plots to a pdf file by coding(do not right-click-and-save).
# These commands may come handy
?dim
?unique
Months = unique(airquality.cleaned$Month);
MeanLevelsByMonth = array(0, c( length(Months), 4 ) );
for(month_index in 1:length(Months)){
  MeanLevelsByMonth[month_index, ] = apply(airquality.cleaned[airquality.cleaned$Month==Months[month_index], 1:4], 2, mean);
}
pdf('Exercise_3.pdf');
  par(mfrow=c(2, 2));
  plot(x=unique(airquality.cleaned$Month), y=MeanLevelsByMonth[, 1], type='o', main='Ozone by month');
  plot(x=unique(airquality.cleaned$Month), y=MeanLevelsByMonth[, 2], type='o', main='Solar.R by month');
  plot(x=unique(airquality.cleaned$Month), y=MeanLevelsByMonth[, 3], type='o', main='Wind by month');
  plot(x=unique(airquality.cleaned$Month), y=MeanLevelsByMonth[, 4], type='o', main='Temp by month');
dev.off();


