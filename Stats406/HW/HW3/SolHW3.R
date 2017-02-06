#########
##Q1 college data
dt_2012 =read.csv('MERGED2009_10_PP.csv',na.strings=c('NULL'), stringsAsFactors = F)
dt_2013 =read.csv('MERGED2014_15_PP.csv',na.strings=c('NULL'), stringsAsFactors = F)
selected= (complete.cases(dt_2012$COSTT4_A));
dt_12 = dt_2012[selected, c('UNITID','OPEID','INSTNM', 'CITY', 'STABBR', 'CONTROL', 'UGDS','COSTT4_A')]
selected= (complete.cases(dt_2013$COSTT4_A));
dt_13 = dt_2013[selected, c('UNITID','OPEID','INSTNM', 'CITY', 'STABBR', 'CONTROL', 'UGDS','COSTT4_A')]

college = unique(dt_12$UNITID);
L=length(college)
dataset = data.frame(matrix(NA,nrow=1,ncol=7))
names(dataset) = c('UNITID','INSTNM','CONTROL','STABBR','UGDS','COSTT4_A','rate_cost')

ind=1;
for (ii in 1:L){
  dt_find_12 = dt_12[dt_12$UNITID==college[ii],]
  dt_find_13 = dt_13[dt_13$UNITID==college[ii],]
  if (nrow(dt_find_13) != 0){
    dataset[ind,1:6] =c(dt_find_12[,c('UNITID','INSTNM','CONTROL','STABBR','UGDS','COSTT4_A')])
    rate = (dt_find_13$COSTT4_A-dt_find_12$COSTT4_A)/dt_find_12$COSTT4_A;
    dataset[ind,7] = rate
  	ind = ind+1;
  }
  print(ii)
}
dataset=dataset[abs(dataset$rate_cost)<2,]
dt1 = dataset$rate_cost[dataset$CONTROL==1]
dt2 = dataset$rate_cost[dataset$CONTROL==2]
dt3 = dataset$rate_cost[dataset$CONTROL==3]
par(mfrow=c(1,3))
boxplot(dt1,col='blue',ylim=c(-0.5,2)); 
boxplot(dt2,col='blue',ylim=c(-0.5,2)); 
boxplot(dt3,col='blue',ylim=c(-0.5,2)); 



#######################
###Q2 complaints data
dt = read.delim('consumer_complaints.txt',header=T,stringsAsFactors=F)
year_Rg = 12:16;
 
##Chase
bank = 'JPMorgan Chase & Co.';
ind = which(dt$Company==bank);
dt_chase = dt[ind,];
lst = strsplit(dt_chase$date_received,"/");
series_chase = 0;
count = 1;
for(year in year_Rg){
	for (month in 1:12){
		ind_13 = sapply(lst,FUN = function(x,m,y)(x[1]==m)&(x[3]==year),y=year,m=month)		
		timely = sum(dt_chase[ind_13,16]=='Yes');
		series_chase[count] = timely/sum(ind_13)
		count=count + 1;
		print(count)
	}
}

##Bank of America

bank = 'Bank of America';
ind = which(dt$Company==bank);
dt_boa = dt[ind,];
lst = strsplit(dt_boa$date_received,"/");
series_boa = 0;
count = 1;
for(year in year_Rg){
	for (month in 1:12){
		ind_13 = sapply(lst,FUN = function(x,m,y)(x[1]==m)&(x[3]==year),y=year,m=month)		
		timely = sum(dt_boa[ind_13,16]=='Yes');
		series_boa[count] = timely/sum(ind_13)
		count=count + 1;
		print(count)
	}
}

##Wells Fargo
bank = 'Wells Fargo & Company';
ind = which(dt$Company==bank);
dt_fargo = dt[ind,];
lst = strsplit(dt_fargo$date_received,"/");
series_fargo = 0;
count = 1;
for(year in year_Rg){
	for (month in 1:12){
		ind_13 = sapply(lst,FUN = function(x,m,y)(x[1]==m)&(x[3]==year),y=year,m=month)
		timely = sum(dt_fargo[ind_13,16]=='Yes');
		series_fargo[count] = timely/sum(ind_13)
		count=count + 1;
		print(count)
	}
}


##Citibank
bank = 'Citibank';
ind = which(dt$Company==bank);
dt_citi = dt[ind,];
lst = strsplit(dt_citi$date_received,"/");
series_citi = 0;
count = 1;
for(year in year_Rg){
	for (month in 1:12){
		ind_13 = sapply(lst,FUN = function(x,m,y)(x[1]==m)&(x[3]==year),y=year,m=month)
		timely = sum(dt_citi[ind_13,16]=='Yes');
		series_citi[count] = timely/sum(ind_13)
		count=count + 1;
		print(count)
	}
}


plot(series_chase,type='l',col='blue',lty=1,xlim=c(1,58),ylim=c(0.8,1))
par(new=T)
plot(series_boa,type='l',col='red',lty=2,xlim=c(1,58),ylim=c(0.8,1)) 
par(new=T)
plot(series_fargo,type='l',col='black',lty=3,xlim=c(1,58),ylim=c(0.8,1)) 
par(new=T)
plot(series_citi,type='l',col='green',lty=4,xlim=c(1,58),ylim=c(0.8,1)) 



##Advanced solution proposed by the GSIs
#### Reading the data ####
# setwd("/home/gaozheng/Teaching/Stats406/HW/HW3/")
data <- read.delim("consumer_complaints.txt",stringsAsFactors = F)

# extract only the interested companies
four_companies <- data[data$Company == "JPMorgan Chase & Co." |
                         data$Company == "Bank of America" |
                         data$Company == "Wells Fargo & Company" |
                         data$Company == "Citibank",
                       c("date_received","Company","timely_response")]

# convert the dates to 'Date' objects with 'as.Date'
four_companies$date_received <-
  as.Date(four_companies$date_received,"%m/%d/%y")
# so that you can extract the years and months easily
year <- format(four_companies$date_received,'%y')
month <- format(four_companies$date_received,'%m')
# alternatively you can use strsplit

# convert the 'timely_response' into binary
four_companies$timely_response <-
  four_companies$timely_response == "Yes"

# attach year and month to the dataset
four_companies_with_year_month <- cbind(year,month,four_companies)

# use 'aggregate' to calculate the mean response rate by category
timely_response_rate <- with(
  four_companies_with_year_month,
  aggregate(timely_response ~ year + month + Company, FUN = mean)
)

# order the dataframe by 1. company 2. year 3. month
timely_response_rate <-
  timely_response_rate[with(timely_response_rate,order(Company,year,month)),]

# unstack the dataframe by company
timely_response_rate_by_company <-
  unstack(timely_response_rate,timely_response ~ Company)

# plot the reponse rates over time, leave out the x-axis
matplot(
  y = timely_response_rate_by_company, ylim = c(0.6,1.1),
  ylab = "timely response rate",type = 'b',xaxt = 'n',xlab = "Year and Month"
)
# we want to label x-axis with year
axis(1, at = 2 + (-1:5) * 12, labels = as.character(2011:2017))

# add legend for readability
legend(
  "bottomright", unique(timely_response_rate$Company),
  pch = as.character(1:4),col = 1:4
)

# if you wish to label x-axis with year/month, do this:
# year_month <- unique(with(timely_response_rate,paste("20",year,"/",month,sep = "")))
# axis(1, at=1:length(year_month), labels=year_month)

