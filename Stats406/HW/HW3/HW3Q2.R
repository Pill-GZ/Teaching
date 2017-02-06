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
