setwd("/home/gaozheng/Teaching/Stats406/HW/HW3/")
list.files()

library(VIM)

sleepdata = cbind("ID"=1:dim(sleep)[1],sleep)
names(sleepdata)
new_63 = sleepdata[62,1:7]
new_63$ID = 63L
sleepdata1 = rbind(sleepdata[,c(1:7)],new_63)
sapply(sleepdata1, class)

sleepdata2 = sleepdata[,c(1,8:11)]

write.csv(sleepdata1,file = "sleep1.csv",na = "NULL",row.names = F)
write.csv(sleepdata2,file = "sleep2.csv",na = "NULL",row.names = F)

########## Students write this part #########

sleepdata1 <- read.csv("sleep1.csv",header = T,na.strings = "NULL")
sleepdata2 <- read.csv("sleep2.csv",header = T,na.strings = "NULL")

sapply(sleepdata1,class)
sapply(sleepdata2,class)

sleepdata <- merge(sleepdata1,sleepdata2)
sapply(sleepdata, class)

with(sleepdata[sleepdata$Danger!=4,],boxplot(Dream/Sleep~Danger))

with(sleepdata,boxplot(Dream/Sleep~Danger))

sleepdata <- sleepdata[order(sleepdata$Danger),]


############ HW3 #################

data <- read.csv("MERGED2009_10_PP.csv",header = T,sep = ',',na.strings = "NULL")
data09 <- data[,c("UNITID","INSTNM","CONTROL","COSTT4_A")]
sapply(data09,class)
class(data09$COSTT4_A)
summary(data09$CONTROL)

data <- read.csv("MERGED2014_15_PP.csv",header = T,sep = ',',na.strings = "NULL")
data14 <- data[,c("UNITID","INSTNM","CONTROL","COSTT4_A")]

data <- read.csv("MERGED2013_14_PP.csv",header = T,sep = ',',na.strings = "NULL")
data13 <- data[,c("UNITID","INSTNM","CONTROL","COSTT4_A")]


apply(data09,2,class)
anyNA(data09$COSTT4_A)
#is.na(data14$COSTT4_A)
anyNA(data14$COSTT4_A)
#is.na(data09$COSTT4_A)
#is.na(data09$COSTT4_A)
#which(data14$COSTT4_A=="NULL")

combined = merge(data09,data14,by=c("UNITID","CONTROL"))
boxplot(data14$COSTT4_A)

combined$ratio <- combined$COSTT4_A.x / combined$COSTT4_A.y
anyNA(combined$ratio)
head(combined)

# sum(with(combined,CONTROL.x != CONTROL.y))

par(mfrow=c(1,1))
with(combined,boxplot(COSTT4_A.y/COSTT4_A.x~CONTROL,outline=F,ylim=c(0.5,1.6)))

combined1314 = merge(data13,data14,by=c("UNITID","CONTROL"))
with(combined1314,boxplot(COSTT4_A.y/COSTT4_A.x~CONTROL,outline=F,ylim=c(0.5,1.6)))

