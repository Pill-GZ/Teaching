setwd("../Downloads/")
list.files()
loandata <- read.csv("LoanStats3a.csv", header = T, skip = 1, nrows = 39786)
summary(loandata)
summary(loandata$loan_status)
sampleIndex <- sample(39786, 39786*0.2, replace = F)
summary(loandata$loan_status[sampleIndex])
summary(loandata$loan_status[-sampleIndex])

training <- loandata[-sampleIndex,]
testing <- loandata[sampleIndex,]


write.csv(training, file = "LoanStatsTraining.csv", qmethod='escape', quote=TRUE)
isBadLoan <- 1 - ((training$loan_status=="Current")|(training$loan_status=="Fully Paid"))
table(training$loan_status, isBadLoan)
write.csv(cbind(training[,1:26],isBadLoan), file = "LoanStatsTraining.csv", qmethod='escape', quote=TRUE, row.names = F)

isBadLoan.testing <-  1 - ((testing$loan_status=="Current")|(testing$loan_status=="Fully Paid"))
sum(isBadLoan.testing)
write.csv(cbind(id = testing$id,isBadLoan = isBadLoan.testing), file = "LoanStatsSolution.csv", row.names = F)
sample.solution <- predict(model, newdata = testing, type="response")
write.csv(cbind(id = testing$id,isBadLoan = sample.solution), file = "LoanStatsSampleSolution.csv", row.names = F)

testing$loan_status <- NA
write.csv(testing[,1:26], file = "LoanStatsTesting.csv", row.names = F)

table(loandata$loan_status, loandata$sub_grade)
table(isBadLoan, training$sub_grade)

table(loandata$dti_joint)


