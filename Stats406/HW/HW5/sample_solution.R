#### adapted from Zixin's solution

#### Question 1 ####

# install.packages("RSQLite", dependencies = T)
library("RSQLite")
drv = dbDriver("SQLite")
conn = dbConnect(drv, "baseball.db")

sqltext = "SELECT (2016 - birthYear) AS Age, AVG(BB*1.0/SO) AS avg_wtso_ratio
FROM Pitching INNER JOIN Master
ON Pitching.playerID = Master.playerID
WHERE yearID = 2004 AND BB*1.0/SO !='NA'
GROUP BY Age"
dt = dbGetQuery(conn, sqltext)

plot(
  x = dt$Age, y = dt$avg_wtso_ratio,
  xlab = "Ages", ylab = "Average Walk To Strike Out Ratio",
  type = 'b', main = "Average Walk to Strike out Ratio"
)

#### Question2 ####

# install.packages('XML', dep = T)

library('XML')
xmlGetNeededInfo = function(root) {
  lst_ch = xmlChildren(root[[1]])
  V1 = xmlValue(xmlChildren(lst_ch$Institution)$Name)
  V2 = xmlValue(lst_ch$AwardAmount)
  return(c(V1,V2))
}

Files = system('ls ./2012', intern = T)
L = length(Files)
df = as.data.frame(matrix(NA, ncol = 2, nrow = L))
names(df) = c('University', 'Amount')

for (jj in 1:L) {
  filename = paste('./2012/', Files[jj], sep = "")
  doc = xmlTreeParse(filename)
  rt = xmlRoot(doc)
  val = xmlGetNeededInfo(rt)
  df[jj,] = val
}

# extraxt all `universities'
pattern = "[Uu]niversity"
df_universities = df[grep(pattern, dt$University),]
# sum by universities
df_universities = aggregate(as.numeric(Amount) ~ University, df_universities, sum)
# order by total amount
df_universities = df_universities[order(dt_new[[2]], decreasing = T),]
# look at top 10
head(df_universities, 10)

#### Question3 ####

doc = xmlTreeParse('AviationData.xml')
root = xmlRoot(doc)
lst_ch = xmlChildren(root[[1]])
rows = length(root[[1]])
dt = as.data.frame(matrix(NA, ncol = 4, nrow = rows))
names(dt) = c("EventDate","Location","Country","TotalFatalInjuries")

for (i in 1:rows) {
  v1 = xmlGetAttr(lst_ch[[i]],"EventDate")
  v2 = xmlGetAttr(lst_ch[[i]],"Location")
  v3 = xmlGetAttr(lst_ch[[i]],"Country")
  v4 = xmlGetAttr(lst_ch[[i]],"TotalFatalInjuries")
  v = c(v1,v2,v3,v4)
  dt[i,] = v
}
dt1 = dt[dt$Country == "United States",]
dt2 = dt1[dt1$TotalFatalInjuries != "",]

dt2[,1] = as.numeric(substr(dt2[,1],7,10))
dt2[,4] = as.numeric(dt2[,4])

dt_new = aggregate(dt2$TotalFatalInjuries, by = list(Year = dt2$EventDate), FUN = sum)
plot(
  x = dt_new$Year, y = dt_new$x,
  xlab = "Year", ylab = "Total Fatal Injuries in the year",
  type = "b", main = "Total Fatal Injuries in years in United States"
)
