### HW4
# setwd("~/Teaching/Stats406/HW/HW4/")
## Load the RSQLite package (install it first if haven't yet)
#install.packages("RSQLite", dep=TRUE);
library("RSQLite");
## Make sure the file is under the working directory and then connect to it
driver = dbDriver("SQLite");
conn = dbConnect(driver, "baseball.db")
## Check the tables in a database
dbListTables(conn)
## Check the variables in a table
dbListFields(conn, "Teams")
dbListFields(conn, "Salaries")
dbListFields(conn, "Master")
dbListFields(conn, "Batting")

#### Q1 ####
avg.salaries <- dbGetQuery(
  conn,
  "SELECT yearID AS year,
  SUM(1.0*salary)/COUNT(DISTINCT teamID) as avg_salary
  FROM Salaries
  GROUP BY year
  HAVING year > 1984
  ORDER BY year"
)

avg.salaries

#### Q2 Solution 1: with 2 queries ####

ws.winners <- dbGetQuery(
  conn,
  "SELECT yearID AS year, teamID AS team
  FROM Teams
  WHERE WSWin=='Y' AND year>1984
  ORDER BY year"
)

sal.by.year.team <- dbGetQuery(
  conn,
  "SELECT yearID AS year, teamID AS team,
  SUM(salary) as team_salary
  FROM Salaries
  GROUP BY year, team
  HAVING year > 1984
  ORDER BY year"
)

winners.salary <- merge(ws.winners, sal.by.year.team)
plot(avg.salaries,type = 'b',ylim = c(0,1.5e8),col = 2)
lines(
  winners.salary[,1],winners.salary[,3],type = 'b',pch = 2,col = 4
)

#### Q2 Solution 2: with just 1 query ####

winner.sal.by.year <- dbGetQuery(
  conn,
  "SELECT Teams.yearID AS year, Teams.teamID AS team,
  SUM(1.0*salary) as team_salary
  FROM Salaries INNER JOIN Teams
  ON Salaries.yearID == Teams.yearID AND Salaries.teamID == Teams.teamID
  WHERE WSWin=='Y'
  GROUP BY year, team
  HAVING year > 1984
  ORDER BY year"
)

lines(winner.sal.by.year[,1],winner.sal.by.year[,3],col = 3)

#### Q3 Solution 1: purely SQL ####
master <- dbGetQuery(conn,"SELECT * FROM Master")
summary(as.factor(master$bats))

avg.BA.left <- dbGetQuery(
  conn,
  "SELECT yearID AS year, AVG(1.0*H/AB) AS avg.BA
  FROM Batting INNER JOIN Master ON Batting.playerID == Master.playerID
  WHERE Master.bats == 'L'
  GROUP BY year
  ORDER BY year"
)

avg.BA.right <- dbGetQuery(
  conn,
  "SELECT yearID AS year, AVG(1.0*H/AB) AS avg.BA
  FROM Batting INNER JOIN Master ON Batting.playerID == Master.playerID
  WHERE Master.bats == 'R'
  GROUP BY year
  ORDER BY year"
)

plot(
  avg.BA.left,ylim = c(0.15,0.32),type = 'l',lty = 1,
  ylab = "Average Batting Average"
)
lines(avg.BA.right,lty = 2)


#### Q3 Solution 2: partly SQL, partly R aggregate ####
BA.left <- dbGetQuery(
  conn,
  "SELECT Batting.playerID, yearID AS year, H, AB, 1.0*H/AB AS BA
  FROM Batting INNER JOIN Master ON Batting.playerID == Master.playerID
  WHERE Master.bats == 'L'
  ORDER BY year"
)

BA.right <- dbGetQuery(
  conn,
  "SELECT Batting.playerID, yearID AS year, H, AB, 1.0*H/AB AS BA
  FROM Batting INNER JOIN Master ON Batting.playerID == Master.playerID
  WHERE Master.bats == 'R'
  ORDER BY year"
)

avg.BA.left.2 <- with(BA.left,aggregate(BA ~ year,FUN = mean))
avg.BA.right.2 <- with(BA.right,aggregate(BA ~ year,FUN = mean))

lines(avg.BA.left.2,col = 2,lty = 1)
lines(avg.BA.right.2,col = 4,lty = 2)

#### Q4 ####

avg.salary.BA <- dbGetQuery(
  conn,
  "SELECT MAX(Batting.yearID) AS year, Batting.playerID AS player,
  AVG(1.0*H/AB) AS avg_BA, AVG(1.0*salary) AS avg_salary
  FROM Batting INNER JOIN Salaries
  ON Batting.playerID == Salaries.playerID
  GROUP BY player
  HAVING MAX(Batting.yearID)>1984"
)

dt4.1 <- dbGetQuery(
  conn,"SELECT Salaries.playerID,
  sum(Batting.H*1.0/Batting.AB)/count(Batting.yearID) as average_batting_averages,
  sum(salary)/count(Salaries.yearID) as average_salaries
  FROM Salaries INNER JOIN Batting ON Salaries.playerID = Batting.playerID
  GROUP BY Batting.playerID
  HAVING MAX(Batting.yearID)>1984"
)

dt4.2 <- dbGetQuery(
  conn,
  "SELECT Salaries.playerID,
  AVG(Batting.H*1.0/Batting.AB) as average_batting_averages,
  AVG(1.0*salary) as average_salaries
  FROM Salaries INNER JOIN Batting
  ON Salaries.playerID = Batting.playerID
  GROUP BY Batting.playerID
  HAVING MAX(Batting.yearID)>1984"
)


head(dt4.1,20)
head(dt4.2,20)
sum(!(is.na(dt4.1[,2]) == is.na(dt4.2[,2])))
sum(!(is.na(dt4.1[,3]) == is.na(dt4.2[,3])))
sum(dt4.1[,2] != dt4.2[,2],na.rm = T)

plot(dt4.1[,2],dt4.1[,3],main = '0/0 treated as 0')
plot(dt4.2[,2],dt4.2[,3],main = '0/0 treated as NA, ignored when averaging')

head(avg.salary.BA,20)

class(avg.salary.BA[,3])
class(dt4.2[,2])

sum(avg.salary.BA[,3] != dt4.2[,2],na.rm = T)
nrow(avg.salary.BA)
nrow(dt4.2)

plot(avg.salary.BA[,3],avg.salary.BA[,4])

dbGetQuery(conn,"SELECT playerID, yearID, H, AB
           FROM Batting
           WHERE playerID == 'aardsda01'")


## Close connections

dbDisconnect(conn)
dbUnloadDriver(drv)