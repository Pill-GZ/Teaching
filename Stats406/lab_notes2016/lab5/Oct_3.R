##Baseball database
library("RSQLite");
drv = dbDriver("SQLite");
conn = dbConnect(drv,"baseball.db")
dbListTables(conn)
sqltext1 = "SELECT * FROM Master WHERE college = 'Michigan';"
dt1 = dbGetQuery(conn, sqltext1);
sqltext2 = "SELECT * FROM Master WHERE college IN ('Michigan', 'Michigan State');"
dt2 = dbGetQuery(conn,sqltext2);

sqltext3 = "SELECT * FROM Master WHERE college IN ('Michigan', 'Michigan State') AND birthYear>1979;"
dt3 = dbGetQuery(conn,sqltext3)
##Practice
sqltext4 = "SELECT * FROM Master WHERE college IN ('Michigan','Michigan State') AND birthState NOT IN ('MI');"
dt4 = dbGetQuery(conn,sqltext4)

sqltext5 = "SELECT yearID, SUM(HR) AS total_HR FROM Teams GROUP BY yearID;"
dt5 = dbGetQuery(conn,sqltext5)
plot(dt5[,1],dt5[,2], type='l', xlab = 'Year',ylab='Home Runs',col='blue')

sqltext6 = "SELECT yearID, sum(HR) AS total_HR, 0.5*sum(G) AS total_G FROM Teams GROUP BY yearID"
dt6 = dbGetQuery(conn,sqltext6)

plot(dt6[,1],dt6[,2]/dt6[,3], type='l', xlab = 'Year',ylab='Home Runs',col='blue')

sqltext7 = "SELECT college, COUNT(playerID) AS total_players FROM Master WHERE birthYear > 1950 GROUP BY college HAVING total_players>10 ORDER BY total_players DESC;"
dt7 = dbGetQuery(conn,sqltext7);
sqltext8 = "SELECT name, W, G, SUM(salary) AS salaries FROM Teams INNER JOIN Salaries ON Teams.teamID =Salaries.teamID AND Teams.yearID = Salaries.yearID WHERE Teams.yearID=2000 GROUP BY Teams.teamID;"
dt8 = dbGetQuery(conn,sqltext8)





