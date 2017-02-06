## lab 4
setwd("/home/gaozheng/Teaching/Stats406/R_scripts/lab4/")
library("RSQLite")


##################### first example ######################

# Load the driver and connect to the database
drv = dbDriver("SQLite")
conn = dbConnect(drv,"Test.db")

# Work with the database...
attributes(conn)
dbListTables(conn)
dbListFields(conn,dbListTables(conn))

sqltext = "SELECT * FROM emissions;";
dt1 = dbGetQuery(conn,sqltext)
?"dbGetQuery"
class(dt1)

sqltext = "SELECT country, val_2001 FROM emissions;";
dt2 = dbGetQuery(conn,sqltext);

sqltext = "SELECT country, round(val_2001,3) AS v_2001,round(val_2006,3) AS v_2006 FROM emissions;"
dt3 = dbGetQuery(conn,sqltext);

sqltext = "SELECT country, round(val_2001,3) AS v_2001,
round(val_2006,3) AS v_2006 FROM emissions WHERE
country IN ('\"United States\"', '\"China\"',
'\"Brazil\"', '\"Egypt\"');"
dt3 = dbGetQuery(conn,sqltext);

sqltext = "SELECT country, round(val_2001,3) AS v_2001,
round(val_2006,3) AS v_2006 FROM emissions WHERE
country LIKE '\"U%';"
dt3 = dbGetQuery(conn,sqltext);

# When you're done, close the connection.
dbDisconnect(conn)
dbUnloadDriver(drv)



##################### second example #####################

library("RSQLite")
drv = dbDriver("SQLite")
conn = dbConnect(drv,"baseball.db")
dbListTables(conn)
dbListFields(conn,"Master")
dbListFields(conn,"Salaries")

dbListFields(conn,"Teams")

sqltext = "SELECT yearID,
SUM(HR) as total_HR FROM Teams GROUP BY yearID;"
dt4 = dbGetQuery(conn,sqltext)
plot(
  dt4[,1],dt4[,2],xlab = 'Years',
  ylab = 'Number of HRs',type = 'b',col = 'blue'
)

sqltext = "SELECT yearID, sum(HR) as total_HR, 0.5*sum(G)
as total_G from Teams GROUP BY yearID";
dt5 = dbGetQuery(conn, sqltext)
plot(
  dt5[,1],dt5[,2] / dt5[,3],xlab = 'Years',
  ylab = 'Number of HRs',type = 'b',col = 'blue'
)

dt7 = dbGetQuery(
  conn, "SELECT college,
  count(playerID) as total_players from Master WHERE
  birthYear > 1950 GROUP BY college
  HAVING total_players > 10
  ORDER BY total_players DESC"
)
