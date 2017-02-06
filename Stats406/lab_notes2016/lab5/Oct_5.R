library(RSQLite);
drv = dbDriver('SQLite');
conn = dbConnect(drv,'baseball.db');

sqltext = "SELECT COUNT(playerID) AS nb_left_players FROM MASTER WHERE bats='L';"
dt1 = dbGetQuery(conn, sqltext)
## How about the counts for all levels of bats?
sqltext = "SELECT bats, COUNT(playerID) AS nb_left_players FROM Master GROUP BY bats;"
dt2 = dbGetQuery(conn, sqltext)

sqltext = "SELECT strftime('%Y',debut) as debut_year, AVG(bats='B') FROM Master GROUP BY debut_year;"
dt3 = dbGetQuery(conn, sqltext)
plot(dt3[,1],dt3[,2],type = 'b',col = 'blue')

sqltext = "SELECT name, 1.0*W/G AS record, sum(salary) AS salary
  FROM Teams INNER JOIN Salaries ON Teams.yearID =Salaries.yearID AND Teams.teamID = Salaries.teamID
  WHERE Teams.yearID = 2000
  GROUP BY Teams.teamID;"
dt4 = dbGetQuery(conn,sqltext)

sqltext = "SELECT name, Teams.yearID AS year, 1.0*W/G AS record, sum(salary) AS salary 
  FROM Teams INNER JOIN Salaries ON Teams.yearID =Salaries.yearID AND Teams.teamID = Salaries.teamID 
  GROUP BY Teams.teamID, Teams.yearID 
  HAVING Teams.yearID>1980;"
dt5 = dbGetQuery(conn,sqltext);
