
### 1. Logistics
## Load the RSQLite package (if necessary, also install it first):
if(!(require(RSQLite))){
	install.packages("RSQLite", dep=TRUE);
	require(RSQLite);
}
## Connect to the file
## Make sure the file is under R's working directory
driver = dbDriver("SQLite");
conn = dbConnect(driver, "baseball.db");

dbListTables(conn);
dbListFields(conn, "Teams");
dbListFields(conn, "Salaries");



### Quiz
Table1.Teams = dbGetQuery(conn, "
                        SELECT yearID AS year, Count(teamID) NumberNetWinTeams
                        FROM Teams
                        WHERE W>L
                        GROUP BY yearID
                        ORDER BY yearID
						");


### Example
Table2.Salaries = dbGetQuery(conn, "
						SELECT T1.yearID year, T1.teamID Team1ID, T2.teamID Team2ID, T1.SumSalary-T2.SumSalary SalaryDifference
						FROM (	SELECT yearID, teamID, Sum(salary) SumSalary
								FROM Salaries
								GROUP BY yearID, teamID
								ORDER BY yearID, teamID
							) T1
							INNER JOIN
							(
								SELECT yearID, teamID, Sum(salary) SumSalary
								FROM Salaries
								GROUP BY yearID, teamID
								ORDER BY yearID, teamID
							) T2
							ON
								T1.yearID=T2.yearID AND T1.teamID<T2.teamID
						WHERE T1.yearID>1996
						GROUP BY T1.yearID, T1.teamID, T2.teamID
						ORDER BY T1.yearID, T1.teamID
						");

















