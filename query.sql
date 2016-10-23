USE ipl_2015;


 
SELECT count(*) AS `Innings`,
       sum(R) AS `Runs`,
       sum(B) AS `Balls`,
       sum(dismissal LIKE 'not%') AS 'Not out',
       max(R) AS `Highest`,
       cast((sum(R) / (count() - sum(dismissal LIKE 'not%'))) AS DECIMAL(5,2)) `Average`,
       cast((sum(R) * 100 / sum(B)) AS DECIMAL(5,2)) `SR`,
       sum(4s) AS `4s`,
       sum(6s) AS `6s`,
       sum(M) AS `Minutes`,
       team AS `Team`
FROM ipl_batting
ORDER BY `Runs` DESC LIMIT 25;



 #Most runs
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Runs` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Most_runs_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Most runs IN the FIRST innings
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   WHERE innings="first"
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Runs` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Most_runs_first_innings_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #SECOND Innings Most Runs
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   WHERE innings="second"
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Runs` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Most_runs_second_innings_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Highest averages
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Average` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Highest_averages_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Highest Averages IN the FIRST Innings
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   WHERE innings="first"
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Average` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Highest_averages_first_innings_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Highest Averages IN the SECOND innings
SELECT tb1.batsman AS `Player`,
       sum(tb1.Inn) AS `Innings`,
       sum(tb1.R) AS `Runs`,
       sum(tb1.B) AS `Balls`,
       sum(tb1. `Not out`) AS `NO`,
       max(tb1.High) AS `Highest`,
       cast((sum(tb1.R) / (sum(tb1.Inn) - sum(tb1.`Not out`))) AS DECIMAL(5,2)) `Average`,
       cast((sum(tb1.R) * 100 / sum(tb1.B)) AS DECIMAL(5,2)) `SR`,
       sum(tb1.fours) AS `4s`,
       sum(tb1.sixes) AS `6s`,
       sum(tb1.Min) AS `Minutes`
FROM
  (SELECT SUBSTRING_INDEX(batsman, '(', 1) AS `batsman`,
          count(match_id) AS `Inn`,
          sum(R) AS `R`,
          sum(B) AS `B`,
          sum(dismissal LIKE 'not%') AS 'Not out',
          max(R) AS `High`,
          sum(4s) AS `fours`,
          sum(6s) AS `sixes`,
          sum(M) AS `Min`,
          team AS `Team`
   FROM ipl_batting
   WHERE innings="second"
   GROUP BY `batsman`,
            `team`) tb1
GROUP BY `Player`
ORDER BY `Average` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Highest_averages_second_innings_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Players WITH highest strike rates who averages
MORE THAN #Most runs BY a captain
SELECT batsman, AS `Player`,
       count(match_id) AS `Innings`,
       sum(R) AS `Runs`,
       sum(B) AS `Balls`,
       sum(dismissal LIKE 'not%') AS 'Not out',
       max(R) AS `Highest`,
       cast((sum(R) / (count(match_id) - sum(dismissal LIKE 'not%'))) AS DECIMAL(5,2)) `Average`,
       cast((sum(R) * 100 / sum(B)) AS DECIMAL(5,2)) `SR`,
       sum(4s) AS `4s`,
       sum(6s) AS `6s`,
       sum(M) AS `Minutes`,
       team AS `Team`
FROM ipl_batting
WHERE batsman LIKE '%(captain)%'
GROUP BY batsman,
         team
ORDER BY `Runs` DESC LIMIT 25 INTO outfile '/var/lib/mysql-files/export/ipl/Most_runs_captain_ipl.csv' FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n';

 #Most runs BY a wicket-keeper
SELECT batsman AS `Player`,
       count(match_id) AS `Innings`,
       sum(R) AS `Runs`,
       sum(B) AS `Balls`,
       sum(dismissal LIKE 'not%') AS 'Not out',
       max(R) AS `Highest`,
       cast((sum(R) / (count(match_id) - sum(dismissal LIKE 'not%'))) AS DECIMAL(5,2)) `Average`,
       cast((sum(R) * 100 / sum(B)) AS DECIMAL(5,2)) `SR`,
       sum(4s) AS `4s`,
       sum(6s) AS `6s`,
       sum(M) AS `Minutes`,
       team AS `Team`
FROM ipl_batting
WHERE batsman LIKE '%(wk)%'
GROUP BY batsman,
         team
ORDER BY `Runs` DESC LIMIT 25;


