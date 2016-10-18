USE ipl_2015;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.match_details.csv' INTO TABLE match_details 
 FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.first_innings_batting.csv' INTO TABLE  first_innings_batting FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.second_innings_batting.csv' INTO TABLE  second_innings_batting FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.second_innings_bowling.csv' INTO TABLE  second_innings_bowling FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.first_innings_bowling.csv' INTO TABLE  first_innings_bowling FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.score_details.csv' INTO TABLE  score_details FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;
