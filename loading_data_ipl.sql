USE ipl_2015;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.match_details.csv' INTO TABLE ipl_match_details 
 FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.batting.csv' INTO TABLE  ipl_batting FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.bowling.csv' INTO TABLE  ipl_bowling FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.score_details.csv' INTO TABLE  ipl_score_details FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;
