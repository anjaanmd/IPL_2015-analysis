CREATE DATABASE ipl_2015;

use ipl_2015;

CREATE TABLE ipl_match_details(
	match_id INT NOT NULL KEY,
    team1 VARCHAR(30) NOT NULL,
    team2 VARCHAR(30) NOT NULL,
    winner VARCHAR(30) NOT NULL,
    match_venue VARCHAR(30) NOT NULL,
    match_date DATE NOT NULL,
    ground VARCHAR(130) NOT NULL,
    max_overs FLOAT(5,2) NOT NULL,
    match_title VARCHAR(130) NOT NULL,
    match_result VARCHAR(130) NOT NULL,
    INDEX match_index (match_id,team1,team2,winner,match_venue,match_date,ground,max_overs,match_title,match_result)
    )DEFAULT CHARSET=utf8;
 
CREATE TABLE ipl_score_details(
    innings_total INT NOT NULL,
    innings_extras INT NOT NULL,
    innings_wickets INT NOT NULL,
    innings_overs_bowled FLOAT(3,1) NOT NULL,
    innings_run_rate FLOAT(4,2) NOT NULL,
    innings_extras_lb INT NOT NULL,
    innings_extras_bye INT NOT NULL,
    innings_extras_wide INT NOT NULL,
    innings_extras_nb INT NOT NULL,
    innings VARCHAR(7) NOT NULL,
    match_id INT NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES ipl_match_details(match_id),
    INDEX score_index (innings_total,innings_wickets, innings_extras,innings_overs_bowled,innings_run_rate,
	innings_extras_lb,innings_extras_bye,innings_extras_wide,innings_extras_nb, innings,match_id)
    )DEFAULT CHARSET=utf8;


CREATE TABLE ipl_batting(
	batsman VARCHAR(30) NOT NULL,
    dismissal VARCHAR(70) NOT NULL,
    R INT NOT NULL,
    M INT NOT NULL,
    B INT NOT NULL,
    4s INT NOT NULL,
    6s INT NOT NULL,
    SR FLOAT(5,2) NULL,
    batting_order INT NOT NULL,
    match_id INT NOT NULL,
    innings VARCHAR(40) NOT NULL,
    team VARCHAR(40) NOT NULL,
    opposition VARCHAR(40) NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES ipl_match_details(match_id), 
    INDEX batting_index (batsman,dismissal,R,M,B,4s,6s,SR,batting_order,match_id,innings,team,opposition)
    )DEFAULT CHARSET=utf8;


CREATE TABLE ipl_bowling(
	bowler VARCHAR(30) NOT NULL,
    O FLOAT(3,1) NOT NULL,
    M INT NOT NULL,
    R INT NOT NULL,
    W INT NOT NULL,
    Econ FLOAT(4,2) NULL,
    bowling_order INT NOT NULL,
    match_id  INT NOT NULL,
    innings VARCHAR(6) NOT NULL,
    team VARCHAR(30) NOT NULL,
    opposition  VARCHAR(30) NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES ipl_match_details(match_id),
    INDEX bowling_index (bowler,O,M,R,W,Econ,bowling_order,match_id,innings,team,opposition)
    )DEFAULT CHARSET=utf8;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.match_details.csv' INTO TABLE ipl_match_details 
 FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;

LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.batting.csv' INTO TABLE  ipl_batting FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.bowling.csv' INTO TABLE  ipl_bowling FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;


LOAD DATA INFILE '/var/lib/mysql-files/2015_ipl.score_details.csv' INTO TABLE  ipl_score_details FIELDS TERMINATED BY ','   LINES TERMINATED BY '\n' IGNORE 1 LINES;
