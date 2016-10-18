CREATE DATABASE ipl_2015;
use ipl_2015;
CREATE TABLE match_details(
	match_id INT(8) NOT NULL KEY,
    team1 VARCHAR(30) NOT NULL,
    team2 VARCHAR(30) NOT NULL,
    winner VARCHAR(30) NOT NULL,
    match_venue VARCHAR(30) NOT NULL,
    match_date DATE NOT NULL,
    ground VARCHAR(130) NOT NULL,
    max_overs FLOAT NOT NULL,
    match_title VARCHAR(130) NOT NULL,
    match_result VARCHAR(130) NOT NULL,
    INDEX match_index (match_id,team1,team2,winner,match_venue,match_date,ground,max_overs,match_title,match_result)
    )DEFAULT CHARSET=utf8;
 
CREATE TABLE score_details(
	first_innings_total INT(3) NOT NULL,
    first_innings_extras INT(3) NOT NULL,
    first_innings_wickets INT(2) NOT NULL,
    first_innings_overs_bowled FLOAT NOT NULL,
    first_innings_run_rate FLOAT NOT NULL,
    first_innings_extras_lb INT(3) NOT NULL,
    first_innings_extras_bye INT(3) NOT NULL,
    first_innings_extras_wide INT(3) NOT NULL,
    first_innings_extras_nb INT(3) NOT NULL,
    second_innings_total INT(3) NOT NULL,
    second_innings_extras INT(3) NOT NULL,
    second_innings_wickets INT(3) NOT NULL,
    second_innings_overs_bowled FLOAT NOT NULL,
    second_innings_run_rate FLOAT NOT NULL,
    second_innings_extras_lb INT(3) NOT NULL,
    second_innings_extras_bye INT(3) NOT NULL,
    second_innings_extras_wide INT(3) NOT NULL,
    second_innings_extras_nb INT(3) NOT NULL,
    match_id INT(8) NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES match_details(match_id),
    INDEX score_index (first_innings_total,first_innings_extras,first_innings_overs_bowled,first_innings_run_rate,first_innings_extras_lb,first_innings_extras_bye,first_innings_extras_wide,second_innings_total,second_innings_extras,second_innings_overs_bowled,second_innings_run_rate,second_innings_extras_lb,second_innings_extras_bye,second_innings_extras_wide,match_id)
    )DEFAULT CHARSET=utf8;
CREATE TABLE first_innings_batting(
	batsman VARCHAR(30) NOT NULL,
    dismissal VARCHAR(70) NOT NULL,
    R INT(3) NOT NULL,
    M INT(3) NOT NULL,
    B INT(3) NOT NULL,
    4s INT(2) NOT NULL,
    6s INT(2) NOT NULL,
    SR FLOAT NULL,
    batting_order INT(2) NOT NULL,
    match_id INT(8) NOT NULL,
    innings VARCHAR(40) NOT NULL,
    team VARCHAR(40) NOT NULL,
    opposition VARCHAR(40) NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES match_details(match_id), 
    INDEX batting_index (batsman,dismissal,R,M,B,4s,6s,SR,batting_order,match_id,innings,team,opposition)
    )DEFAULT CHARSET=utf8;
    desc first_innings_batting;
show INDEX FROM first_innings_batting;

CREATE TABLE second_innings_batting LIKE first_innings_batting;
show INDEX FROM second_innings_batting;

CREATE TABLE first_innings_bowling(
	bowler VARCHAR(30) NOT NULL,
    O FLOAT NOT NULL,
    M INT(1) NOT NULL,
    R INT(2) NOT NULL,
    W INT(2) NOT NULL,
    Econ FLOAT NULL,
    bowling_order INT(2) NOT NULL,
    match_id  INT(8) NOT NULL,
    innings VARCHAR(6) NOT NULL,
    team VARCHAR(30) NOT NULL,
    opposition  VARCHAR(30) NOT NULL,
    constraint foreign key for_key(match_id) REFERENCES match_details(match_id),
    INDEX bowling_index (bowler,O,M,R,W,Econ,bowling_order,match_id,innings,team,opposition)
    )DEFAULT CHARSET=utf8;
    
CREATE TABLE second_innings_bowling like first_innings_bowling;



