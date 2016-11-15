library(RMySQL)
library(dplyr)


con <- dbConnect(MySQL(), dbname='ipl_2015')


ipl_batting <- dbGetQuery(conn = con, "SELECT * from ipl_batting")
ipl_bowling <- dbGetQuery(conn = con, "SELECT * from ipl_bowling")
ipl_match_details <- dbGetQuery(conn = con, "SELECT * from ipl_match_details")
ipl_score_details <- dbGetQuery(conn = con, "SELECT * from ipl_score_details")

dbDisconnect(conn = con)

batting_ipl = tbl_df(ipl_batting)
bowling_ipl = tbl_df(ipl_bowling)
score_details_ipl = tbl_df(ipl_score_details)


match_details_ipl <- tbl_df(ipl_match_details %>% 
								mutate(match= trimws(gsub("^\"|:.*$", "", match_title)), `Match_result` = trimws(gsub("^.*won", "", match_result))) %>% 
								arrange(match_date) %>%
								mutate(`Match_no` = match(match, `match`)))

match_and_score_details_ipl <- match_details_ipl %>% inner_join(score_details_ipl, by = "match_id") %>%  
							    mutate(`Batting Team` = ifelse(innings=="first", team1, team2)) %>% 
							    arrange(match_date) 
#overall batting stats
batting_overall_ipl <- summarise( batting_ipl, 
								`Matches` = n_distinct(match_id),
								`Individual Innings` = n(), 
								Runs = sum(R),
								Balls = sum(B),
								`Not out` = sum(grepl('not', dismissal)),
								Highest = max(R),
								`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
								SR = round(Runs * 100/Balls, digits = 2),
								`4s` = sum(`4s`),
								`6s` = sum(`6s`),
								`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
								)

Overall_runs_ipl <- bind_rows(match_and_score_details_ipl %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
														)%>%
								mutate(`Result` = "All", `Team` = "All teams")%>%
								select(`Result`, `Team`,`Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()
									),
								match_and_score_details_ipl %>%
								filter(winner == `Batting Team`) %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
								)%>%
								mutate(`Result` = "Won", `Team` = "All teams")%>%
								select(`Result`, `Team`,`Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()),
								match_and_score_details_ipl %>%
								filter(winner != `Batting Team`) %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
											)%>%
								mutate(`Result` = "Lost", `Team` = "All teams")%>%
								select(`Result`, `Team`,`Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()
								))
								
#write.csv(batting_overall_ipl, "files/batting_overall_ipl.csv", row.names = FALSE)
Overall_runs_by_teams_ipl <- bind_rows(match_and_score_details_ipl %>%
								filter(winner != `Batting Team`) %>%
								group_by(`Batting Team`) %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
											)%>%
								mutate(`Result` = "Lost")%>%
								select(`Result`, `Team`= `Batting Team`,`Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()
								),match_and_score_details_ipl %>%
								filter(winner == `Batting Team`) %>%
								group_by(`Batting Team`) %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
											)%>%
								mutate(`Result` = "Won")%>%
								select(`Result`, `Team`= `Batting Team`,`Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()
								),
								match_and_score_details_ipl %>%
								group_by(`Batting Team`) %>%
								summarise(`Matches` = n_distinct(match_id),
											`Runs` = sum(innings_total),
											`Extras` = sum(innings_extras),
											`Runs by Batsmen` = `Runs` - `Extras`,
											`Wides` = sum(innings_extras_wide),
											`No Balls` = sum(innings_extras_nb),
											`Leg-byes` = sum(innings_extras_lb),
											`Byes` = sum(innings_extras_bye)
											)%>%
								mutate(`Result` = "All")%>%
								select(`Result`, `Team`= `Batting Team`, `Matches`, `Runs`,`Runs by Batsmen`, `Extras`,  everything()
								)
								) %>% mutate(Result = factor(Result, levels = c("All", "Won", "Lost")) ) %>%
								arrange(desc(Runs))
								


team_total_matchwise_ipl <- batting_ipl %>% 
								inner_join(match_and_score_details_ipl, by = c("match_id", "team"="Batting Team")) %>% 
								group_by(team, innings.x, `match`,`Match_no`, match_date) %>% 
#								mutate(id = seq_len(n())) %>% 
								summarise(`Individual Innings` = n(),
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
								) %>% arrange(`Match_no`, innings.x) %>% 
                				select(`Team` = `team`, `Innings` = innings.x, `Match` = `match`,
                      			`Individual Innings`, `Runs`, `Balls`, `Not out`, `Highest`,
                      			`Average`, `SR`, `4s`, `6s`, `Boundaries %`,
                   			    `Match_date` = `match_date`, `Match_no`) %>%
                mutate(Average = ifelse(!is.finite(Average), as.numeric(Runs), Average))


team_total_team_and_matchwise <- bind_rows(team_total_matchwise_ipl %>%
									filter(Team == "Chennai Super Kings") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Delhi Daredevils") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Kings XI Punjab") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Kolkata Knight Riders") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Mumbai Indians") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Rajasthan Royals") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Royal Challengers Bangalore") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										),
									team_total_matchwise_ipl %>%
									filter(Team == "Sunrisers Hyderabad") %>% 
									arrange(`Match_no`) %>%
									mutate(`match_no_for_team` = 1:n()) %>%
									select(`Match_no for team` = `match_no_for_team`, everything()
										)
									)


#batting stats_inningswise
batting_inningswise_ipl <- batting_ipl %>% 
								group_by(innings)%>%
								summarise( 
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)%>%
									mutate(`Result` = "All") %>%
								select(`Result`, `Innings` = innings, everything())

batting_innings_resultwise_ipl <- bind_rows(batting_ipl %>% 
								group_by(innings)%>%
								summarise( 
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)%>%
									mutate(`Result` = "All") %>%
								select(`Result`, `Innings` = innings, everything()
									),
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
								group_by(innings)%>%
								summarise( 
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)%>% mutate(`Result` = "Won") %>% 
								select(`Result`, `Innings` = innings, everything()
									),
								batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
								group_by(innings)%>%
								summarise( 
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)%>% mutate(`Result` = "Lost") %>% 
								select(`Result`, `Innings` = innings, everything())
									)


batting_resultwise_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							mutate(`Teams` = 'Winning team') %>%
							select(`Teams`, everything()
								),
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							mutate(`Teams` = 'Losing team') %>%
							select(`Teams`, everything())
								)

batting_positionwise_ipl <- bind_rows(batting_ipl %>% 
#								inner_join(score_details_ipl, by = c("match_id", "innings")) %>%
								group_by(batting_order)%>%
								summarise(  
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(score_details_ipl$innings_total), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
								mutate(`Result` = "All") %>%
								select(`Result`, `Batting Order` = batting_order, everything()
									),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team == winner) %>%
								group_by(batting_order)%>%
								summarise(  
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Won"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
								mutate(`Result` = "Won") %>%
								select(`Result`, `Batting Order` = batting_order, everything()
									),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team != winner) %>%
								group_by(batting_order)%>%
								summarise(  
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Lost"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
								mutate(`Result` = "Lost") %>%
								select(`Result`, `Batting Order` = batting_order, everything()
								) )%>%
								arrange(`Batting Order`) %>% 
								mutate(Average = ifelse(!is.finite(Average), Runs, Average))


batting_orderwise_ipl <- bind_rows(batting_ipl %>% 
								filter(`batting_order` %in% c(1,2,3,4))%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(score_details_ipl$innings_total), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Top order(1,2,3,4)' , `Result` = "All") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								filter(`batting_order` %in% c(5,6,7))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(score_details_ipl$innings_total), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Middle order(5,6,7)' , `Result` = "All") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								filter(`batting_order` %in% c(8,9,10,11))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(score_details_ipl$innings_total), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Lower order(8,9,10,11)' , `Result` = "All") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team == winner) %>%
								filter(`batting_order` %in% c(1,2,3,4))%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Won"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Top order(1,2,3,4)', `Result` = "Won") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team == winner) %>%
								filter(`batting_order` %in% c(5,6,7))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Won"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Middle order(5,6,7)', `Result` = "Won") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team == winner) %>%
								filter(`batting_order` %in% c(8,9,10,11))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Won"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Lower order(8,9,10,11)', `Result` = "Won") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team != winner) %>%
								filter(`batting_order` %in% c(1,2,3,4))%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Lost"]), digit = 2),									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Top order(1,2,3,4)', `Result` = "Lost") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team != winner) %>%
								filter(`batting_order` %in% c(5,6,7))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Lost"]), digit = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Middle order(5,6,7)', `Result` = "Lost") %>%
								select(`Result`, `Order`, everything()
								),
								batting_ipl %>% 
								inner_join(match_details_ipl, by = "match_id") %>%
								filter(team != winner) %>%
								filter(`batting_order` %in% c(8,9,10,11))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %` = round(Runs*100/sum(`Overall_runs_ipl`$Runs[Overall_runs_ipl$`Result`=="Lost"]), digit = 2),	
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Lower order(8,9,10,11)', `Result` = "Lost") %>%
								select(`Result`, `Order`, everything()
								)) 
								



batting_matchwise_ipl <- batting_ipl %>%
								inner_join(match_details_ipl, by = "match_id")%>% 
								group_by(`Match_no`,`match`, team1, team2, winner, `Match_result`)%>%
								summarise(  
									`Number of batsmen` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(n() - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
								select(`Match_no`, `Match`= `match`, everything())


batting_groundwise_ipl <-  batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							mutate(`match_venue` = trimws(gsub('\\(.*\\)','', match_venue))) %>%
							group_by(ground, `match_venue`) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							select(`Ground` = ground, `Location` = match_venue, everything()) %>%
							arrange(desc(Runs), desc(`Average`)) 


batting_teamwise_ipl <- batting_ipl %>% 
								group_by(team)%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
								select(`Team` = team, everything())


							
batting_when_losing_all_teams_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							group_by(team) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							select(`Team` = team, everything())%>%
							arrange(desc(Runs), desc(`Average`)
								) ,
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							mutate(`team` = 'All') %>%
							select(`Team` = team, everything())
							)

							
batting_when_winning_all_teams_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							group_by(team) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							select(`Team` = team, everything())%>%
							arrange(desc(Runs), desc(`Average`)
								) ,
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							mutate(`team` = 'All') %>%
							select(`Team` = team, everything())
							)


most_runs_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`, team) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							head(25)

most_runs_first_innings <- batting_ipl %>%
							filter(innings == "first") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							head(25)




most_runs_second_innings <- batting_ipl %>%
							filter(innings == "second") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							head(25)

most_runs_by_captain <- batting_ipl %>%
							filter(grepl('captain', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`))



most_runs_by_wicket_keeper <- batting_ipl %>%
							filter(grepl('\\(wk\\)', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`))

highest_scores_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							mutate(	SR = round(R * 100/B, digits = 2),
									`Team Runs %`= round(R*100/innings_total, digits = 2),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/R, digits = 2),
									Result = paste(ifelse(winner == team, "Won", "Lost"), Match_result, sep = " ")
									) %>%
							select( `Player`,
									Runs = R,
									Dismissal = dismissal,
									Balls = B,
									`4s`,
									`6s`,
									SR,
									`Min` = M,
									Team = team,
									Opponent = opposition,
									Result,
									Match = `match`,
									`Match Date` = match_date,
									`Team Runs %`,
									`Boundaries %`) %>%
							arrange(desc(Runs)) %>% 
							head(25)				


highest_sr_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							filter((`Average` > 25.25), Runs > 100) %>%
							arrange(desc(SR), `Average`) %>%
							head(25)




highest_average_ipl  <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(
								    `Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							filter( Runs > 100) %>%
							arrange(desc(SR), `Average`) %>%
							head(25)

most_fours_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(`4s`)) %>%
							head(25)


most_sixes_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(`6s`)) %>%
							head(25)

most_minutes_on_crease_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(`Min`)) %>%
							head(25)

most_runs_in_winning_cause_ipl <- batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							head(25)


most_runs_in_losing_cause_ipl <- batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), desc(`Average`)) %>% 
							head(25)

most_percentage_of_runs_contributed_to_team <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`, team) %>%
							summarise(
#							`Matches` = n_distinct(match_id),
							`Individual Innings` = n(),
							Runs = sum(R),
							Balls = sum(B),
							`Not out` = sum(grepl('not', dismissal)),
							Highest = max(R),
							`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
							SR = round(Runs * 100/Balls, digits = 2),
							`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
							`4s` = sum(`4s`),
							`6s` = sum(`6s`),
							`Min` = sum(M),
							`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
							)  %>% arrange(desc(`Team Runs %`)) %>% head(25)


most_fifty_plus_scores <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							inner_join(match_and_score_details_ipl, by =c("match_id", "team" = "Batting Team")) %>%
							group_by(`Player`) %>%
							summarise(  
#									`Matches` = n_distinct(match_id),
									`Individual Innings` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(`Individual Innings` - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`50plus` = sum(R>50),
									`Team Runs %`= round(Runs*100/sum(innings_total), digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
							arrange(desc(`50plus`), `Individual Innings`, desc(Runs)) %>%
							head(20)



list_batting <- ls(pattern = '^.*ipl$')

ifelse(!dir.exists(file.path("files")), dir.create(file.path("files")), FALSE)

for (i in seq_along(list_batting)){
	write.csv(get(list_batting[i]), file = paste("files/", list_batting[i], ".csv", sep = ""), row.names = FALSE,
          fileEncoding='utf8')}

# #lapply(seq_along(list_batting), 
# 	function(x) 
# 	(write.csv(get(list_batting[x]), file = paste("files/", list_batting[x], ".csv", sep = ""), row.names = FALSE))
# 	)
# for (i in seq_along(list_batting)){
# 	View(get(list_batting[i]))
