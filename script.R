library(RMySQL)
library(dplyr)


con <- dbConnect(MySQL(),
user = 'username',
password = 'password',dbname='ipl_2015')


ipl_batting <- dbGetQuery(conn = con, "SELECT * from ipl_batting")
ipl_bowling <- dbGetQuery(conn = con, "SELECT * from ipl_bowling")
ipl_match_details <- dbGetQuery(conn = con, "SELECT * from ipl_match_details")
ipl_score_details <- dbGetQuery(conn = con, "SELECT * from ipl_score_details")

batting_ipl = tbl_df(ipl_batting)
bowling_ipl = tbl_df(ipl_bowling)
score_details_ipl = tbl_df(ipl_score_details)


match_details_ipl <- tbl_df(ipl_match_details %>% 
								mutate(match= trimws(gsub('\"(.*?):.*', '\\1', match_title))) %>% 
								arrange(match_date) %>%
								mutate(`Match_no` = match(match, `match`)))


#overall batting stats
batting_overall_ipl <- summarise( batting_ipl, 
								`Matches` = n_distinct(match_id),
								Individual Innings = n(), 
								Runs = sum(R),
								Balls = sum(B),
								`Not out` = sum(grepl('not', dismissal)),
								Highest = max(R),
								`Average` = round(Runs/(Innings - `Not out`), digits = 2),
								SR = round(Runs * 100/Balls, digits = 2),
								`4s` = sum(`4s`),
								`6s` = sum(`6s`),
								`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
								)
write.csv(batting_overall_ipl, "files/batting_overall_ipl.csv", row.names = FALSE)

#batting stats_inningwise
batting_inningwise_ipl <- batting_ipl %>% 
								group_by(innings)%>%
								summarise( 
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)

batting_positionwise_ipl <- batting_ipl %>% 
								group_by(batting_order)%>%
								summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)


batting_orderwise_ipl <- bind_rows(batting_ipl %>% 
								filter(`batting_order` %in% c(1,2,3,4))%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Top order(1,2,3,4)') %>%
								select(`Order`, everything()
								),
								batting_ipl %>% 
								filter(`batting_order` %in% c(5,6,7))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Middle order(5,6,7)') %>%
								select(`Order`, everything()
								),
								batting_ipl %>% 
								filter(`batting_order` %in% c(8,9,10,11))%>%
								summarise(   
									`Matches` = n_distinct(match_id), 
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>% 
								mutate(`Order` = 'Lower order(8,9,10,11)') %>%
								select(`Order`, everything()
								))



batting_matchwise_ipl <- batting_ipl %>%
								inner_join(match_details_ipl, by = "match_id")%>% 
								group_by(`Match_no`,`match`)%>%
								summarise(  
									`Matches` = n_distinct(match_id),  
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
									)


batting_groundwise_ipl <-  batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							mutate(`match_venue` = trimws(gsub('\\(.*\\)','', match_venue))) %>%
							group_by(ground, `match_venue`) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) 


batting_teamwise_ipl <- batting_ipl %>% 
								group_by(team)%>%
								summarise(    
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									)

batting_resultwise_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							mutate(`Teams` = 'Winning team') %>%
							select(`Teams`, everything()
								),
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							mutate(`Teams` = 'Losing team') %>%
							select(`Teams`, everything())
								)
							
batting_when_losing_all_teams_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							group_by(team) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`
								) ,
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							mutate(`team` = 'All') %>%
							select(`team`, everything())
							)

							
batting_when_winning_all_teams_ipl <- bind_rows(batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							group_by(team) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`
								) ,
							batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team == winner) %>%
							summarise(`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							mutate(`team` = 'All') %>%
							select(`team`, everything())
							)


most_runs_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)

most_runs_first_innings <- batting_ipl %>%
							filter(innings == "first") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)




most_runs_second_innings <- batting_ipl %>%
							filter(innings == "second") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)

most_runs_by_captain <- batting_ipl %>%
							filter(grepl('captain', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`)



most_runs_by_wicket_keeper <- batting_ipl %>%
							filter(grepl('\\(wk\\)', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`)

highest_score_ipl <- 	batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),
									Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Highest), `Average`) %>%
							head(25)					


highest_sr_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
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
							group_by(`Player`) %>%
							summarise(
								    Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
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
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(`4s`)) %>%
							head(25)


most_sixes_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(`6s`)) %>%
							head(25)

most_minutes_on_crease_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
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
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)


most_runs_in_losing_cause_ipl <- batting_ipl %>% 
							inner_join(match_details_ipl, by = "match_id") %>%
							filter(team != winner) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(  
									`Matches` = n_distinct(match_id),Individual Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M),
									`Boundaries %` = round(((`6s`*6)+(`4s`*4))*100/Runs, digits = 2)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)
