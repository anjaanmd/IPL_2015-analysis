library(dplyr)

#overall batting stats
batting_overall_ipl <- summarise( batting_ipl, 
								Innings = n(), 
								Runs = sum(R),
								Balls = sum(B),
								`Not out` = sum(grepl('not', dismissal)),
								Highest = max(R),
								`Average` = round(Runs/(Innings - `Not out`), digits = 2),
								SR = round(Runs * 100/Balls, digits = 2),
								`4s` = sum(`4s`),
								`6s` = sum(`6s`)
								)

#batting stats_inningwise
batting_inningwise_ipl <- batting_ipl %>% 
								group_by(innings)%>%
								summarise(  
									Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`)
									)

batting_orderwise_ipl <- batting_ipl %>% 
								group_by(batting_order)%>%
								summarise(  
									Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`)
									)

batting_matchwise_ipl <- batting_ipl %>% 
								group_by(match_id)%>%
								summarise(  
									`Number of batsmen` = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(n() - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`)
									) %>% 
								mutate(Match_no = match(match_id, match_id)) %>%
								select(Match_no, `Number of batsmen`:`6s`)



most_runs_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)

most_runs_first_innings <- batting_ipl %>%
							filter(innings == "first") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)




most_runs_second_innings <- batting_ipl %>%
							filter(innings == "second") %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(Runs), `Average`) %>% 
							head(25)

most_runs_by_captain <- batting_ipl %>%
							filter(grepl('captain', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(Runs), `Average`)



most_runs_by_wicket_keeper <- batting_ipl %>%
							filter(grepl('\\(wk\\)', batsman)) %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(Runs), `Average`)						

highest_sr_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							filter((`Average` > 25.25), Runs > 100) %>%
							arrange(desc(SR), `Average`) %>%
							head(25)					

highest_average_ipl  <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							filter( Runs > 100) %>%
							arrange(desc(SR), `Average`) %>%
							head(25)

most_fours_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(`4s`)) %>%
							head(25)


most_sixes_ipl <- batting_ipl %>%
							mutate(`Player` = trimws(gsub('\\(.*\\)','', batsman))) %>%
							group_by(`Player`) %>%
							summarise(Innings = n(), 
									Runs = sum(R),
									Balls = sum(B),
									`Not out` = sum(grepl('not', dismissal)),
									Highest = max(R),
									`Average` = round(Runs/(Innings - `Not out`), digits = 2),
									SR = round(Runs * 100/Balls, digits = 2),
									`4s` = sum(`4s`),
									`6s` = sum(`6s`),
									`Min` = sum(M)
									) %>%
							arrange(desc(`6s`)) %>%
							head(25)
