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



most_runs_ipl <-batting_ipl %>%
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
			arrange(desc(Runs))

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
			arrange(desc(Runs))

