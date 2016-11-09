overall_runs_by_team_gg <- ggplot(Overall_runs_by_teams_ipl, 
       aes(x = reorder(Team, -Runs), y = Runs, fill = Result, text = paste("Team: ", reorder(Team, -Runs))))+
    geom_bar(position = "dodge",stat = "identity") + ggtitle("Total Runs scored by the Teams") + xlab("")+ ylab("Runs")+
    coord_flip() +theme_mine()
    
ggplotly(overall_runs_by_team_gg, tooltip = c("text", "y", "fill"))

overall_runs_distribution <- gather(Overall_runs_ipl, key = `Runs_class`, 
								value = `Runs`, -c(Result, Team, Matches)) %>% 
								filter(Runs_class %in% c("Runs by Batsmen", "Extras"))

ggplot(overall_runs_distribution, 
       aes(x = Result, y = Runs, fill = Runs_class))+
    geom_bar(stat = "identity") + ggtitle("Total Runs scored by the Teams") + xlab("")+ ylab("Runs")+
    coord_flip() +theme_mine()

overall_extras_All <- gather(Overall_runs_ipl, key = `Extras_class`, 
								value = `Runs`, -c(Result, Team, Matches)) %>% 
								filter(!Extras_class %in% c("Runs","Runs by Batsmen", "Extras"), Result == "All")
