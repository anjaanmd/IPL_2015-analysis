overall_runs_by_team_bar <- ggplot(Overall_runs_by_teams_ipl,
                                   aes(
                                     x = reorder(Team, -Runs),
                                     y = Runs,
                                     fill = Result,
                                     text = paste("Team: ", reorder(Team, -Runs))
                                   )) +
  geom_bar(position = "dodge", stat = "identity") + ggtitle("Total Runs scored by the Teams") + xlab("") + ylab("Runs") +
  coord_flip() + theme_mine()

#ggplotly(overall_runs_by_team_gg, tooltip = c("text", "y", "fill"))

overall_runs_by_team_pie <-
  Overall_runs_by_teams_ipl %>% filter(Result == "All") %>%
  ggplot(., aes(x = Result, y = Runs, fill = Team)) +
  geom_bar(stat = "identity") + ggtitle("Runs scored by the Teams") +
  xlab("") + ylab("Runs") +
  theme_mine() + coord_polar(theta = "y") + theme(axis.text.x =
                                                    element_blank()) +
  geom_text(aes(y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = percent(Runs / sum(Runs))))

# overall_runs_distribution <- gather(Overall_runs_ipl, key = `Runs_class`,
# 								value = `Runs`, -c(Result, Team, Matches)) %>%
# 								filter(Runs_class %in% c("Runs by Batsmen", "Extras"))

overall_runs_distribution_all <-
  gather(Overall_runs_ipl,
         key = `Runs_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(Runs_class %in% c("Runs by Batsmen", "Extras"), Result == "All")


overall_runs_distribution_won <-
  gather(Overall_runs_ipl,
         key = `Runs_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(Runs_class %in% c("Runs by Batsmen", "Extras"), Result == "Won")



overall_runs_distribution_lost <-
  gather(Overall_runs_ipl,
         key = `Runs_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(Runs_class %in% c("Runs by Batsmen", "Extras"), Result == "Lost")


overall_runs_distribution_all_pie <-
  ggplot(overall_runs_distribution_all,
         aes(x = Result, y = Runs, fill = Runs_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "All") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) + 
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(b = -5, unit = "pt")))

overall_runs_distribution_won_pie <-
  ggplot(overall_runs_distribution_won,
         aes(x = Result, y = Runs, fill = Runs_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "Won") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) +
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(b = -5, unit = "pt")))

overall_runs_distribution_lost_pie <-
  ggplot(overall_runs_distribution_lost,
         aes(x = Result, y = Runs, fill = Runs_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "Lost") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) + 
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(b = -5, unit = "pt")))

overall_extras_and_batsmen_comb_pie <-
  grid.arrange(overall_runs_distribution_all_pie, overall_runs_distribution_won_pie, 
               overall_runs_distribution_lost_pie, 
               top = textGrob("Runs scored by the Teams",
                              gp=gpar(fontsize=18,font_family = "helvetica", fontface = "bold"), vjust=4.25), 
               bottom = textGrob("Composition of runs by batsmen and extras(in %)", 
                                 gp=gpar(fontsize=15,font_family = "helvetica"), vjust=-17.65), 
               nrow = 1, ncol = 3)

legend_extras_batsmen <- g_legend(ggplot(overall_runs_distribution_lost,
	aes(x = Result, y = Runs, fill = Runs_class)) +
	geom_bar(stat = "identity", width = 1) + labs(title = "All") +
	theme_mine_legend_bottom())


overall_extras_all <-
  gather(Overall_runs_ipl,
         key = `Extras_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(!Extras_class %in% c("Runs", "Runs by Batsmen", "Extras"),
         Result == "All")

overall_extras_won <-
  gather(Overall_runs_ipl,
         key = `Extras_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(!Extras_class %in% c("Runs", "Runs by Batsmen", "Extras"),
         Result == "Won")


overall_extras_lost <-
  gather(Overall_runs_ipl,
         key = `Extras_class`,
         value = `Runs`,-c(Result, Team, Matches)) %>%
  filter(!Extras_class %in% c("Runs", "Runs by Batsmen", "Extras"),
         Result == "Lost")



overall_extras_all_pie <- ggplot(overall_extras_all,
       aes(x = Result, y = Runs, fill = Extras_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "All") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) + 
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(b = -5, unit = "pt")))

overall_extras_won_pie <- ggplot(overall_extras_won,
       aes(x = Result, y = Runs, fill = Extras_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "Won") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) + 
  theme(plot.title = element_text(size = 13, face = "bold", margin = margin(b = -5, unit = "pt")))

overall_extras_lost_pie <- ggplot(overall_extras_lost,
       aes(x = Result, y = Runs, fill = Extras_class)) +
  geom_bar(stat = "identity", width = 1) + labs(title = "Lost") +
  theme_mine_no_legend() + coord_polar(theta = "y")  +
  geom_text(aes(x= 1.2, y = Runs / 2 + c(0, cumsum(Runs)[-length(Runs)]),
                label = round(Runs * 100 / sum(Runs), digits = 1))) + 
  theme(plot.title = element_text(size = 13, face = "bold",  margin = margin(b = -5, unit = "pt")))


overall_extras_comb_pie <-
		grid.arrange(overall_extras_all_pie, overall_extras_won_pie, 
			overall_extras_lost_pie, top = textGrob("Extras received by the Teams",
				gp=gpar(fontsize=18,font_family = "helvetica", fontface = "bold"), vjust=4.25), 
			bottom = textGrob("Composition of extras by category(in %)", 
			                  gp=gpar(fontsize=15,font_family = "helvetica"), vjust=-17.65),  
			nrow = 1, ncol = 3)


legend_extras <- g_legend(ggplot(overall_extras_lost,
       aes(x = Result, y = Runs, fill = Extras_class)) +
  geom_bar(stat = "identity", width = 1) +
	theme_mine_legend_bottom())
