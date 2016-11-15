
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)
library(dplyr)
library(ggthemes)
library(plotly)
library(ggiraph)
library(ggvis)
library(gganimate)
library(animation)



overall_runs_by_team_bar <- ggplot(Overall_runs_by_teams_ipl,
                                   aes(
                                     x = reorder(Team, -Runs),
                                     y = Runs,
                                     fill = Result,
                                     text = paste("Team: ", reorder(Team, -Runs))
                                   )) +
  geom_bar(position = "dodge", stat = "identity") + ggtitle("Total Runs scored by the Teams") + xlab("") + ylab("Runs") +
  coord_flip() + theme_mine_pie()

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

#team_total_matchwise
team_total_matchwise_ipl <- team_total_matchwise_ipl %>%
                              mutate(`Runs from boundaries` = ((`4s`*4)+(`6s`*6)))

match_aggregates_ipl <- batting_ipl %>%
  inner_join(match_and_score_details_ipl, by = c("match_id", "team"="Batting Team")) %>%
  group_by( `match`,`Match_no`, match_date, team1, team2, winner, match_result) %>%
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
  ) %>% arrange(`Match_no`) %>%
  select( `Match` = `match`,
         `Individual Innings`, `Runs`, `Balls`, `Not out`, `Highest`,
         `Average`, `SR`, `4s`, `6s`, `Boundaries %`,
         `Match_date` = `match_date`, `Match_no`,
         `Team1` = `team1`, `Team2` = team2, `Winner` = winner,
         `Match_Result` = match_result) %>%
  mutate(`Runs from boundaries` = ((`4s`*4)+(`6s`*6))) %>%
  mutate(`1s, 2s and 3s` = Runs - `Runs from boundaries`)

match_aggregates_1_ipl<- gather(match_aggregates_ipl,
       key = `Runs_class`,
       value = `Total`, `Runs from boundaries`, `1s, 2s and 3s`,
       -c(`Match`,
          `Individual Innings`, `Runs`, `Balls`, `Not out`, `Highest`,
          `Average`, `SR`, `4s`, `6s`, `Boundaries %`,
          `Match_date`, `Match_no`,
          `Team1`, `Team2`, `Winner`,
          `Match_Result`))




all_total_ipl_bar <- ggplot(team_total_matchwise_ipl,
       aes(
         x = `Match_no`,
         y = Runs,
         fill = `Innings`,  frame = `Match_no`, 
         cumulative = TRUE,
         text = paste(`Match`, "\n", `Team`))) +
  geom_bar_interactive(
    position = "dodge",
    stat = "identity", width = 1) +
    ggtitle("Total for every innings") + xlab("Match_no") + ylab("Runs") +
    theme_new()+
    scale_fill_economist()  

ggsave("graphs/all_total_ipl_bar.svg", 
       all_total_ipl_bar, width = 16, height = 6)

gg_animate(all_total_ipl_bar,"graphs/all_total_ipl_bar.gif", interval = .01 , 
           ani.width = 1152, ani.height = 576, loop = 1, autoplay = FALSE)


all_aggregate_ipl_bar <- ggplot(data= match_aggregates_ipl,
                        aes(x = `Match_no`,
    y = Runs,text = paste(`Match`, ":", `Team1`, "vs", Team2, Winner, Match_Result, sep = " "))) +
  geom_bar( stat = "identity", color = "coral4",
            fill = "coral") +
  geom_point(aes(x=`Match_no`, y=`Runs from boundaries`,
                 fill = "Runs from boundaries"),
             shape = 45, size =15,
              colour = "coral4")+
  # ggtitle("Match Aggregates", sub = "Runs made in every match") +
  # xlab("Match_no") + ylab("Runs") +
#  theme_wsj()
+ labs(x="Match_no", y="Runs", title="Match Aggregates",
      subtitle="Runs made in every match")+
  theme(legend.title=element_blank(),
        legend.justification=c(.47,-.5),
        legend.position=c(.47,-.5))+
  theme(plot.subtitle=element_text(size=16, hjust=0.5,
                                   face="italic", color="black"))
  # scale_color_manual(name = element_blank(),
  #                    labels = "Runs from boundaries",
  #                    values = "coral4")+
  # scale_shape_manual(values = 45)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

all_aggregate_ipl_bar + theme(axis.line = element_line(size = 0))
ggplotly(all_aggregate_ipl_bar)

all_aggregate_ipl_bar <- ggplot(data= match_aggregates_ipl,
                                aes(x = `Match_no`,
                                    y = Runs,text = paste(`Match`, ":", `Team1`, "vs", Team2, Winner, Match_Result, sep = " "))) +
  geom_bar( stat = "identity", color = "coral4") +
  geom_point(aes(x=`Match_no`, y=`Runs from boundaries`,
                 fill = "Runs from boundaries"),
             shape = 45, size =15,
             colour = "coral4")

ggplot(data= match_aggregates_1_ipl,
       aes(x = `Match_no`,
           y = `Total`, fill = `Runs_class`,
           text = paste(`Match`, ":", `Team1`, "vs", Team2, Winner, Match_Result, sep = " "))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x="Match_no", y="Runs", title="Match Aggregates",
       subtitle="Runs made in every match")+
  theme(legend.title=element_blank(),
        legend.direction = "horizontal",
        legend.justification=c(.5,-.5),
        legend.position=c(.5,-.5),
        plot.title=element_text(size=16, hjust=0.5, face = "bold", color="black"),
        plot.subtitle=element_text(size=16, hjust=0.5,
                                   face="italic", color="black"),
        plot.margin = unit(c(10,5,25,5),"mm"),
        plot.background = element_blank(),
        panel.background = element_blank()) + theme_new()+
    scale_fill_economist()

boundaries_percentage_per_match_line <- ggplot(team_total_matchwise_ipl)+
           geom_line(aes(x=`Match_no`, y=`Boundaries %`), color = "brown2")+
           geom_hline(yintercept = 60.67, color = "brown4")+
           annotate("text", x = 57, y = 80, label = "Overall: 60.67 %")+
           xlab("Match") + ylab("Boundaries Percentage") +
           ggtitle("Percentage of Runs in Boundaries per Match")+ theme_mine()+
           annotate("segment", x = 59, xend = 59, y = 78,  yend = 62,
                    colour="brown4", size=0.4, arrow=arrow(length=unit(.2, "cm")))

highest_score_in_every_match <- ggplot(team_total_matchwise_ipl)+
           geom_line(aes(x=`Match_no`, y=`Highest`), color = "brown2")+
           geom_line(aes(x=`Match_no`, y=((`4s`*4)+(`6s`*6)), color = "yellow"))+
           geom_hline(yintercept = 133, color = "brown4")+ ylim(0,160)+
           annotate("text", x = 57, y = 153, label = "Overall highest: 133")+
           xlab("Match") + ylab("Runs") +
           ggtitle("Highest score and Runs in Boundaries in every Match")+ theme_mine()+
           annotate("segment", x = 59, xend = 59, y = 150,  yend = 135,
                    colour="brown4", size=0.4, arrow=arrow(length=unit(.2, "cm")))