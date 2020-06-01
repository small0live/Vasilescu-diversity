# Load Packages -----------------------------------------------------------

library(data.table) 
library(ggplot2)
library(magrittr)
library(tidyverse)

# Import Data -------------------------------------------------------------

setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

menlist <- fread("NoWomanTeams_SmartSampled.csv")

menlist <- unique(menlist$project_id)

myDF <- fread("user_data.csv") %>%
  subset(project_id %in% menlist)


# Create Functions ---------------------------------------------------------

`%not_in%` <- purrr::negate(`%in%`) 

# Project Programming Language ---------------------------------------------------------

lang <- myDF %>%
  subset(select = c("project_id",
                    "language")) %>%
  unique() %>%
  group_by(language) %>%
  summarise("Count" = n()) 

lang <- lang[order(-lang$Count),]

topTenLang <- head(lang$language, n = 10)

lang$language[lang$language %not_in% topTenLang] <- "Other languages"

lang <- lang %>%
  group_by(language) %>%
  summarise("Count" = sum(Count)) 

lang <- lang[order(-lang$Count),]

lang <- lang[c(1:2,4:10,3),]

lang$language <- factor(lang$language, levels = lang$language)

#png(filename="barPlots_mixedGenderProjects_Language.png",
#    width=1500, height=1000,
#    units="px", res=330)
langPlot <- ggplot(lang, 
                   aes(x = factor(language),
                       y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(lang$language))) +
  ggtitle("Programming Language") +
  ylab("") +
  xlab("") +
  theme(panel.background=element_rect(fill = "white", 
                                      color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()




# Project Contributors (12mos) ----------------------------------------------------

contributors <- myDF %>% 
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx)) %>% 
  subset(window_idx > (maxWI - 4)) %>% 
  mutate(num_team_year = length(unique(user_ID))) %>%
  distinct(project_id, .keep_all = TRUE) %>% 
  select(num_team_year) %>%
  ungroup() %>%
  mutate(contributors =  
           ifelse(num_team_year <= 2, "2",
                  ifelse(num_team_year == 3, "3",
                         ifelse(num_team_year == 4, "4",
                                ifelse(num_team_year == 5, "5",
                                       ifelse(num_team_year == 6, "6",
                                              ifelse(num_team_year == 7, "7",
                                                     ifelse(num_team_year == 8, "8",
                                                            ifelse(num_team_year == 9, "9",
                                                                   ifelse(num_team_year > 9 & num_team_year < 21, "10 to 20",
                                                                          ifelse(num_team_year > 20 & num_team_year < 51, "21 to 50",
                                                                                 ifelse(num_team_year > 50 & num_team_year < 101, "51 to 100",
                                                                                        ifelse(num_team_year > 100 & num_team_year < 501, "101 to 500", "> 500"
                                                                                        ))))))))))))) %>%
  group_by(contributors) %>%
  summarise("Count" = n()) 

contributors <- contributors[c(2,4:6,8:11,1,3,7),]

contributors$contributors <- factor(contributors$contributors, levels = contributors$contributors)

#png(filename="barPlots_mixedGenderProjects_12moContributors.png",
#    width=1500, height=1000,
#    units="px", res=330)
teamPlot <- ggplot(contributors, 
                   aes(x = factor(contributors),
                       y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(contributors$contributors))) +
  ggtitle("Contributors (12 mos)") +
  ylab("") +
  xlab("") +
  theme(panel.background=element_rect(fill = "white", 
                                      color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()




# Project Commits (12mos) -------------------------------------------------

commits <- myDF %>% 
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx)) %>% 
  subset(window_idx > (maxWI - 4)) %>% 
  mutate(num_commits_year = sum(num_commits)) %>%
  distinct(project_id, .keep_all = TRUE) %>% 
  select(num_commits_year) %>%
  ungroup() %>%
  mutate(commits =  
           ifelse(num_commits_year <= 50, "1-50",
                  ifelse(num_commits_year > 50 & num_commits_year < 101, "51-100",
                         ifelse(num_commits_year > 100 & num_commits_year < 151, "101-150",
                                ifelse(num_commits_year > 150 & num_commits_year < 201, "151-200",
                                       ifelse(num_commits_year > 200 & num_commits_year < 251, "201-250",
                                              ifelse(num_commits_year > 250 & num_commits_year < 301, "251-300",
                                                     ifelse(num_commits_year > 300 & num_commits_year < 351, "301-350",
                                                            ifelse(num_commits_year > 350 & num_commits_year < 1001, "351-1000",
                                                                   ifelse(num_commits_year > 1000 & num_commits_year < 5001, "1001-5000", ">5000"
                                                                   )))))))))) %>%
  group_by(commits) %>%
  summarise("Count" = n()) 

commits <- commits[c(2,10,4:9,3,1),]

commits$commits <- factor(commits$commits, levels = commits$commits)

#png(filename="barPlots_mixedGenderProjects_12moCommits.png",
#    width=1500, height=1000,
#    units="px", res=330)
commitPlot <- ggplot(commits, 
                     aes(x = commits,
                         y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(commits$commits))) +
  ggtitle("Commits (12 mos)") +
  ylab("") +
  xlab("") +
  theme(panel.background=element_rect(fill = "white", 
                                      color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()


# Forks -------------------------------------------------------------------

forks <- myDF %>%
  select(project_id, forks) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(num_forks =  
           ifelse(forks <= 5, "1-5",
                  ifelse(forks > 5 & forks < 11, "6-10",
                         ifelse(forks > 10 & forks < 16, "11-15",
                                ifelse(forks > 15 & forks < 21, "16-20",
                                       ifelse(forks > 20 & forks < 26, "21-25",
                                              ifelse(forks > 25 & forks < 301, "26-30",
                                                     ifelse(forks > 30 & forks < 36, "31-35",
                                                            ifelse(forks > 35 & forks < 1001, "36-40",">40"
                                                            ))))))))) %>%
  
  group_by(num_forks) %>%
  summarise("Count" = n()) 

forks <- forks[c(1,7,2:6),]

forks$num_forks <- factor(forks$num_forks, levels = forks$num_forks)

#png(filename="barPlots_mixedGenderProjects_Forks.png",
#    width=1500, height=1000,
#    units="px", res=330)
forkPlot <- ggplot(forks, 
                   aes(x = num_forks,
                       y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(forks$num_forks))) +
  ggtitle("Forks") +
  ylab("") +
  xlab("") +
  theme(panel.background=element_rect(fill = "white", 
                                      color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()



# Watchers ----------------------------------------------------------------

watchers <- myDF %>%
  select(project_id, watchers) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(num_watch =  
           ifelse(watchers <= 5, "1-5",
                  ifelse(watchers > 5 & watchers < 11, "6-10",
                         ifelse(watchers > 10 & watchers < 16, "11-15",
                                ifelse(watchers > 15 & watchers < 21, "16-20",
                                       ifelse(watchers > 20 & watchers < 26, "21-25",
                                              ifelse(watchers > 25 & watchers < 301, "26-30",
                                                     ifelse(watchers > 30 & watchers < 36, "31-35",
                                                            ifelse(watchers > 35 & watchers < 1001, "36-40",">40"
                                                            ))))))))) %>%
  
  group_by(num_watch) %>%
  summarise("Count" = n()) 

watchers <- watchers[c(2,8,3:7,1),]

watchers$num_watch <- factor(watchers$num_watch, levels = watchers$num_watch)


#png(filename="barPlots_mixedGenderProjects_Watchers.png",
#    width=1500, height=1000,
#    units="px", res=330)
watchPlot <- ggplot(watchers, 
                    aes(x = num_watch,
                        y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(watchers$num_watch))) +
  ggtitle("Watchers") +
  ylab("") +
  xlab("") +
  theme(panel.background=element_rect(fill = "white", 
                                      color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()



# Project Age -------------------------------------------------------------

projectAge <- myDF %>%
  select(project_id, project_age) %>%
  distinct(project_id, .keep_all = TRUE) 

projectAge$project_age[projectAge$project_age == 0] <- "<=1"
projectAge$project_age[projectAge$project_age == 1] <- "<=1"

projectAge <- projectAge %>%
  group_by(project_age) %>%
  summarise("Count" = n()) 

projectAge <- projectAge[c(1,12,18:24,2:11,13:17),]

projectAge$project_age <- factor(projectAge$project_age, levels = projectAge$project_age)

#png(filename="barPlots_mixedGenderProjects_Age.png",
#    width=1500, height=1000,
#    units="px", res=330)
agePlot <- ggplot(projectAge, 
                  aes(x = project_age,
                      y = Count)) +
  geom_bar(stat = "identity",
           lwd = .2, 
           color = "gray28",
           fill = "goldenrod3", 
           alpha = .9) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(projectAge$project_age))) +
  ggtitle("Project Age") +
  ylab("") +
  xlab("") +
  theme(panel.background = element_rect(fill = "white", 
                                        color = "slategray"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color="gray88")) 
#dev.off()

# Arrange Plots Together --------------------------------------------------

png(filename="universeDimensionsDist_noWmxn1.png",
    width=5400, height=1200,
    units="px", res=330)
ggarrange(langPlot, teamPlot, commitPlot,
          ncol = 3,
          #nrow = 2,
          align = "h")
dev.off()

png(filename="universeDimensionsDist_noWmxn2.png",
    width=5400, height=1200,
    units="px", res=330)
ggarrange(forkPlot, watchPlot, agePlot,
          ncol = 3,
          #nrow = 2,
          align = "h")
dev.off()

