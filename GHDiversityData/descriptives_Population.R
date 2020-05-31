## GitHub Diversity Data Set
## For CSCW 2020
## Olivia B. Newton
## Created March 2020



# Load Packages -----------------------------------------------------------

library(data.table) 
library(ggplot2)
library(ggpubr)
library(magrittr)
library(scales)
library(tidyverse)

# Import Data -------------------------------------------------------------

setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

#myDF <- fread("wmxn_user_data.csv") 
myDF <- fread("user_data.csv") 

#myDF <- myDF %>%
#  subset(language != "None")

# Team Size Descriptives --------------------------------------------------

#myDF <- myDF %>%
#  mutate(teamSize =  ## team size bins: 2	3-5	6-9	10+
#           ifelse(num_team <= 2, "2",
#                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
#                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
#                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
#                                ))))#,
         #commitSize = ## project size bins: 1-10 11-100 101-500 500-1000 1001-5000 5000-10000 10001-15000 15001-25000
         #  ifelse(num_commits < 11, 10,
         #         ifelse(num_commits > 10 & num_commits < 101, 100,
         #                ifelse(num_commits > 100 & num_commits < 501, 500,
         #                       ifelse(num_commits > 500 & num_commits < 1001, 1000,
         #                              ifelse(num_commits > 1000 & num_commits < 5001, 5000,
         #                                     ifelse(num_commits > 5000 & num_commits < 10001, 10000,
         #                                            ifelse(num_commits > 10000 & num_commits < 15001, 15000, 25000
         #                                            )))))))
         #)
         
teamSizes <- myDF %>%
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize")) %>%
  unique() %>%
  group_by(teamSize) %>%
  summarise("Count" = n())

teamSizes$teamSize <- factor(teamSizes$teamSize, 
                             levels = c("2", 
                                        "3 to 5",
                                        "6 to 9", 
                                        "10 to 15", 
                                        "16 or more"))

ggplot(teamSizes,
       aes(x = teamSize, y = Count)) +
  geom_bar(stat = "identity",
           fill = "palegreen4",
           color = "gray28",
           alpha = .95,
           lwd = .2) +
  ggtitle("Distribution of Project Team Size") +
  labs(subtitle = "Multiple data points for each of the 5,539 teams") +
  ylab("Number of Teams") +
  xlab("Team Size") +
  theme(
    panel.background = element_rect(fill = "white", color = "slategray"), 
    panel.grid.major = element_line(color = "gray88"),
    panel.grid.minor = element_line(color = "gray88"),
    panel.grid.major.x = element_blank()
    )



# Gender Descriptives -----------------------------------------------------

genderCounts <- myDF %>%
  subset(select = c("user_ID",
                    "gender")) %>%
  distinct(user_ID, gender) %>%
  #distinct(user_ID, gender, .keep_all = TRUE) %>%
  #unique() %>%
  group_by(gender) %>%
  summarise(Count = n()) %>%
  mutate(Percent = percent(Count/sum(Count)))

## just captilizing these
genderCounts[1,1] <- "Female"
genderCounts[2,1] <- "Male"

## none is an inaccurate label
genderCounts[3,1] <- "Unknown"

ggplot(genderCounts,
       aes(x = gender,
           y = Count)) +
  geom_bar(stat = "identity", 
           lwd = .2, 
           color = "gray28", 
           fill = "goldenrod3", 
           alpha =.98) +
  ggtitle("GitHub User Gender") +
  xlab("") +
  ylab("Number of Users") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y = element_line(color = "gray88"),
        panel.grid.major.y = element_line(color ="gray88")) +
  geom_label(aes(label = Percent), data = genderCounts)


# Platform Tenure Descriptives/Plots --------------------------------------------

tenure <- myDF %>%
  subset(select = c("project_id", 
                    "window_idx", 
                    "user_ID", 
                    "github_tenure",
                    "gender")) %>%
  group_by(user_ID) %>%
  top_n(1, github_tenure) %>%
  distinct(user_ID, .keep_all = TRUE) %>%
  subset(select = c("user_ID", "github_tenure", "gender"))

png(filename="platformTenure_allUsers.png",
    width=1500, height=1000,
    units="px", res=330)
ggplot(tenure,
       aes(x = gender,
           y = github_tenure,
           group = gender)) +
  geom_boxplot(lwd = .5, 
               color = "gray28",
               fill = "goldenrod3", 
               alpha = .9,
               notch = T,
               outlier.size = 3,
               outlier.shape = 5,
               outlier.color = "black") +
  #ggtitle("GitHub Tenure Distributions") +
  xlab("User Gender") +
  ylab("Tenure (days)") +
  scale_x_discrete(labels = c("female" = "Female",
                              "male" = "Male",
                              "None" = "Unknown")) +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_line(color="gray88"))
dev.off()


ftenureCounts <- tenure %>%
  subset(gender == "female")

mtenureCounts <- tenure %>%
  subset(gender == "male")

idktenureCounts <- tenure %>%
  subset(gender == "None")

png(filename="platformTenure_femaleUsersQQ.png")
qqnorm(ftenureCounts$github_tenure, pch = 1, frame = FALSE, main = "Normal Q-Q Plot \nFemale Users")
qqline(ftenureCounts$github_tenure, col = "goldenrod", lwd = 2)
dev.off()

png(filename="platformTenure_maleUsersQQ.png")
qqnorm(mtenureCounts$github_tenure, pch = 1, frame = FALSE, main = "Normal Q-Q Plot \nMale Users")
qqline(mtenureCounts$github_tenure, col = "goldenrod", lwd = 2)
dev.off()

png(filename="platformTenure_unknownUsersQQ.png")
qqnorm(idktenureCounts$github_tenure, pch = 1, frame = FALSE, main = "Normal Q-Q Plot \nUnknown Gender Users")
qqline(idktenureCounts$github_tenure, col = "goldenrod", lwd = 2)
dev.off()

#png(filename="platformTenure_femaleUsers.png",
#    width=1500, height=1000,
#    units="px", res=330)
laydees <- ggplot(ftenureCounts,
       aes(x = github_tenure)) +
  geom_histogram(binwidth = 30,
                 lwd = .2, 
                 color = "gray28",
                 fill = "goldenrod3", 
                 alpha = .8) + 
  ggtitle("Women") +
  #labs(subtitle = "5,524 unique accounts identified as female") +
  xlab("Tenure (in days)") +
  ylab("Number of Users") +
  theme(
    text = element_text(family = "mono"),
    legend.position = "none",
    panel.background=element_rect(fill = "white", color = "slategray"),
    panel.grid.minor.y=element_line(color="gray88"),
    panel.grid.major.y=element_line(color="gray88"))
#dev.off()
#
#png(filename="platformTenure_maleUsers.png",
#    width=1500, height=1000,
#    units="px", res=330)
mens <- ggplot(mtenureCounts,
       aes(x = github_tenure)) +
  geom_histogram(binwidth = 30,
                 lwd = .2, 
                 color = "gray28",
                 fill = "goldenrod3", 
                 alpha = .8) + 
  ggtitle("Men") +
  #labs(subtitle = "58,216 unique accounts identified as male") +
  #xlab("Tenure (in days)") +
  xlab("") +
  ylab("Number of Users") +
  theme(
    text = element_text(family = "mono"),
    legend.position = "none",
    panel.background=element_rect(fill = "white", color = "slategray"),
    panel.grid.minor.y=element_line(color="gray88"),
    panel.grid.major.y=element_line(color="gray88"))
#dev.off()

#png(filename="platformTenure_unknownUsers.png",
#    width=1500, height=1000,
#    units="px", res=330)
whodat <- ggplot(idktenureCounts,
       aes(x = github_tenure)) +
  geom_histogram(binwidth = 30,
                 lwd = .2, 
                 color = "gray28",
                 fill = "goldenrod3", 
                 alpha = .8) + 
  ggtitle("Unknown Gender Contributors") +
  #labs(subtitle = "15,056 unique accounts with no specified or identifiable gender") +
  xlab("Tenure (in days)") +
  ylab("Number of Users") +
  theme(text = element_text(family = "mono"),
    legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88"))
#dev.off()

fig <- ggarrange(laydees + rremove("xlab"), mens + rremove("ylab"), whodat + rremove("ylab") + rremove("xlab"),
         labels = c("A", "B", "C"),
         nrow = 1,
         align = "h")

png(filename="platformTenure_allGroups.png",
    width=3400, height=990,
    units="px", res=330)
annotate_figure(fig,
                bottom = text_grob("GitHub Tenure (days)",
                                   family = "mono"))
dev.off()
# Platform Tenure Correlation ---------------------------------------------


# Project Age + Domain Descriptives/Plots -------------------------------------------------------------

ages <- myDF %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "project_age")) %>%
  group_by(project_id) %>%
  top_n(1, window_idx) %>%
  distinct(project_id, .keep_all = TRUE)

ggplot(ages,
       aes(x = project_age)) + ## project age is in quarters
  geom_histogram(binwidth = 1) ## 1 = 3 month interval; project with value of 1 is 3 mos old, 2 is 6 mos old, etc.

domains <- myDF %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "domain")) %>%
  group_by(project_id) %>%
  top_n(1, window_idx) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(domain) %>%
  summarise(count = n())

ggplot(domains,
       aes(x = factor(domain),
           y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30))
  


# Platform Turnover Plot Descriptives --------------------------------------------

turnover <- myDF %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "domain",
                    "teamSize",
                    "turnover")) %>%
  group_by(project_id) %>%
  #top_n(1, window_idx) %>%
  distinct(window_idx, .keep_all = TRUE) %>%
  mutate(initialTeamSize = ifelse(window_idx == min(window_idx), paste0(teamSize), NA))


turnoverLQ <- myDF %>%
  subset(select = c("project_id", 
                    "window_idx", 
                    "user_ID", 
                    "turnover")) %>%
  group_by(project_id) %>%
  top_n(1, window_idx) %>%
  distinct(window_idx, .keep_all = TRUE) %>% ## get rid of duplicates at the project level
  subset(select = c("project_id", "window_idx", "turnover"))


#png(filename="platformTenure_femaleUsersQQ.png")
qqnorm(turnover$turnover, pch = 1, frame = FALSE, main = "Normal Q-Q Plot")
qqline(turnover$turnover, col = "goldenrod", lwd = 2)
#dev.off()

ggplot(turnover,
       aes(x = window_idx,
           y = turnover,
           group = project_id,
           color = project_id)) +
  geom_line() +
  theme(
    legend.position = "none")

ggplot(turnoverLQ,
       aes(x = turnover)) +
  geom_histogram(#binwidth = 30,
                 lwd = .2, 
                 color = "gray28",
                 fill = "goldenrod3", 
                 alpha = .8) + 
  ggtitle("Turnover in Last Project Quarter") +
  xlab("Turnover (percent)") +
  ylab("Number of Projects") +
  theme(
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88"))


## MAYbe don't need seperate data frames
## consider using GGPairs???

turnoverGRP1 <- turnover %>%
  subset(domain == "OTHER")

turnoverGRP2 <- turnover %>%
  subset(domain == "DEV_FRAME")

turnoverGRP3 <- turnover %>%
  subset(domain == "LIBRARY")

turnoverGRP4 <- turnover %>%
  subset(domain == "GUI")

turnoverGRP5 <- turnover %>%
  subset(domain == "MID_TEAR")

turnoverGRP6 <- turnover %>%
  subset(domain == "WEB")

turnoverGRP7 <- turnover %>%
  subset(domain == "APPLICATION")

turnoverGRP8 <- turnover %>%
  subset(domain == "PROGRAM_ANALYSIS")

turnoverGRP9 <- turnover %>%
  subset(domain == "DB")

turnoverGRP10 <- turnover %>%
  subset(domain == "EDUCATION")

turnoverGRP11 <- turnover %>%
  subset(domain == "MOBILE")


ggplot(turnoverGRP6,
       aes(x = window_idx,
           y = turnover,
           group = factor(project_id),
           color = factor(project_id))) +
  geom_line() +
  theme(
    legend.position = "none")


# Project Programming Language ---------------------------------------------------------

lang <- myDF %>%
  subset(select = c("project_id",
                    "language")) %>%
  unique() %>%
  group_by(language) %>%
  summarise("Count" = n())

# Project Similarity ------------------------------------------------------



  
  


