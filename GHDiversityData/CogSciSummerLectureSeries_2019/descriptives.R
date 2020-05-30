## Diversity Data Set for GitHub Projects
## Vasilescu et al. 2015
## Olivia Newton

# Load Packages -----------------------------------------------------------

library(ggplot2)
library(magrittr)
library(purrr)
library(RColorBrewer)
library(skimr) ## using skim
library(tidyverse)

# Import Data -------------------------------------------------------------

setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

myDF <- read.csv("diversity_data.csv",
                 sep = ";")


# Inspect Data ------------------------------------------------------------

# skim(myDF)

# Subset and Smmarize Data ----------------------------------------------------------

## used for overall distribution of team size
teamSizes <- (myDF %>%
                 group_by(num_team) %>%
                 summarise("Count"=n())
             )


## used for visualizing changes in team size over time
teamSizes.proj <- (myDF %>%
                       subset(select = c("project_id", 
                                         "domain", 
                                         "windows", 
                                         "window_idx", 
                                         "num_team"))
                     )

teamSizes.five <- (teamSizes.proj %>%
                     subset(windows <= 5 & num_team )
                   )

teamSizes.ten <- (teamSizes.proj %>%
                     subset(windows > 5 & windows <= 10)
                  )

teamSizes.fif <- (teamSizes.proj %>%
                    subset(windows > 10 & windows <= 15)
                  )

teamSizes.dub <- (teamSizes.proj %>%
                    subset(windows > 15 & windows <= 20)
                  )

teamSizes.dfour <- (teamSizes.proj %>%
                    subset(windows > 20 & windows <= 24)
                  )


# Project Descriptives ------------------------------------------------------------

## only keep high-level project information
descriptive <- (myDF %>%
                  subset(select = c("project_id", 
                                    "domain", 
                                    "language", 
                                    "windows"))
                )

## remove duplicates
descriptive <- descriptive[!duplicated(descriptive[1:4]),]

## get counts
domains <- descriptive %>%
  group_by(domain) %>%
  summarise(Count = n())

lang <- descriptive %>%
  group_by(language) %>%
  summarise(Count = n())

windows <- descriptive %>%
  group_by(windows) %>%
  summarise(Count = n())

## create plot and save image
png(filename="domainDistribution.png",
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300) 
ggplot(domains,
       aes(x=reorder(domain, -Count),
           fill=reorder(domain, -Count),
           y=Count)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Distribution of Projects by Domain") +
  labs(subtitle = "23,493 Projects") +
  xlab("Domain") +
  ylab("number of projects") +
  theme(legend.position = "none",
        text = element_text(size=9),
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88"),
        axis.text.x = element_text(angle=30)
        )
dev.off()


new.color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
new.color %<>% discard(~ .x %in% c('white','aliceblue','antiquewhite',
                                   'antiquewhite1',
                                   'antiquewhite2',
                                   'antiquewhite3',
                                   'antiquewhite4'))

new.color <- new.color %>% sample()
 
png(filename="langDistribution.png",
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300) 
ggplot(lang,
       aes(x=reorder(language, -Count),
           fill=reorder(language, -Count),
           y=Count)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribution of Projects by Programming Language") +
  labs(subtitle = "23,493 Projects") +
  xlab("Language") +
  ylab("number of projects") +
  scale_fill_manual(values = new.color) +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88"),
        axis.text.x = element_text(angle=90, size = 5)
  )
dev.off()

png(filename="windowsDist.png",
    width=1800, height=1200,
    units="px", res=330)
ggplot(windows,
       aes(x=windows,
           y=Count)) +
  geom_bar(stat = "identity") +
  ggtitle("Project Age") +
  labs(subtitle = "23,493 Projects") +
  xlab("Quarters") +
  ylab("number of projects") +
  #scale_fill_manual(values = new.color) +
  scale_x_continuous(breaks=seq(0, 25, by=2)) +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
        #axis.text.x = element_text(angle=90)
  )
dev.off()

# Team Size Plots -------------------------------------------------------------------

png(filename="overallTeamSizesDist.png",
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300) 
ggplot(teamSizes, 
       aes(x=num_team)) +
  geom_histogram(aes(y=..density..), 
    binwidth = 10, lwd=.1, color = "gray28", fill = "palegreen4", alpha=.95) +
  geom_density(fill="lightgray", alpha =.3) +
  #geom_bar(stat = "identity", lwd=.2, color = "gray28", fill = "palegreen4", alpha=.95) +
  ggtitle("Team Size Distribution") +
  labs(subtitle = "binwidth = 10") +
  ylab("density") +
  xlab("Team Size") +
  #scale_y_continuous(breaks=seq(0, 12, by=2)) +
  theme(
    panel.background=element_rect(fill="white", color="slategray"), 
    panel.grid.major=element_line(color="gray88"),
    panel.grid.minor=element_line(color="gray88")
    )
dev.off()


png("barchart-Gender.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)        # smaller font size
ggplot(gender,
       aes(x=gender,
           y=Count)) +
  geom_bar(stat = "identity", lwd=.2, color = "gray28", fill = "palegreen4", alpha=.95) +
  #lwd =.2, color ="gray28",fill="forestgreen", alpha=.9) +
  ggtitle("Distribution of Gender (Overall)") +
  xlab("") +
  ylab("number of users") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
  )
dev.off()

ggplot(teamSizes.dfour,
       aes(x = window_idx,
           group = factor(project_id),
           color = domain,
           y = num_team)) +
  geom_line(size = .25) +
  labs(title="Team Size per Quarter", 
       subtitle="Each line represents a project") +
  xlab("Quarter") +
  ylab("Team Size") +
  scale_color_brewer(palette ="Set3") +
  theme(#legend.position = "none",
        panel.background=element_rect(fill = "gray5", 
                                      color = "slategray"),
        panel.grid.minor = element_line(color = "gray15"),
        panel.grid.major = element_line(color = "gray15")  
  )
#dev.off()

png(filename="teamDist-3yrs.png",
    width=2400, height=1000,
    units="px", res=330)
ggplot(teamSizes.fif,
       aes(x=factor(project_id),
           fill=domain,
           y=num_team)) +
  geom_boxplot(size = .25,
    outlier.size = 0.25, 
               outlier.shape = 1, 
               outlier.stroke = 0.25, 
               outlier.alpha = 1) +
  ggtitle("Distribution of Team Size for Projects (2.5-3.5 years old)") +
  ylab("Team Size") +
  xlab("") +
  theme(
    title = element_text(size = 7),
    legend.title = element_blank(),
    legend.text = element_text(size = 5),
    legend.position = "bottom",
    legend.key.size = unit(0.3, "cm"),
    axis.title.y = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background=element_rect(fill="white", color="slategray"), 
    panel.grid.major=element_line(color="gray88"),
    panel.grid.minor=element_line(color="gray88"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
    
  )
dev.off()

# Commit Tenure -----------------------------------------------------------


# Gender ------------------------------------------------------------------


