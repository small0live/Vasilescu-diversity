

# Load Packages -----------------------------------------------------------


library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(scales)


# Load and Subset Data ----------------------------------------------------


gender <- fread("user_data.csv") %>%
  subset(select = c("user_ID", "gender")) %>%
  unique() %>%
  group_by(gender) %>%
  summarise(Count=n()) %>%
  mutate(Percent = percent(Count/sum(Count)))


# Plot --------------------------------------------------------------------

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
