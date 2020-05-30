library("data.table")
library(dplyr)
library(ggplot2)
library(magrittr)
myDF <- fread("diversity_data.csv",
              sep = ";")

teamSizes.proj <- (myDF %>%
                     subset(select = c("project_id", "domain", "windows", "window_idx", "num_team"))
)

sizes <- (teamSizes.proj %>%
            mutate(sizeClass=
                     ifelse(num_team < 11, "small", 
                            ifelse(num_team > 10 & num_team < 30, "medium", 
                                   "large"
                                   ))))

size.sums <- sizes %>%
  group_by(domain, sizeClass) %>%
  summarise(Count = n())

#small.teams <- size.sums %>%
#  subset(sizeClass == 1)


ggplot(size.sums,
       aes(x=sizeClass,
           group=reorder(domain, -Count),
           fill=reorder(domain, -Count),
           y=Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Project Sizes") +
  xlab("Team Size") +
  ylab("number of projects") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(name ="Team Size", 
                   limits=c("small","medium","large")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
        )

                  