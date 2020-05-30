

library(ggplot2)
library(magrittr)

myDF <- read.csv("diversity_data.csv",
                 sep = ";")


gender <- myDF %>%
  subset(select = c("project_id", 
                    "domain", 
                    "language",
                    "windows", 
                    "window_idx",
                    "num_team",
                    "female",
                    "has_woman",
                    "blau_gender"))

app <- gender %>%
  subset(domain == "APPLICATION")

other <- gender %>%
  subset(domain == "OTHER")

libOSS <- gender %>%
  subset(domain == "LIBRARY")

gui <- gender %>%
  subset(domain == "GUI")


ggplot(gui,
       aes(x = window_idx,
           group = factor(project_id),
           color = factor(project_id),,
           y = female)) +
  geom_line(size = .25) +
  labs(title="Women in GUI OSS Projects" 
       subtitle="Each line represents a project") +
  xlab("Quarter") +
  ylab("Number of Women") +
  #scale_color_manual(values = coul) +
  #scale_color_brewer(palette ="Paired") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "gray5", 
                                      color = "slategray"),
        panel.grid.minor = element_line(color = "gray15"),
        panel.grid.major = element_line(color = "gray15")  
  )

ggplot(test,
       aes(x = window_idx,
           group = factor(project_id),
           color = factor(project_id),
           y = blau_gender)) +
  geom_line(size = .25) + 
  #position=position_jitter(w=0.02, h=0)) 
  labs(title="Gender Diversity in GUI OSS Projects", 
  #     subtitle="Each line represents a project") +
  xlab("Quarter") +
  ylab("Gender Diversity (Blau Index)") +
  #scale_color_manual(values = new.color) +
  theme(legend.position = "none",
    panel.background=element_rect(fill = "gray5", 
                                  color = "slategray"),
    panel.grid.minor = element_line(color = "gray15"),
    panel.grid.major = element_line(color = "gray15")  
  )
