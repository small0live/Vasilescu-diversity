
teamSizes.proj <- (myDF %>%
                     subset(select = c("project_id", "domain", "windows", "window_idx", "num_team"))
                   )

teamSizes.other <- teamSizes.proj %>%
  subset(domain == "OTHER")

teamSizes.lib <- teamSizes.proj %>%
  subset(domain == "LIBRARY")

teamSizes.devFrame <- teamSizes.proj %>%
  subset(domain == "DEV_FRAME")

teamSizes.gui <- teamSizes.proj %>%
  subset(domain == "GUI")

teamSizes.mid <- teamSizes.proj %>%
  subset(domain == "MID_TEAR")

teamSizes.app <- teamSizes.proj %>%
  subset(domain == "APPLICATION")

teamSizes.anly <- teamSizes.proj %>%
  subset(domain == "PROGRAM_ANALYSIS")

teamSizes.db <- teamSizes.proj %>%
  subset(domain == "DB")

teamSizes.mobile <- teamSizes.proj %>%
  subset(domain == "MOBILE")

teamSizes.edu <- teamSizes.proj %>%
  subset(domain == "EDUCATION")

png(filename="teamDist-analysisOSS.png",
    width=2400, height=1000,
    units="px", res=330)
ggplot(teamSizes.anly,
       aes(x=factor(project_id),
           y=num_team)) +
  geom_boxplot(size = .25,
               outlier.size = 0.25, 
               outlier.shape = 1, 
               outlier.stroke = 0.25, 
               outlier.alpha = 1) +
  ggtitle("Distribution of Team Size for Projects") +
  labs(subtitle = "Program Analysis OSS") +
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


png(filename="teamDist-dbOSS.png",
    width=2400, height=1000,
    units="px", res=330)
ggplot(teamSizes.db,
       aes(x=factor(project_id),
           y=num_team)) +
  geom_boxplot(size = .25,
               outlier.size = 0.25, 
               outlier.shape = 1, 
               outlier.stroke = 0.25, 
               outlier.alpha = 1) +
  ggtitle("Distribution of Team Size for Projects") +
  labs(subtitle = "Database OSS") +
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

png(filename="teamDist-mobOSS.png",
    width=2400, height=1000,
    units="px", res=330)
ggplot(teamSizes.mobile,
       aes(x=factor(project_id),
           y=num_team)) +
  geom_boxplot(size = .25,
               outlier.size = 0.25, 
               outlier.shape = 1, 
               outlier.stroke = 0.25, 
               outlier.alpha = 1) +
  ggtitle("Distribution of Team Size for Projects") +
  labs(subtitle = "Mobile OSS") +
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

png(filename="teamDist-eduOSS.png",
    width=2400, height=1000,
    units="px", res=330)
ggplot(teamSizes.edu,
       aes(x=factor(project_id),
           y=num_team)) +
  geom_boxplot(size = .25,
               outlier.size = 0.25, 
               outlier.shape = 1, 
               outlier.stroke = 0.25, 
               outlier.alpha = 1) +
  ggtitle("Distribution of Team Size for Projects") +
  labs(subtitle = "Education OSS") +
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

ggplot(teamSizes.devFrame,
       aes(x = window_idx,
           group = factor(project_id),
           color = factor(project_id),
           y = num_team)) +
  geom_line(size = .25) +
  labs(title="Team Size per Quarter", 
       subtitle="Each line represents a project") +
  xlab("Quarter") +
  ylab("Team Size") +
  #scale_color_brewer(palette ="Set3") +
  theme(legend.position = "none",
    panel.background=element_rect(fill = "gray5", 
                                  color = "slategray"),
    panel.grid.minor = element_line(color = "gray15"),
    panel.grid.major = element_line(color = "gray15")  
  )
  