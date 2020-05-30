library(DescTools)

long.term <- kitchen.sink %>%
  subset(windows > 20, select = c("project_id", 
                                  "windows", 
                                  "window_idx", 
                                  "domain", 
                                  "language", 
                                  "blau_gender", 
                                  "num_team",
                                  "github_tenures")) %>%
  group_by(project_id, window_idx) %>%
  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
  ungroup() %>%
  subset(select = c("project_id", 
                    "windows", 
                    "window_idx", 
                    "domain", 
                    "language", 
                    "blau_gender", 
                    "num_team",
                    "Gini_gh_ten")) %>%
  unique()

diverse <- long.term %>%
  subset(project_id == 5591 | project_id == 24293 | project_id == 104697 | project_id == 1088) %>%
  na.omit()


#python.db <- long.term %>%
#  subset(project_id == 24293) %>%
#  na.omit()


png("line_plot_Diverse.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)  
ggplot(diverse, 
       aes(x=window_idx,
           color=as.factor(language),
           y=blau_gender)) +
  geom_line() +
  #ggtitle("Diversity in 4 Projects") +
  labs(color = "Language") +
  xlab("Quarter") +
  ylab("Blau Index (gender diversity)") +
  theme(#legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
  )
dev.off()


png("scatter_Diverse.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)  
ggplot(diverse, 
       aes(x=Gini_gh_ten,
           color=as.factor(language),
           y=blau_gender)) +
  geom_point(size = 2) +
  #ggtitle("Diversity in 4 Projects") +
  labs(color = "Language") +
  xlab("Gini (tenure disparity)") +
  ylab("Blau Index (gender diversity)") +
  theme(#legend.position = "none",
    panel.background=element_rect(fill = "white", color = "slategray"),
    panel.grid.minor.y=element_line(color="gray88"),
    panel.grid.major.y=element_line(color="gray88")
  )
dev.off()

# png("line_plot_python.png",    # create PNG for the heat map        
#     width = 5*600,        # 5 x 300 pixels
#     height = 5*300,
#     res = 300)  
# ggplot(python.db, 
#        aes(x=window_idx,
#            fill = as.factor(language),
#            y=blau_gender)) +
#   geom_line(color="forestgreen") +
#   ggtitle("Gender Diversity in a Translation Tool OSS") +
#   labs("Python Programming Language") +
#   xlab("Quarter") +
#   ylab("Blau Index (gender diversity)") +
#   theme(legend.position = "none",
#         panel.background=element_rect(fill = "white", color = "slategray"),
#         panel.grid.minor.y=element_line(color="gray88"),
#         panel.grid.major.y=element_line(color="gray88")
#   )
# dev.off()
