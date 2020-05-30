

GHtenure <- kitchen.sink %>%
  subset(window_idx == windows, 
         select = c("project_id", 
                    "window_idx", 
                    "user_ID", 
                    "github_tenures"))

GHtenures <- GHtenure[!duplicated(GHtenure[3:4]),]

GHtenures$github_tenures <- as.numeric(GHtenures$github_tenures)

png("histogram-GHtenure.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)        # smaller font size
ggplot(GHtenures,
       aes(x=github_tenures)) +
  geom_histogram(aes(y=..density..), 
                 lwd =.2, color ="gray28",fill="forestgreen", alpha=.9) +
  geom_density(fill="lightgray", alpha =.3) +
  #geom_bar(stat = "identity", fill = "darkslateblue") +
  ggtitle("Distribution of GitHub Tenure (Overall)") +
  xlab("Tenure (in days)") +
  ylab("density of users") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
  )
dev.off()
