
genBYten <- kitchen.sink %>%
  subset(gender != "None", select = c("user_ID",
                                      "gender",
                                      "github_tenures"
                                      ))

genBYten <- unique(genBYten)

men <- genBYten %>%
  subset(gender == "male")

women <- genBYten %>%
  subset(gender == "female")

none <- kitchen.sink %>%
  subset(gender == "None", select = c("user_ID",
                                      "gender",
                                      "github_tenures")
         ) %>%
  unique()

men$github_tenures <- as.numeric(men$github_tenures)
women$github_tenures <- as.numeric(women$github_tenures)

png("histogram-GHtenurMEN.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)        # smaller font size
ggplot(men,
       aes(x=github_tenures)) +
  geom_histogram(aes(y=..density..), 
                 lwd =.2, color ="gray28",fill="forestgreen", alpha=.9) +
  geom_density(fill="lightgray", alpha =.3) +
  #geom_bar(stat = "identity", fill = "darkslateblue") +
  ggtitle("Distribution of GitHub Tenure (Men)") +
  xlab("Tenure (in days)") +
  ylab("density of users") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
  )
dev.off()

png("histogram-GHtenurWOMEN.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)        # smaller font size
ggplot(women,
       aes(x=github_tenures)) +
  geom_histogram(aes(y=..density..), 
                 lwd =.2, color ="gray28",fill="forestgreen", alpha=.9) +
  #geom_density(fill="lightgray", alpha =.3) +
  #geom_bar(stat = "identity", fill = "darkslateblue") +
  ggtitle("Distribution of GitHub Tenure (Women)") +
  xlab("Tenure (in days)") +
  ylab("density of users") +
  theme(legend.position = "none",
        panel.background=element_rect(fill = "white", color = "slategray"),
        panel.grid.minor.y=element_line(color="gray88"),
        panel.grid.major.y=element_line(color="gray88")
  )
dev.off()

none$github_tenures <- as.numeric(none$github_tenures)

png("histogram-GHtenurNONE.png",    # create PNG for the heat map        
    width = 5*600,        # 5 x 300 pixels
    height = 5*300,
    res = 300)        # smaller font size
ggplot(none,
       aes(x=github_tenures)) +
  geom_histogram((aes(y=..density..)), 
                  lwd =.2, color ="gray28",fill="forestgreen", alpha=.9) +
                   #geom_density(fill="lightgray", alpha =.3) +
                   #geom_bar(stat = "identity", fill = "darkslateblue") +
                   ggtitle("Distribution of GitHub Tenure (No Identifiable Gender)") +
                   xlab("Tenure (in days)") +
                   ylab("density of users") +
                   theme(legend.position = "none",
                         panel.background=element_rect(fill = "white", color = "slategray"),
                         panel.grid.minor.y=element_line(color="gray88"),
                         panel.grid.major.y=element_line(color="gray88")
                   )
                 dev.off()
