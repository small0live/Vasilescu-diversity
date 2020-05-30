
library(dplyr)

setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

usr <- fread("wmxn_user_data.csv", ## this file contains data for ALL projects in Vascilescu's data set 
             stringsAsFactors = TRUE)

tenure <- usr %>%
  subset(select = c("project_id", 
                    "window_idx", 
                    "user_ID", 
                    "github_tenure",
                    "gender")) %>%
  group_by(user_ID) %>%
  top_n(1, github_tenure) %>%
  distinct(user_ID, .keep_all = TRUE) %>%
  subset(select = c("user_ID", "github_tenure", "gender"))

group_by(tenure, gender) %>%
  summarise(
    count = n(),
    mean = mean(github_tenure, na.rm = TRUE),
    sd = sd(github_tenure, na.rm = TRUE),
    median = median(github_tenure, na.rm = TRUE),
    IQR = IQR(github_tenure, na.rm = TRUE)
  )

tenure.fm <- tenure %>%
  subset(gender != "None")

tenure.fu <- tenure %>%
  subset(gender != "male")

tenure.um <- tenure %>%
  subset(gender != "female") 

## Mann Whitney assumes same distribution shape for two groups
## Mann Whitney only works with two samples

## option 1
wilcox.test(tenure.fu$github_tenure ~ tenure.fu$gender, conf.int = TRUE)
kruskal.test(github_tenure ~ gender, data = tenure.fu)
kruskal.test(github_tenure ~ gender, data = tenure.fm)
kruskal.test(github_tenure ~ gender, data = tenure.um)

## option 2
kruskal.test(github_tenure ~ gender, data = tenure)
pairwise.wilcox.test(tenure$github_tenure, tenure$gender,
                     p.adjust.method = "BH")

## both options produce same results
