## Diversity in GitHub
## Olivia B. Newton
## Created Summer 2019
## Modified Spring 2020


## This code is used to reshape Vasilescu's data set.
## Data for each repository's contributors at each time point
## (a quarter of a year) is contained with a single row.
## For my analyis, each contributor at each time point needs their own row. 
## To do this, user id must be linked to to user data.


# Load Packages -----------------------------------------------------------

library(DescTools) # Gini is a DescTools function
library(diverse)
library(stringr)
library(data.table) # fread is a data.table function
library(tidyverse) # seperate_rows is a tidyr function

# Import Data -------------------------------------------------------------

## set working directory
setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

## import as data table (faster than data frame)
myDF <- fread("diversity_data.csv",
                 sep = ";") %>%
  subset(language != "None")

# Subset Data -------------------------------------------------------------


## for breaking out committers and their commits row
user.commits <- myDF %>%
  subset(select = c("project_id",
                    "created_at",
                    "domain", 
                    "windows", 
                    "window_idx",
                    "num_team",
                    "committers",
                    "commits"))


## for breaking out all contributors and their GH tenures row
user.genten <- myDF %>%
  subset(select = c("project_id",
                    "project_age",
                    "created_at",
                    "domain", 
                    "language",
                    "windows", 
                    "window_idx",
                    "watchers",
                    "forks",
                    "turnover",
                    "num_comments",
                    "num_issues",
                    "has_woman",
                    "num_pull_req",
                    "num_commits",
                    "num_team",
                    "team",
                    "github_tenures",
                    "project_tenures",
                    "genders",
                    "blau_gender"))

turnover <- myDF %>%
  subset(select = c("project_id", 
                    "created_at",
                    "windows",
                    "window_idx", 
                    "left", 
                    "stayed", 
                    "joined"))

user.join <- turnover %>% 
  subset(select = c("project_id", 
                    "created_at",
                    "window_idx", 
                    "joined"))

user.left <- turnover %>% 
  subset(select = c("project_id", 
                    "created_at",
                    "windows",
                    "window_idx", 
                    "left"))

# Separate into Rows ------------------------------------------------------

## seperate contributor information (from single cell to individual rows)
user.commits <- user.commits %>% separate_rows(committers, ## the committers themselves
                                               commits) ## the number of commits they contributed

## rename column for df merging 
colnames(user.commits)[colnames(user.commits)=="committers"] <- "team" ## the commit team, these ppl have write privileges

## convert to character to apply string removal
user.genten[,github_tenures:=as.character(github_tenures)] # specific method for data tables

## apply string removal for random dash in a couple of cells
user.genten[,github_tenures:=str_remove_all(github_tenures,"-")]

## seperate cell into rows
user.genten <- user.genten %>% separate_rows(team,
                                             project_tenures,
                                             github_tenures,
                                             genders)

# Merge New Data Frames ---------------------------------------------------

## merge data, starting with gender-tenure and commits info
kitchen.sink <- merge(user.genten, 
                      user.commits, 
                      by = c("project_id", 
                           "created_at",
                           "domain", 
                           "windows", 
                           "window_idx",
                           "num_team",
                           "team"
                           ), 
                      all = T)

## check if there are any NAs produced as a result of merge
apply(kitchen.sink, 2, function(x) any(is.na(x))) ## NAs in commits column are ok bc sometimes user did not submit any commits but remained on the team


## rename columns for clarity
colnames(kitchen.sink)[colnames(kitchen.sink)=="team"] <- "user_ID"
colnames(kitchen.sink)[colnames(kitchen.sink)=="genders"] <- "gender"

## merge previousy merged dat with leaver information
kitchen.sink <- merge(kitchen.sink, 
                      user.left, 
                      by = c("project_id",
                             "created_at",
                             "windows",
                             "window_idx"),
                      all.x = TRUE)

# Assign Active Status for Next Quarter -----------------------------------

## identify those users who leave the repository in the next quarter
kitchen.sink$leaver <- mapply(grepl, 
                              paste0('\\b', kitchen.sink$user_ID, '\\b'), 
                              kitchen.sink$left)

## create new column tracking who will leave
kitchen.sink <- as.data.table(kitchen.sink %>%
                                mutate(absent = 
                                         ifelse(leaver == TRUE, 1, 0)) %>%
                                subset(select = c("project_id",
                                                  "project_age",
                                                  "created_at",
                                                  "windows",
                                                  "window_idx",
                                                  "domain",
                                                  "language",
                                                  "num_team",
                                                  "has_woman",
                                                  "blau_gender",
                                                  "turnover",
                                                  "forks",
                                                  "watchers",
                                                  "num_pull_req",
                                                  "num_commits",
                                                  "num_comments",
                                                  "num_issues",
                                                  "user_ID",
                                                  "gender",
                                                  "commits",
                                                  "github_tenures",
                                                  "project_tenures",
                                                  "absent")))


## check if there are any NAs (just for good measure)
apply(kitchen.sink, 2, function(x) any(is.na(x))) ## NAs in commits column are ok bc sometimes user did not submit any commits but remained on the team

# Calculate Tenure Inequality ---------------------------------------------

## first convert from character to numeric
kitchen.sink[,github_tenures:=as.numeric(github_tenures)]
kitchen.sink[,commits:=as.numeric(commits)]

## calculate Gini coefficient for each project in each quarter
kitchen.sink <- kitchen.sink %>%
  group_by(project_id, window_idx) %>%
  mutate(Gini_gh_ten = Gini(github_tenures)) %>%  ## projID 2 @ window 1 should be 0.1721049
  ungroup

## rename column for clarity
colnames(kitchen.sink)[colnames(kitchen.sink)=="github_tenures"] <- "github_tenure"
colnames(kitchen.sink)[colnames(kitchen.sink)=="project_tenures"] <- "project_tenure"



# Identify Mixed Gender and All Women Teams -----------------------------------------------

## identify all teams that had at least one female contributor 
## at some point in time (i.e., overall, not at each quarter)
wmxn <- kitchen.sink %>%
  subset(select = c("project_id",
                    "windows",
                    "window_idx",
                    "has_woman")) %>%
  unique() %>%
  #mutate(wmxnTrue = ifelse(gender == "female", 1, 0)) %>% 
  subset(has_woman == 1, select = c("project_id")) %>%
  unique()

wmxn.list <- unique(wmxn$project_id)

## identify all teams that had at least one female contributor AND one male contributor
mxd <- kitchen.sink %>%
  subset(project_id %in% wmxn.list, 
         select = c("project_id",
                    "windows",
                    "window_idx",
                    "has_woman",
                    "gender")) %>%
  unique() %>%
  subset(gender == "male") %>%
  mutate(has_men_Ever = TRUE) %>%
  subset(has_men_Ever == TRUE, 
         select = c("project_id", 
                    "has_men_Ever")) %>%
  unique()

wmxn <- left_join(wmxn,
                  mxd,
                  by.x="project_id") 

wmxn$has_men_Ever <- wmxn$has_men_Ever %>%
  replace_na(FALSE)

## check if there are any NAs (just for good measure)
#apply(test, 2, function(x) any(is.na(x)))

#sum(test$has_men == FALSE) # SHOULD BE 44

# Calculate Programming Languages Expertise ---------------------

## not sure how to get count of unique programming languages in a given quarter
## Q1 for project X =/= Q1 for project y
## furthermore, seems that project age =/= platform age

# languages <- kitchen.sink %>%
#  subset(select = c("project_id",
#                    "window_idx",
#                    "language",
#                    "user_ID")) %>%
#  unique() %>%
#  group_by(user_ID) %>%
#   mutate(
#          uniqLang = length(unique(language))) %>%
#   subset(select = c("user_ID",
#                     
#                     "uniqLang")) 


# Fix Dates ---------------------------------------------------------------

kitchen.sink$created_at <- as.Date(kitchen.sink$created_at, 
                                   format = "%Y-%m-%d")

# Save Data ---------------------------------------------------------------

write.csv(kitchen.sink, 
          "user_data.csv")


wmxn.ks <- left_join(wmxn,
                     kitchen.sink,
                     by.x="project_id")

write.csv(wmxn.ks, 
          "wmxn_user_data.csv",
          row.names = F)

