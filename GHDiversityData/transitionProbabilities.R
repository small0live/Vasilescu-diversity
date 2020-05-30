## GitHub Diversity Data Set
## For CSCW 2020
## Olivia B. Newton
## Created April 2020

# Load Packages -----------------------------------------------------------

library(car)
library(data.table)
library(DescTools)
#library(GGally)
library(ggplot2)
library(lme4)
library(magrittr)
library(MASS)
library(tidyverse)

# Import and Subset Data -------------------------------------------------------------


## set working directory
setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

## import as data table (faster than df)
allProjects <- fread("diversity_data.csv", sep = ";") %>% 
  subset(language != "None")

## need project IDs for case study sample
css <- fread("caseStudy_Sample.csv")

## only keep data for case study sample
caseStudy <- allProjects %>%
  subset(project_id %in% css$project_id, 
         select = c("project_id", 
                    "project_age",
                    "windows",
                    "window_idx",
                    "team",
                    "stayed",
                    "left",
                    "joined",
                    "genders"))

# Get and Merge All CSS Contributor Data ----------------------------------

## CSS - case study sample

## seperate contributor information (from single cell to individual rows)
stayers <- caseStudy %>% 
  separate_rows(stayed) %>% 
  subset(stayed != "", select = c("project_id", "window_idx", "stayed")) %>% 
  mutate(present = 1)

leavers <- caseStudy %>% 
  separate_rows(left) %>%
  subset(left != "", select = c("project_id", "window_idx", "left")) %>%
  mutate(present = 0)

joiners <- caseStudy %>% 
  separate_rows(joined) %>%
  subset(joined != "", select = c("project_id", "window_idx", "joined")) %>%
  mutate(present = 1)

userGender <- caseStudy %>% 
  separate_rows(team, genders) %>%
  subset(select = c("project_id", "team", "genders")) %>%
  unique()

## renames columns in prep for merge
colnames(stayers)[colnames(stayers) == "stayed"] <- "user_ID"
colnames(leavers)[colnames(leavers) == "left"] <- "user_ID"
colnames(joiners)[colnames(joiners) == "joined"] <- "user_ID"
colnames(userGender)[colnames(userGender) == "team"] <- "user_ID"
colnames(userGender)[colnames(userGender) == "genders"] <- "gender"

## merge to link user ID to gender info
stayers <- left_join(stayers, userGender)
leavers <- left_join(leavers, userGender)
joiners <- left_join(joiners, userGender)


## merge so that all users are in one df
allppl <- merge(stayers,
                leavers,
                all = T)

allppl <- merge(allppl,
                joiners,
                all = T)

## add missing team members info
## ASSUMPTION: if there user_id is present in team column
## AND their user_id is not present in any of 3 stay/leave/join columns
## then they are counted as present
lostMembers <- caseStudy %>%
  separate_rows(team, genders) %>%
  subset(select = c("project_id",
                    "window_idx",
                    "team",
                    "genders")) %>%
  mutate(missing = ifelse(team %in% allppl$user_ID, "No", "Yes")) %>%
  subset(missing == "Yes", select = -5) %>%
  mutate(present = 1)

colnames(lostMembers)[colnames(lostMembers) == "team"] <- "user_ID"
colnames(lostMembers)[colnames(lostMembers) == "genders"] <- "gender"

allppl <- merge(allppl,
                lostMembers,
                all = T)

#checkDup <- allppl %>%
#  subset(select = c("project_id",
#         "window_idx",
#         "user_ID")) %>%
#  distinct()

# Create Transition Matrix Function ---------------------------------------

trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}  


# Case Study Sample Project IDs -------------------------------------------

#[1] 3329   
#[2] 31780   
#[3] 33356   
#[4] 64396  
#[5] 241258  
#[6] 258919  
#[7] 270950  
#[8] 412561
#[9] 423191  
#[10] 996361 
#[11] 1472750 
#[12] 1686129 
#[13] 2255704 
#[14] 2778668 
#[15] 2778708

# Project [1] 3329 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 3329)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn


# Project [2] 31780 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 31780)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [3] 33356 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 33356)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [4] 64396 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 64396)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [5] 241258 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 241258)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [6] 258919 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 258919)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn


# Project [7] 270950 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 270950)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [8] 412561 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 412561)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [9] 423191 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 423191)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [10] 996361 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 996361)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [11] 1472750 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 1472750)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn


# Project [12] 1686129 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 1686129)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [13] 2255704 -----------------------------------------------------------


allGen <- allppl %>%
  subset(project_id == 2255704)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [14] 2778668 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 2778668)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn

# Project [15] 2778708 -----------------------------------------------------------

allGen <- allppl %>%
  subset(project_id == 2778708)

allGenGen <- allGen %>%
  subset(select = c("user_ID", "gender")) 

sumGen <- allGenGen %>%
  group_by(gender) %>%
  summarise(Count = n())

cast.all <- allGen %>%
  subset(select = c(
    "window_idx",
    "user_ID",
    "gender",
    "present")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  replace(., is.na(.), 0) 

cast.all <- left_join(allGenGen,
                      cast.all)

cast.fem <- cast.all %>% subset(gender == "female")
cast.mal <- cast.all %>% subset(gender == "male")
cast.unkn <- cast.all %>% subset(gender == "None")

all <- cast.all %>%
  subset(select = -c(1,2)) 

fem <- cast.fem %>%
  subset(select = -c(1,2)) 

mal <- cast.mal %>%
  subset(select = -c(1,2)) 

unkn <- cast.unkn %>%
  subset(select = -c(1,2)) 

all <- trans.matrix(as.matrix(all))
fem <- trans.matrix(as.matrix(fem))
mal <- trans.matrix(as.matrix(mal))
unkn <- trans.matrix(as.matrix(unkn))

all
fem
mal
unkn



