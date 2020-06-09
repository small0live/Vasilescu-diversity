library(data.table)
library(ggplot2)
library(magrittr)
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
                    "turnover",
                    "blau_gender"
                    #"team",
                    #"stayed",
                    #"left",
                    #"joined",
                    #"genders"
                    )) %>%
  distinct(project_id, window_idx, .keep_all = T)

caseStudy$project_idF <- factor(caseStudy$project_id, levels = unique(caseStudy$project_id))

# Plots Plots Plots -------------------------------------------------------

ggplot(caseStudy,
       aes(x = window_idx,
           y = blau_gender,
           group = project_idF,
           color = project_idF)) +
  geom_line()

