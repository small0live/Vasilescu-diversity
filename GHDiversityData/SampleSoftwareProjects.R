# SAMPLING SOFTWARE PROJECTS
# Microsoft Research License Agreement
# Non-Commercial Use Only 
# For the full license, please see file License.txt
#
# (c) Microsoft Corporation. All rights reserved. 



# Load Packages -----------------------------------------------------------


library(data.table)
library(magrittr)
library(tidyverse)


# Create Functions --------------------------------------------------------



## Similarity functions

create.numeric.similarity.sd <- function(value.project, values.universe) {
  stddev <- sd(values.universe)
  function(value.other.project) { 
    value.project - stddev <= value.other.project & value.other.project <= value.project + stddev 
  }
}

create.numeric.similarity <- function(value.project, values.universe) {
  lower <- 10 ^ (log10(value.project+1) - 0.5) - 1 
  upper <- 10 ^ (log10(value.project+1) + 0.5) - 1
  function(value.other.project) { 
    lower <= value.other.project & value.other.project <= upper 
  }
}

create.factor.similarity <- function(value.project, values.universe) {
  function(value.other.project) { 
    value.project == value.other.project 
  }
}


## score a sample of projects in a universe for a given space

score.projects <- function(sample, universe, space, configuration=NA) {
  variables <- all.vars(space)

  if ( length(setdiff(variables, names(sample))) > 0 ) 
    stop(gettextf("variables '%s' not found in sample", paste(setdiff(variables, names(sample)), collapse=", ")), domain = NA)             

  if ( length(setdiff(variables, names(universe))) > 0 ) 
    stop(gettextf("variables '%s' not found in universe", paste(setdiff(variables, names(universe)), collapse=", ")), domain = NA)             

  project_var <- variables[1]
  dimension_vars <- variables[-1]
  
  dim_index_matrix <- matrix(rep(F, length=length(dimension_vars)*nrow(universe)), 
                             nrow=length(dimension_vars), ncol=nrow(universe), byrow=T)
  index_set <- rep(F, length=nrow(universe))
  
  if (nrow(sample)>0) for (pid in 1:nrow(sample)) {
    project_index_set <- rep(T, length=nrow(universe))

    for (dim in 1:length(dimension_vars)) {
      dimension <- dimension_vars[dim]
      
      if (dim <= length(configuration) & !is.na(configuration[dim])) {
        is.similar <- configuration[[dim]](sample[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])      
      }
      else if (is.numeric(universe[,dimension])) {
        is.similar <- create.numeric.similarity(sample[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])
      }
      else if (is.factor(universe[,dimension])) {
        is.similar <- create.factor.similarity(sample[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])
      }
      else {
        stop(gettextf("no similarity function defined for '%s'", dimension, domain = NA))         
      }
      
      dim_index_matrix[dim,] <- dim_index_matrix[dim,] | d
      project_index_set <- project_index_set & d
    }
    index_set <- index_set | project_index_set
  }
  
  score <- sum(index_set, na.rm=T)/length(index_set)
  dimension_score <- apply(dim_index_matrix, 1, function(x) { sum(x, na.rm=T)/length(x) })
  return(list(dimensions=dimension_vars,
              score=score, dimension.score=dimension_score,
              score.indexset=index_set, dimension.indexset=dim_index_matrix))
}


## Compute the N next projects that if added to the sample maximize the score

next.projects <- function(N, sample, universe, space, configuration=NA) {

  start_time <- Sys.time()
  variables <- all.vars(space)
  
  if ( length(setdiff(variables, names(sample))) > 0 ) 
    stop(gettextf("variables '%s' not found in sample", paste(setdiff(variables, names(sample)), collapse=", ")), domain = NA)             
  
  if ( length(setdiff(variables, names(universe))) > 0 ) 
    stop(gettextf("variables '%s' not found in universe", paste(setdiff(variables, names(universe)), collapse=", ")), domain = NA)             
  
  dimension_vars <- variables[-1]

  candidates <- universe
  candidates$score.increase <- 1

  result <- sample[-1:-nrow(sample),]

  print(paste("Computing similarity matrix... This may take a while, please be patient.", Sys.time()))
  
  similar_project_matrix <- matrix(data=F, nrow=nrow(candidates), ncol=nrow(universe), byrow=T)

  for (pid in 1:nrow(candidates)) {
      
    project_index_set <- rep(T, length=nrow(universe))

    for (dim in 1:length(dimension_vars)) {
      dimension <- dimension_vars[dim]
        
      if (dim <= length(configuration) & !is.na(configuration[dim])) {
        is.similar <- configuration[[dim]](candidates[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])      
      }
      else if (is.numeric(candidates[,dimension])) {
        is.similar <- create.numeric.similarity(candidates[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])
      }
      else if (is.factor(candidates[,dimension])) {
        is.similar <- create.factor.similarity(candidates[pid,dimension], universe[,dimension])
        d <- is.similar(universe[,dimension])
      }
      else {
        stop(gettextf("no similarity function defined for '%s'", dimension, domain = NA))         
      }
      project_index_set <- project_index_set & d
    }
    similar_project_matrix[pid,] <- project_index_set
  }
  
  print(paste("Computing similarity matrix... Done", Sys.time()))

  # Initial score
  sc <- score.projects(rbind(sample, result[,1:ncol(sample)]), universe, space, configuration)
  sc.score <- sc$score
  sc.score.indexset <- sc$score.indexset
  
  print(paste("Finding next projects... This may take a while, please be patient.", Sys.time()))
  
  # Compute n next projects
  num_projects = nrow(universe)
  candidate_list <- 1:nrow(candidates)
  for (n in 1:N) {    
    best.increase.sofar <- 0
    
    count <- 0
    for (pid in candidate_list) {
      if (best.increase.sofar >= candidates[pid,]$score.increase) break;
      count <- count + 1
      index_set <- sc.score.indexset | similar_project_matrix[pid,]
      score <- sum(index_set, na.rm=T)/num_projects
      
      candidates[pid,]$score.increase <- score - sc.score
      best.increase.sofar <- max(candidates[pid,]$score.increase, best.increase.sofar)
    }

    new_order = order(candidates$score.increase, decreasing=T, na.last=NA)
    best.increase.row <- new_order[1]
    result = rbind(result, candidates[best.increase.row,])
    sc.score.indexset <- sc.score.indexset | similar_project_matrix[best.increase.row,]
    sc.score <- sum(sc.score.indexset, na.rm=T)/num_projects
    candidates[best.increase.row,]$score.increase <- NA
    
    candidate_list = new_order[-1]
    
    print(paste("Found", n, "projects. New score", sc.score, Sys.time()))
    
    if (sc.score>=1.0) break; # we have covered everything
  }

  sc <- score.projects(rbind(sample, result[,1:ncol(sample)]), universe, space, configuration)
  return(list(new.projects=result, score=sc))
}


# Import and Prep All Projects Data----------------------------------------------------

## set working directory
setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

usr <- fread("user_data.csv", ## this file contains data for ALL projects in Vascilescu's data set 
             stringsAsFactors = TRUE)
## ALL PROJECTS

## import as data table
usr <- fread("user_data.csv", ## this file contains data for ALL projects in Vascilescu's data set 
             stringsAsFactors = TRUE) %>% 
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx), ## last window isn't always == to window count 
         data_collected = "2014-01-02") %>% 
  subset(window_idx > (maxWI - 4)) %>% ## only keeping last four quarters of data for 12-month aggregate variables
  mutate(num_team_year = length(unique(user_ID))) %>%## across last 12 mos, count unique user IDs associated with a project ID
  distinct(window_idx, .keep_all = TRUE) %>% ## get rid of duplicates at the project level
  mutate(num_commits_year = sum(num_commits)) %>% ## across last 12 mos, count number of commits made in a project
  subset(window_idx == maxWI & language != "None", ## only keep last quarter and get rid of projects that don't have a main programming language
         select = c("project_id",
                    "project_age",
                    "created_at", 
                    "data_collected",
                    "windows",
                    "window_idx",
                    "language",
                    "domain",
                    "forks",
                    "watchers",
                    "turnover",
                    "num_team_year",
                    "num_commits_year")) #%>% 
 #mutate(project_age = MESS::age(created_at, data_collected)) 
#%>% ## calculate age using project creation date and data dump date
 #subset(select = -c(2,3)) ## get rid of created_at and data_collected columns

## get user, their gender, and their status for the projects in data table above
absent <- fread("user_data.csv",
                stringsAsFactors = TRUE) %>%
  subset(select = c("project_id",
                    "window_idx",
                    "user_ID",
                    "gender",
                    "absent")) %>%
  group_by(project_id) %>%
  mutate(maxWI = max(window_idx)) %>%
  subset(window_idx > (maxWI - 4),
         select = -6)

## merge these two data tables
## left_join works on data tables and is faster than merge()
Universe <- left_join(usr,
                      absent,
                      by = c("project_id",
                             "window_idx"))

## convert to data frame for use in functions
#uniDF <- as.data.frame(Universe)

## make sure data is in correct format for functions to work
ageLvls <- unique(Universe$project_age)
Universe$project_age <- factor(Universe$project_age, 
                            levels = ageLvls)

languages <- unique(Universe$language)
Universe$language <- factor(Universe$language, 
                         levels = languages)

# Import and Prep All WMXN Projects Data----------------------------------------------------

# ---
## WMXN PROJECTS
# ---

wmxn <- fread("wmxn_user_data.csv", ## this file contains data for projects in Vascilescu's data set who had at least one woman on the team at some point in time
              stringsAsFactors = TRUE) %>% 
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx), ## last window isn't always == to window count 
         data_collected = "2014-01-02") %>% 
  subset(window_idx > (maxWI - 4)) %>% ## only keeping last four quarters of data for 12-month aggregate variables
  mutate(num_team_year = length(unique(user_ID))) %>%## across last 12 mos, count unique user IDs associated with a project ID
  distinct(window_idx, .keep_all = TRUE) %>% ## get rid of duplicates at the project level
  mutate(num_commits_year = sum(num_commits),) %>% ## across last 12 mos, count number of commits made in a project
  subset(window_idx == maxWI & language != "None", ## only keep last quarter and get rid of projects that don't have a main programming language
         select = c("project_id",
                    "project_age",
                    "created_at", 
                    "data_collected",
                    "windows",
                    "window_idx",
                    "language",
                    "domain",
                    "forks",
                    "watchers",
                    "turnover",
                    "num_team_year",
                    "num_commits_year")) 

## get user data for those projects
## note: check that i even need to do this, before i was using absent but now i use turnover?
wmxn.absent <- fread("wmxn_user_data.csv",
                     stringsAsFactors = TRUE) %>%
  subset(select = c("project_id",
                    "window_idx",
                    "user_ID",
                    "gender",
                    "absent")) %>%
  group_by(project_id) %>%
  mutate(maxWI = max(window_idx)) %>%
  subset(window_idx > (maxWI - 4),
         select = -6)


## merge these two data tables, left_join is faster than merge() 

wmxn.Sample <- left_join(wmxn,
                          wmxn.absent,
                          by = c("project_id",
                                 "window_idx"))

## convert to data frame for use in functions
#wmxnUniDF <- as.data.frame(wmxnUniverse)

## make sure data is in correct format for functions to work
ageLvls <- unique(wmxn.Sample$project_age)
wmxn.Sample$project_age <- factor(wmxn.Sample$project_age, 
                                levels = ageLvls)

languages <- unique(wmxn.Sample$language)
wmxn.Sample$language <- factor(wmxn.Sample$language, 
                             levels = languages)

# Import and Prep All MXD Projects Data----------------------------------------------------

# ---
## THIS SUBSET OF THE DATA ONLY INCLUDES GITHUB PROJECTS THAT  
## HAD AT LEAST ONE FEMALE CONTRIBUTOR AND ONE MALE CONTRIBUTOR
## I.E., ALL WOMEN TEAMS ARE EXCLUDED
# ---

## create vector of IDs for projects that are maintained by a mixed-gen team

mxd.list <- fread("wmxn_user_data.csv",
                  stringsAsFactors = TRUE) %>% 
  subset(has_men_Ever == TRUE, 
         select = "project_id") %>%
  unique()

mxd.list <- mxd.list$project_id


## select those proejcts from the wmxn data set

mxd <- fread("wmxn_user_data.csv", 
              stringsAsFactors = TRUE) %>% 
  subset(project_id %in% mxd.list) %>%
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx), 
         data_collected = "2014-01-02") %>% 
  subset(window_idx > (maxWI - 4)) %>% 
  mutate(num_team_year = length(unique(user_ID))) %>%
  distinct(window_idx, .keep_all = TRUE) %>% 
  mutate(num_commits_year = sum(num_commits),) %>% 
  subset(window_idx == maxWI & language != "None", 
         select = c("project_id",
                    "project_age",
                    "created_at", 
                    "data_collected",
                    "windows",
                    "window_idx",
                    "language",
                    "domain",
                    "forks",
                    "watchers",
                    "turnover",
                    "num_team_year",
                    "num_commits_year"))


## get user data for those projects

mxd.absent <- fread("wmxn_user_data.csv",
                     stringsAsFactors = TRUE) %>%
  subset(project_id %in% mxd.list,
         select = c("project_id",
                    "window_idx",
                    "user_ID",
                    "gender",
                    "absent")) %>%
  group_by(project_id) %>%
  mutate(maxWI = max(window_idx)) %>%
  subset(window_idx > (maxWI - 4),
         select = -6)


## merge these data set

mxd.Sample <- left_join(mxd,
                        mxd.absent,
                        by = c("project_id",
                                 "window_idx"))


## make sure data is in correct format for functions to work
ageLvls <- unique(mxd.Sample$project_age)
mxd.Sample$project_age <- factor(mxd.Sample$project_age, 
                                   levels = ageLvls)

languages <- unique(mxd.Sample$language)
mxd.Sample$language <- factor(mxd.Sample$language, 
                                levels = languages)


# Import and Prep All MXN (nonWMXN) Projects Data----------------------------------------------------

# ---
## THIS SUBSET OF THE DATA ONLY INCLUDES GITHUB PROJECTS   
## THAT HAD AT **NO** IDENTIFIED FEMALE CONTRIBUTORS
## I.E., ONLY TEAMS THAT WERE ALL MALE OR MALE + UKNOWN ARE INCLUDED
# ---

## get project IDs

wmxn.list <- unique(wmxn$project_id)

##smart sample
mxn.list <- fread("NoWomanTeams_SmartSampled.csv")
mxn.list <- mxn.list$project_id

## create function that is the opposite of %in%

`%not_in%` <- purrr::negate(`%in%`) 


## import project data as data table

mxn <- fread("user_data.csv", 
              stringsAsFactors = TRUE) %>%
  subset(project_id %in% mxn.list) %>%
  #subset(project_id %not_in% wmxn.list) %>%
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx), 
         data_collected = "2014-01-02") %>% 
  subset(window_idx > (maxWI - 4)) %>% 
  mutate(num_team_year = length(unique(user_ID))) %>% 
  distinct(window_idx, .keep_all = TRUE) %>% 
  mutate(num_commits_year = sum(num_commits)) %>% 
  subset(window_idx == maxWI & language != "None", 
         select = c("project_id",
                    "project_age",
                    "created_at", 
                    "data_collected",
                    "window_idx",
                    "language",
                    "domain",
                    "forks",
                    "watchers",
                    "turnover",
                    "num_team_year",
                    "num_commits_year")) 


## get user data for those projects

mxn.absent <- fread("user_data.csv",
                     stringsAsFactors = TRUE) %>%
  subset(project_id %in% mxn.list)  %>%
  #subset(project_id %not_in% wmxn.list)  %>%
  subset(select = c("project_id",
                    "window_idx",
                    "user_ID",
                    "gender",
                    "absent")) %>%
  group_by(project_id) %>%
  mutate(maxWI = max(window_idx)) %>%
  subset(window_idx > (maxWI - 4),
         select = -6)


## merge these two data tables

mxn.Sample <- left_join(mxn,
                         mxn.absent,
                         by = c("project_id",
                                "window_idx"))

## convert to data frame for use in functions
#mxnUniDF <- as.data.frame(mxnUniverse)

## make sure data is in correct format for functions to work
ageLvls <- unique(mxn.Sample$project_age)
mxn.Sample$project_age <- factor(mxn.Sample$project_age, 
                                levels = ageLvls)

languages <- unique(mxn.Sample$language)
mxn.Sample$language <- factor(mxn.Sample$language, 
                             levels = languages)


# Convert Data Tables to Data Frame ---------------------------------------

# ---
## Functions are set up for data frame syntax
## Data was read in as table rather than frame (re: efficiency)
## Also need to remove duplicates from data sets
# ---

Universe <- as.data.frame(Universe)
wmxn.Sample <- as.data.frame(wmxn.Sample)
mxd.Sample <- as.data.frame(mxd.Sample)
mxn.Sample <- as.data.frame(mxn.Sample)


## keep only a single observation per project

Universe <- Universe %>%
  distinct(project_id, .keep_all = TRUE)

wmxn.Sample <- wmxn.Sample %>%
  distinct(project_id, .keep_all = TRUE)

mxd.Sample <- mxd.Sample %>%
  distinct(project_id, .keep_all = TRUE)

mxn.Sample <- mxn.Sample %>%
  distinct(project_id, .keep_all = TRUE)


# Get Sample Score for onlyWMXN+MXD Projects ---------------------------------------------------

# ---
## Coverage score for wmxn.Sample (universe = all GH OSS projects)
# ---

wmxn.score <- score.projects(wmxn.Sample, universe = Universe, 
                             turnover ~ num_team_year + num_commits_year + forks + watchers + language + project_age) 


wmxn.scoreValue <- purrr::pluck(wmxn.score, 2) ## 0.912482921854254 when absent outcome & 0.881269976293778 when turnover outcome

dimNam <- purrr::pluck(wmxn.score, 1)
dimVal <- purrr::pluck(wmxn.score, 3)

wmxn.scoreDF <- data.frame(dimNam, dimVal) %>%
  mutate(sampleScore = wmxn.scoreValue)


## save sample coverage score for wmxn sample

write.csv(wmxn.scoreDF,
          "wmxnSample_Scores_TurnoverOutcome.csv",
          row.names = F)

# Get Sample Score for MXD Projects ---------------------------------------------------

## THIS CODE IS USED TO GET THE SAMPLE SCORE FOR THE wmxn SAMPLE (compared to all)

mxd.score <- score.projects(mxd.Sample, universe = Universe, 
                             turnover ~ num_team_year + num_commits_year + forks + watchers + language + project_age) 

mxd.scoreValue <- purrr::pluck(mxd.score, 2) ##  0.8788746  when turnover outcome

dimNam <- purrr::pluck(mxd.score, 1)

dimVal <- purrr::pluck(mxd.score, 3)

mxd.scoreDF <- data.frame(dimNam, dimVal) %>%
  mutate(sampleScore = mxd.scoreValue)

## save sample coverage score for mxd sample

write.csv(mxd.scoreDF,
          "mxdSample_Scores_TurnoverOutcome.csv",
          row.names = F)

# Get Sample Score for MXN Projects ---------------------------------------------------

## THIS CODE IS USED TO GET THE SAMPLE SCORE FOR THE mxn SAMPLE (compared to all)

mxn.score <- score.projects(mxn.Sample, universe = Universe, 
                             turnover ~ num_team_year + num_commits_year + forks + watchers + language + project_age)

mxn.scoreValue <- purrr::pluck(mxn.score, 2)
# 0.771287547866916 when absent outcome
# 0.881289976293778 when turnover outcome

dimNam <- purrr::pluck(mxn.score, 1)
dimVal <- purrr::pluck(mxn.score, 3)
mxn.scoreDF <- data.frame(dimNam, dimVal) %>%
  mutate(sampleScore = mxn.scoreValue)

write.csv(mxn.scoreDF,
          "mxnSample_Scores_TurnoverOutcome.csv",
          row.names = F)


# Get Sample for Case Study of onlyWMXN+MXD ------------------------------


## THIS CODE IS USED TO SAMPLE PROJECTS FROM THE wmxn POPULATION

## creates large list with 2 elements 
## first element: df of selected projects
## second element: df of sample scores

## create empty df with same columns as universeDF
## used for sampling
emptyDF = wmxnUniverseT[FALSE,]

wmxnUniverseT <- wmxnUniverseT %>%
  subset(windows >= 4)

np <- next.projects(15, emptyDF, universe = wmxnUniverseT, 
                    turnover ~ num_team_year + num_commits_year + forks + watchers + language + project_age) 

## keep just the first element of large list
projectsDF <- purrr::pluck(np, 1)

## keep just the second element of large list
scores <- purrr::pluck(np, 2)

dfDims <- unlist(scores$dimensions)
dfDimScores <- unlist(scores$dimension.score)

sampleScore <- purrr::pluck(scores, 2)

scoresDF <- data.frame(dfDims, dfDimScores) %>%
  mutate(sampleScore.wmxn = sampleScore)

caseStudySample <- UniverseT %>%
  subset(project_id %in% projectsDF$project_id)

cs.score.uni <- score.projects(caseStudySample, universe = UniverseT, 
                            turnover ~ num_team_year + num_commits_year + forks + watchers + language + project_age)
ss.uni <- purrr::pluck(cs.score.uni, 2)

scoresDF <- scoresDF %>%
  mutate(sampleScore.uni = ss.uni)

####
## keep just the first element of large list
pDF <- purrr::pluck(cs.score.uni, 1)

## keep just the second element of large list
ses <- purrr::pluck(cs.score.uni, 3)

#dfDims <- unlist(scores$dimensions)
#dfDimScores <- unlist(scores$dimension.score)

#sampleScore <- purrr::pluck(scores, 2)

sesDF <- data.frame(pDF, ses)

# Save Case Study Data as CSV --------------------------------------------------------

write.csv(projectsDF,
          "caseStudy_Sample.csv",
          row.names = FALSE)

write.csv(scoresDF,
          "caseStudy_dimScores.csv",
          row.names = FALSE)


## the code below was provided by researchers who created the functions in this file
#sample <- wmxn[wmxn$project_id==2,]
#score <- score.projects(sample, universe=ks, num_commits ~ forks + watchers)
#score.2 <- score.projects(sample, universe=ohloh, id ~ total_code_lines + twelve_month_contributor_count, configuration=c(create.numeric.similarity.sd, NA))
