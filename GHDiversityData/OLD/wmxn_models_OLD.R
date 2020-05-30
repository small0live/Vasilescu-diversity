## GitHub Diversity Data Set
## For SMST Day 2020
## Olivia B. Newton
## Created March 2020

# Load Packages -----------------------------------------------------------

library(car)
library(data.table)
library(DescTools)
library(GGally)
library(ggplot2)
library(lme4)
library(magrittr)
library(MASS)
library(tidyverse)


# Import Data -------------------------------------------------------------

setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

allProjects <- fread("wmxn_user_data.csv")

allProjectsYear <- allProjects %>%
  group_by(project_id) %>% 
  mutate(maxWI = max(window_idx)) %>% ## last window isn't always == to window count 
  subset(window_idx > (maxWI - 4)) %>% ## only keeping last four quarters of data for 12-month aggregate variables
  mutate(num_team_year = length(unique(user_ID)), ## across last 12 mos, count unique user IDs associated with a project ID
         data_collected = "2014-01-02") %>% ## add a column with the date the data was dumped
  distinct(window_idx, .keep_all = TRUE) %>% ## get rid of duplicates at the project level
  mutate(num_commits_year = sum(num_commits)) %>%
  subset(window_idx == maxWI & language != "None", ## only keep last quarter and get rid of projects that don't have a main programming language
         select = c("project_id",
                    "created_at",
                    "data_collected",
                    "window_idx",
                    "language",
                    "domain",
                    "forks",
                    "watchers",
                    "num_team_year",
                    "num_commits_year")) %>%
  mutate(project_age = MESS::age(created_at, data_collected)) %>% ## calculate age using project creation date and data dump date
  subset(select = -c(2,3)) ## across last 12 mos, count number of commits made in a project


csSample <- fread("sample4probtransitions_CSCW2020.csv") %>%
  subset(select = c("project_id",
                    "project_age",
                    "num_team_year",
                    "num_commits_year",
                    "score.increase")) %>%
  mutate(sample_score = sum(score.increase))

caseStudies <- left_join(csSample,
                         allProjects,
                         by = c("project_id")) %>%
  subset(select = c("project_id",
                    "project_age",
                    "num_team_year",
                    "num_commits_year",
                    "windows",
                    "window_idx",
                    "domain",
                    "language",
                    "forks",
                    "watchers",
                    "num_team",
                    "num_commits",
                    "blau_gender",
                    "Gini_gh_ten",
                    "user_ID",
                    "commits",
                    "gender",
                    "github_tenure",
                    "absent"))

# Visualize All Projects Data ---------------------------------------------

####
## MAIN PROGRAMMING LANGUAGE
####

`%not_in%` <- purrr::negate(`%in%`) 

allProjectsYear$langGroup <- ifelse(allProjectsYear$language %not_in% c("JavaScript", "Ruby", "Python", "PHP", "Java", "C", "C++", "Shell", "Perl", "Objective-C", "C#"), 
                                    "Other", allProjectsYear$language)

allLanguages <- allProjectsYear %>%
  subset(select = c("project_id",
                    "langGroup")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(langGroup) %>%
  summarise(total = n()) 

allLanguages <- setorder(allLanguages, -total)
rownames(allLanguages) <- NULL
allLanguages <- allLanguages[c(1:3, 5:12, 4),]
rownames(allLanguages) <- NULL
allLanguages <- Rev(allLanguages, margin = 1)

ggplot(allLanguages,
       aes(x = langGroup,
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Main Programming Language") +
  xlab("") +
  ylab("Number of Projects") + 
  coord_flip() +
  scale_x_discrete(limits = allLanguages$langGroup) 


####
## PROJECT AGE
####


allAges <- allProjectsYear %>%
  subset(select = c("project_id",
                    "project_age")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(project_age) %>%
  summarise(total = n())

allAges$project_age <- ifelse(allAges$project_age == 5, "5 to 6 years",
                            ifelse(allAges$project_age == 4, "4 to 5 years",
                                   ifelse(allAges$project_age == 3, "3 to 4 years",
                                          ifelse(allAges$project_age == 2, "2 to 3 years",
                                                 ifelse(allAges$project_age == 1, "1 to 2 years", "< 1 year")
                                                 ))))


allAges$project_age <- factor(allAges$project_age,
                              levels = c("< 1 year",
                                         "1 to 2 years",
                                         "2 to 3 years",
                                         "3 to 4 years",
                                         "4 to 5 years",
                                         "5 to 6 years"))

ggplot(allAges,
       aes(x = project_age,
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Project Age", subtitle = "As of January 2014") +
  coord_flip() +
  xlab("") +
  ylab("Number of Projects") +
  xlim(rev(levels(allAges$project_age)))

####
## PROJECT TEAM SIZE (12mo)
####

allTeamSize <- allProjectsYear %>%
  subset(select = c("project_id",
                    "num_team_year")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(numContributors = ifelse(num_team_year == 2, "2",
                                  ifelse(num_team_year == 3, "3",
                                         ifelse(num_team_year == 4, "4",
                                                ifelse(num_team_year == 5, "5",
                                                       ifelse(num_team_year == 6, "6",
                                                              ifelse(num_team_year == 7, "7",
                                                                     ifelse(num_team_year == 8, "8",
                                                                            ifelse(num_team_year == 9, "9",
                                                                                   ifelse(num_team_year >= 10 & num_team_year <= 20, "10 to 20",
                                                                                          ifelse(num_team_year > 20 & num_team_year <= 50, "21 to 50",
                                                                                                 ifelse(num_team_year > 50 & num_team_year <= 100, "50 to 100",
                                                                                                        ifelse(num_team_year > 100 & num_team_year <= 500, "100 to 500",
                                                                                                               "> 500"
                                                                                                        ))))))))))))
         ) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(numContributors) %>%
  summarise(total = n())

allTeamSize$numContributors <- as.factor(allTeamSize$numContributors)

allTeamSize <- allTeamSize[c(4,6,7,8,10:13,2,5,9,3,1),]
rownames(allTeamSize) <- NULL
allTeamSize <- Rev(allTeamSize, margin = 1)

ggplot(allTeamSize,
       aes(x = numContributors,
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Project Team Size (last 12 months)") +
  ylab("") +
  xlab("Number of Projects") +
  coord_flip() +
  scale_x_discrete(limits = allTeamSize$numContributors) 

####
## NUMBER OF COMMITS (12mo)
####

allCommits <- allProjectsYear %>%
  subset(select = c("project_id",
                    "num_commits_year")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(numCommits = ifelse(num_commits_year >= 1 & num_commits_year < 51, "1-50",
                             ifelse(num_commits_year >= 51 & num_commits_year < 101, "51-100",
                                    ifelse(num_commits_year >= 101 & num_commits_year < 151, "101-150",
                                           ifelse(num_commits_year >= 151 & num_commits_year < 201, "151-200",
                                                  ifelse(num_commits_year >= 201 & num_commits_year < 251, "201-250",
                                                         ifelse(num_commits_year >= 251 & num_commits_year < 301, "251-300",
                                                                ifelse(num_commits_year >= 301 & num_commits_year < 1001, "301-1000",
                                                                       ifelse(num_commits_year >= 1001 & num_commits_year < 5001, "1001-5000", ">5000"))))))))
  ) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(numCommits) %>%
  summarise(total = n())

allCommits$numCommits <- as.factor(allCommits$numCommits)
allCommits <- allCommits[c(2,9,4:8,3,1),]
rownames(allCommits) <- NULL
allCommits <- Rev(allCommits, margin = 1)

ggplot(allCommits,
       aes(x = numCommits,
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Commits (last 12 months)") +
  ylab("") +
  xlab("Number of Projects") +
  coord_flip() +
  scale_x_discrete(limits = allCommits$numCommits) 

# Visualize Case Study Data ----------------------------------------------------------

####
## MAIN PROGRAMMING LANGUAGE
####

csLanguages <- caseStudies %>%
  subset(select = c("project_id",
                  "language")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(language) %>%
  summarise(total = n())

ggplot(csLanguages,
       aes(x = reorder(language, -total),
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Main Programming Language") +
  xlab("") +
  ylab("Number of Projects")

####
## PROJECT AGE
####

csAges <- caseStudies %>%
  subset(select = c("project_id",
                    "project_age")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  group_by(project_age) %>%
  summarise(total = n())

ggplot(csAges,
       aes(x = factor(project_age),
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Project Age") +
  xlab("") +
  ylab("Number of Projects") +
  scale_x_discrete(labels=c("0" = "< 1 year", 
                            "1" = "1 to 2 years",
                            "2" = "2 to 3 years",
                            "3" = "3 to 4 years",
                            "4" = "4 to 5 years"))

############################
## PROJECT TEAM SIZE (12mo)
############################
csContributors <- caseStudies %>%
  subset(select = c("project_id",
                    "num_team_year")) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  mutate(numContributors = ifelse(num_team_year == 2, "2",
                                  ifelse(num_team_year == 3, "3",
                                         ifelse(num_team_year == 4, "4",
                                                ifelse(num_team_year == 5, "5",
                                                       ifelse(num_team_year == 6, "6",
                                                              ifelse(num_team_year == 7, "7",
                                                                     ifelse(num_team_year == 8, "8",
                                                                            ifelse(num_team_year == 9, "9",
                                                                                   ifelse(num_team_year >= 10 & num_team_year <= 20, "10 to 20",
                                                                                          ifelse(num_team_year > 20 & num_team_year <= 50, "21 to 50",
                                                                                                 ifelse(num_team_year > 50 & num_team_year <= 100, "50 to 100",
                                                                                                        ifelse(num_team_year > 100 & num_team_year <= 500, "100 to 500",
                                                                                                               "> 500"
                                                                                          )))))))))))))

ggplot(csContributors,
       aes(x = numContributors,
           y = total)) + 
  geom_bar(stat = "identity") +
  ggtitle("Project Age") +
  xlab("") +
  ylab("Number of Projects") 

# Check Distribution ------------------------------------------------------

qqPlot(allProjects$absent, "norm")

qqPlot(allProjects$absent, "lnorm")

qqPlot(caseStudies$absent, "norm")

qqPlot(caseStudies$absent, "lnorm")

nbinom <- fitdistr(caseStudies$absent, 
                   "Negative Binomial")
qqp(caseStudies$absent, "nbinom", 
    size = nbinom$estimate[[1]], 
    mu = nbinom$estimate[[2]])

poisson <- fitdistr(caseStudies$absent, "Poisson")
qqp(caseStudies$absent, "pois", poisson$estimate, lambda = poisson$sd)

gamma <- fitdistr(caseStudies$absent, "gamma")
qqp(caseStudies$absent, "gamma", 
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])

# Construct Model ---------------------------------------------------------
caseStudies$absent.num <- (as.numeric(as.character(caseStudies$absent))-1)
caseStudies$project_id <- factor(caseStudies$project_id)
caseStudies$user_ID <- factor(caseStudies$user_ID)
caseStudies$window_idx <- factor(caseStudies$window_idx)
caseStudies$gender <- factor(caseStudies$gender)
caseStudies$language <- factor(caseStudies$language)


###
library(Hmisc)


ggplot(aes(x = blau_gender, 
           y = absent.num), data = caseStudies) + 
  stat_summary(fun.data = "mean_cl_boot", 
               geom ='line', 
               aes(group = project_id))


###

glmer(absent ~ (1 | num_team),
      family = binomial(link = "logit"),
      caseStudies)

glmer(absent ~ (1 | user_ID),
      family = binomial(link = "logit"),
      caseStudies)

glmer(absent ~ gender + (1 | project_id),
      family = binomial(link = "logit"),
      caseStudies)

glmer(absent ~ gender + (1 | project_id) + (1 | user_ID),
      family = binomial(link = "logit"),
      caseStudies)


glmer(absent ~ gender + (user_ID | project_id),
      family = binomial(link = "logit"),
      caseStudies)

glmer(absent ~ gender + blau_gender + Gini_gh_ten + language + (1 | num_team) + (1 | project_id),
      family = binomial(link = "logit"),
      caseStudies)


# DTMC --------------------------------------------------------------------


trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}  

mmDF <- caseStudies %>%
  subset(select = c("project_id",
                    "window_idx",
                    "user_ID",
                    "gender",
                    "absent"))


mmDF <- mmDF[with(mmDF, order(project_id, window_idx)),]

female.mmDF <- mmDF %>%
  subset(gender == "female" & project_id == 46357)

male.mmDF <- mmDF %>%
  subset(gender == "male" & project_id == 46357)

unknown.mmDF <- mmDF %>%
  subset(gender == "None" & project_id == 46357)



fem.cast.mmDF <- female.mmDF %>%
  subset(select = c(
                    "window_idx",
                    "user_ID",
                    "absent")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  subset(select = -c(1,2))

masc.cast.mmDF <- male.mmDF %>%
  subset(select = c(
                    "window_idx",
                    "user_ID",
                    "absent")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  subset(select = -c(1,2))

unknown.cast.mmDF <- unknown.mmDF %>%
  subset(select = c(
                    "window_idx",
                    "user_ID",
                    "absent")) %>%
  reshape2::dcast(user_ID ~ window_idx) %>%
  subset(select = -c(1,2))


#solution <- as.data.frame(table(unlist(cast.mmDF)))

female <- trans.matrix(as.matrix(fem.cast.mmDF))
male <- trans.matrix(as.matrix(masc.cast.mmDF))
unknown <- trans.matrix(as.matrix(unknown.cast.mmDF))

female
male
unknown

#library(markovchain)
#byRow <- TRUE
#mcLadiesLeave <- new("markovchain",
#                     states = c("0", "1"),
#                     transitionMatrix = matrix(data = c(0.79195, 0.20805,
#                                                        1, 0), byrow=byRow, nrow=2),
#                     name = "test")
#
#mcLadsLeave <- new("markovchain",
#                     states = c("0", "1"),
#                     transitionMatrix = matrix(data = c(0.7838607, 0.2161393,
#                                                        1, 0), byrow=byRow, nrow=2),
#                     name = "test")
#
#mcTheyLeave <- new("markovchain",
#                     states = c("0", "1"),
#                     transitionMatrix = matrix(data = c(0.7646201, 0.2353799,
#                                                        1, 0), byrow=byRow, nrow=2),
#                     name = "test")
#
#defaultMc <- new("markovchain")
#
#initialState <- c(0, 1)
#after2quarters <- initialState * (mcLadiesLeave * mcLadiesLeave)
#after8quarters <- initialState * (mcLadiesLeave ^ 8)
#after2quarters
#after8quarters
#
#after2quarters <- initialState * (mcLadsLeave * mcLadsLeave)
#after8quarters <- initialState * (mcLadsLeave ^ 8)
#after2quarters
#after8quarters




# Hidden Markov Model -----------------------------------------------------

library(depmixS4)

## need to pick a subset of projects, and then run three hmm per project (f,m,none)

hmmDF <- myDF %>%
  subset(select = -c(2:3))


hmmDF <- hmmDF[
  with(mmDF, order(project_id, window_idx)), ## not working
  ]

female.hmm <- hmmDF %>%
  subset(gender == "female")

male.hmm <- hmmDF %>%
  subset(gender == "male")

unknown.hmm <- hmmDF %>%
  subset(gender == "None")

#set.seed(1)

#myHMMdf <- myDF %>%
#  subset(select = c("window_idx",
#                    "project_id",
#                    #"language",
#                    #"num_team",
#                    "blau_gender",
#                    "Gini_gh_ten",
#                    "gender",
#                    "github_tenure",
#                    "absent"))
#
#names(myHMMdf)[1] <- "time"
#apply(myHMMdf, 2, function(x) any(is.na(x)))

#myHMMdf$language <- factor(myHMMdf$language, levels = ugh)
#myHMMdf$gender <- as.factor(myHMMdf$gender)

mod <- depmix(response = absent ~ 1, 
              data = female.hmm, 
              nstates = 2,
              family = poisson())

fm <- fit(mod)
fm
summary(fm)

mod <- depmix(response = absent ~ 1, 
              data = male.hmm, 
              nstates = 2,
              family = poisson())

fm <- fit(mod)
fm
summary(fm)

mod <- depmix(response = absent ~ 1, 
              data = unknown.hmm, 
              nstates = 2,
              family = poisson())

fm <- fit(mod)
fm
summary(fm)




