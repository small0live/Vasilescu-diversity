
# Load Packages -----------------------------------------------------------

library(data.table)
library(DescTools)
library(effects)
library(GGally)
library(ggplot2)
library(lme4)
library(tidyverse)
library(Hmisc)


# Import and Subset Data --------------------------------------------------

## set working directory
setwd("~/Documents/GitHub/Vasilescu-diversity/GHDiversityData")

## import main data file
myDF <- fread("user_data.csv")

## import information needed to select projects
## for modeling
wmxnList <- fread("wmxn_user_data.csv")

wmxnList <- unique(wmxnList$project_id)

mxnList <- fread("NoWomanTeams_SmartSampled.csv")

mxnList <- mxnList$project_id

genStatus <- fread("project_data_smartSamplingFeatures.csv") %>%
  subset(select = c("project_id", "hasWomenEver")) %>%
  distinct(project_id, .keep_all = TRUE)

## select desired projects from main data set
myDF <- myDF %>%
  subset(project_id %in% wmxnList | project_id %in%mxnList)

## add column to indicate teams with women and no women teams
myDF <- left_join(myDF, 
                  genStatus)

modelDF <- myDF %>%
  mutate(genNum = ifelse(gender == "female", 0,
                         ifelse(gender == "male", 1, 2)),
         hasWomEvNum = ifelse(hasWomenEver == TRUE, 1, 0)) %>%
  subset(select = c("project_id",
                    "window_idx",
                    #"user_ID",
                    #"genNum",
                    #"github_tenure",
                    #"commits",
                    "num_team",
                    "num_pull_req",
                    "num_issues",
                    "num_comments",
                    #"blau_gender",
                    "Gini_gh_ten",
                    "hasWomEvNum",
                    "hasWomenEver",
                    "turnover"#,
                    #"absent"
  )) %>%
  distinct(project_id, window_idx, .keep_all = T)

modelDFnoNA <- modelDF %>%
  na.omit()

# Plot Distributions ------------------------------------------------------

ggplot(modelDFnoNA,
       aes(x = turnover)) +
  geom_histogram() 

## user characteristics
#ggplot(myDF,
#       aes(x = gender)) +
#  geom_bar(stat = "count")
#
#ggplot(myDF,
#       aes(x = github_tenure)) +
#  geom_histogram() ## kind of normal but not really
#
#
#ggplot(myDF,
#       aes(x = commits)) +
#  geom_histogram() ## heavy skew
#
### group composition
#ggplot(myDF,
#       aes(x = blau_gender)) +
#  geom_histogram() ## heavy skew
#
#ggplot(myDF,
#       aes(x = Gini_gh_ten)) + 
#  geom_histogram() ## nearly normal, some skew
#
### project characteristics
#
#wmCheck <- myDF %>%
#  distinct(project_id, .keep_all =TRUE)
#
#ggplot(wmCheck,
#       aes(x = hasWomenEver)) +
#  geom_bar(stat = "count")
#
#ggplot(myDF,
#       aes(x = num_team)) + 
#  geom_histogram() ## heavy skew

#ggplot(myDF,
#       aes(x = num_issues)) + 
#  geom_histogram() ## heavy skew

####not sure it makes sense to include these given it's the same num at each timepoint?
#ggplot(myDF,
#       aes(x = forks)) +
#  geom_histogram() ## heavy skew
#
#ggplot(myDF,
#       aes(x = watchers)) +
#  geom_histogram() ## heavy skew

# Simple Correlations ------------------------------------------------------------

rcorr(as.matrix(modelDF))

ggcorr(modelDF, label = TRUE)

#modelDF.laydee <-  modelDF %>%
#  subset(hasWomEvNum == 1, select = -12)

#rcorr(as.matrix(modelDF.laydee))
#
#ggcorr(modelDF.laydee, label = TRUE)

#modelDF.noLaydee <-  modelDF %>%
#  subset(hasWomEvNum == 0, select = -c(10,12))

#rcorr(as.matrix(modelDF.noLaydee))
#
#ggcorr(modelDF.noLaydee, label = TRUE)

#cor(modelDF,
#    use = "pairwise.complete.obs"
#    )
#
rcorr(as.matrix(modelDF))

ggcorr(modelDF, label = TRUE)

#png(filename="pairs.png")
#ggpairs(modelDF) ## not handling missing values, idk what to do rn
#dev.off()

png(filename="corrpairs.png")
ggparcoord(modelDF,
           missing = "exclude")
dev.off()




# R-Squared Function ------------------------------------------------------

# credit to Jarrett Byrnes
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}


# Prep for Model  ---------------------------------------------

#modelDF$num_team_scaled <- scale(modelDF$num_team)#[, 1]
#modelDF$num_comments_scaled <- scale(modelDF$num_comments)#[, 1]
#modelDF$num_pull_req_scaled <- scale(modelDF$num_pull_req)#[, 1]
#modelDF$num_iss_scaled <- scale(modelDF$num_issues)#[, 1]
#
#modelDFnoNA$project_idF <- factor(modelDFnoNA$project_id, levels = unique(modelDFnoNA$project_id))

# Construct Single Term Models ------------------------------------------------------

## main effects null model 
nullModel <- lmer(turnover ~ (1| project_id),
                  data = modelDFnoNA,
                  REML = FALSE)

#summary(nullModel, corr=FALSE)

## MODEL 1: Identifiable Mixed Gender Composition
mod1 <- lmer(turnover ~ hasWomenEver + (1 | project_id), 
             data = modelDFnoNA,
             REML = FALSE)

## MODEL 2: Project Tenure Diversity
mod2 <- lmer(turnover ~ Gini_gh_ten + (1 | project_id),
             data = modelDFnoNA,
             REML = FALSE)

## MODEL 3: Team Size
#mod3 <- lmer(turnover ~ num_team_scaled + (1 | project_id), 
mod3 <- lmer(turnover ~ num_team + (1 | project_id),              
             data = modelDFnoNA,
             REML = FALSE)

## MODEL 4: Number of Pull Requests
#mod4 <- lmer(turnover ~ num_pull_req_scaled + (1 | project_id), 
mod4 <- lmer(turnover ~ num_pull_req + (1 | project_id),
             data = modelDFnoNA,
             REML = FALSE)

## MODEL 5: Number of Comments
#mod5 <- lmer(turnover ~ num_comments_scaled + (1 | project_id), 
mod5 <- lmer(turnover ~ num_comments + (1 | project_id),
             data = modelDFnoNA,
             REML = FALSE)

## MODEL 6: Number of Issues
#mod6 <- lmer(turnover ~ num_iss_scaled + (1 | project_id), 
mod6 <- lmer(turnover ~ num_issues + (1 | project_id),              
             data = modelDFnoNA,
             REML = FALSE)



# Single Term Models Summary and anova Output -----------------------------------------------

## MODEL 1
summary(mod1, corr=FALSE)
anova(nullModel, mod1) #### get p value with logliklihood method


## MODEL 2
summary(mod2, corr=FALSE)
anova(nullModel, mod2)


## MODEL 3
summary(mod3, corr=FALSE)
anova(nullModel, mod3)


## MODEL 4
summary(mod4, corr=FALSE)
anova(nullModel, mod4)


## MODEL 5
summary(mod5, corr=FALSE)
anova(nullModel, mod5)


## MODEL 6
summary(mod6, corr=FALSE)
anova(nullModel, mod6)

# Single Term Models R Squared -----------------------------------------------

## MODEL 1
r2.corr.mer(mod1)
r2.corr.mer(mod1) - r2.corr.mer(nullModel)


## MODEL 2
r2.corr.mer(mod2)
r2.corr.mer(mod2) - r2.corr.mer(nullModel)


## MODEL 3
r2.corr.mer(mod3)
r2.corr.mer(mod3) - r2.corr.mer(nullModel)


## MODEL 4
r2.corr.mer(mod4)
r2.corr.mer(mod4) - r2.corr.mer(nullModel)


## MODEL 5
r2.corr.mer(mod5)
r2.corr.mer(mod5) - r2.corr.mer(nullModel)


## MODEL 6
r2.corr.mer(mod6)
r2.corr.mer(mod6) - r2.corr.mer(nullModel)

# Single Term Models Residuals Check  -----------------------------------------------

## MODEL 1
plot(fitted(mod1), residuals(mod1))
abline(0,0)
hist(residuals(mod1))
qqnorm(residuals(mod1))


## MODEL 2
plot(fitted(mod2), residuals(mod2))
abline(0,0)
hist(residuals(mod2))
qqnorm(residuals(mod2))


## MODEL 3
plot(fitted(mod3), residuals(mod3))
abline(0,0)
hist(residuals(mod3))
qqnorm(residuals(mod3))


## MODEL 4
plot(fitted(mod4), residuals(mod4))
abline(0,0)
hist(residuals(mod4))
qqnorm(residuals(mod4))


## MODEL 5
plot(fitted(mod5), residuals(mod5))
abline(0,0)
hist(residuals(mod5))
qqnorm(residuals(mod5))


## MODEL 6
plot(fitted(mod6), residuals(mod6))
abline(0,0)
hist(residuals(mod6))
qqnorm(residuals(mod6))

# Interactions Model (2 way) ------------------------------------------------------------------

nullFModel <- lmer(turnover ~ hasWomenEver + num_pull_req +
                     (1 | project_id), 
                   data = modelDFnoNA,
                   REML = FALSE)

fullModel <- lmer(turnover ~ 
                    hasWomenEver *
                    num_pull_req  +
                    (1 | project_id), 
                  
                  data = modelDFnoNA,
                  REML = FALSE)

summary(fullModel, corr=FALSE)

plot(fitted(fullModel), residuals(fullModel))
abline(0,0)
hist(residuals(fullModel))
qqnorm(residuals(fullModel))

anova(nullFModel, fullModel)

r2.corr.mer(fullModel)
r2.corr.mer(fullModel) - r2.corr.mer(nullFModel)

#beta(fullModel)

# Interactions Model (EVERYTING) ------------------------------------------------------------------


nullFModel <- lmer(turnover ~ 
                    hasWomenEver +
                    Gini_gh_ten +
                    num_team_scaled +
                    num_pull_req_scaled +
                    num_comments_scaled +
                    (1 | project_id), 
                                  data = modelDFnoNA,
                  REML = FALSE)

fullModel <- lmer(turnover ~ 
                    hasWomenEver *
                    Gini_gh_ten *
                    num_team_scaled *
                    num_pull_req_scaled *
                    num_comments_scaled +
                    (1 | project_id), 
                  data = modelDFnoNA,
                  REML = FALSE)

summary(fullModel, corr=FALSE)

plot(fitted(fullModel), residuals(fullModel))
abline(0,0)
hist(residuals(fullModel))
qqnorm(residuals(fullModel))

anova(nullFModel, fullModel)

r2.corr.mer(fullModel)
r2.corr.mer(fullModel) - r2.corr.mer(nullFModel)

