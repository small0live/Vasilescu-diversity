
# Load Packages -----------------------------------------------------------

library(data.table)
library(DescTools)
library(GGally)
library(ggplot2)
library(lattice)
library(lme4)
library(tidyverse)


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


# Classify Language (Paradigm) -------------------------------------------------------

## based on Ray et al. (2017) http://dx.doi.org/10.1145/3126905

eventDr <- ("Visual Basic")
declar <- c("ASP", "Puppet", "XSLT")
impProc <- c("C", "C++", "C#", "Objective-C", "Java", "Go", "Vala")
impScri <- c("CoffeeScript", "JavaScript", "Python", "Groovy", "Perl", "PHP", "Ruby", "Smalltalk", "TypeScript")
Funct <- c("Clojure", "Erlang", "Haskell", "Scala", "Emacs Lisp", "Elixir")
Scri <- c("Shell", "VimL", "Augeas", "PowerShell")
multiPa <- c("JavaScript", "Lua", "FORTRAN", "R", "ActionScript", "Common Lisp", "Standard ML", "Rust", "Racket", "Matlab", "OCaml", "Julia")
markup <- c("CSS", "AUGEUS", "XML")

myDF <- myDF %>%
  mutate(langParadigm = ifelse(language %in% impProc, "imperativePocedural",
                               ifelse(language %in% impScri, "imperativeScripting",
                                      ifelse(language %in% Funct, "functional",
                                             ifelse(language %in% multiPa, "multiParadigm",
                                                    ifelse(language %in% Scri, "script",
                                                           ifelse(language %in% markup, "markup",
                                                                  ifelse(language %in% declar, "declarative",
                                                                         ifelse(language %in% eventDr, "eventDriven",
                                             "HELP")))))))))

TEST <- myDF %>%
  distinct(project_id, .keep_all = T)

# OLDPrepare Data ------------------------------------------------------------

#ks.sizes <- (kitchen.sink %>%
#               subset(gender != "None") %>%
#               mutate(sizeClass=
#                        ifelse(num_team < 11, "small", 
#                               ifelse(num_team > 10 & num_team < 30, "medium", 
#                                      "large"
#                               ))))
#
#
#wID <- sort(c(unique(ks.sizes$windows), 1))
#ks.sizes$window_idx <- factor(as.character(ks.sizes$window_idx), levels = wID)
#
#pID <- (c(unique(ks.sizes$project_id)))
#ks.sizes$project_id <- as.character(ks.sizes$project_id)
#
#ks.sizes$project_id_f <- factor(ks.sizes$project_id, levels = pID)
#
#ks.sizes$num_team <- as.numeric(as.character(ks.sizes$num_team))
#ks.sizes$github_tenures <- as.numeric(as.character(ks.sizes$github_tenures))
#ks.sizes$commits <- as.numeric(as.character(ks.sizes$commits))
#
#ks.sizes$gh_tenure_scaled <- scale(ks.sizes$github_tenures)[, 1]
#ks.sizes$gh_tenure_log <- log(ks.sizes$github_tenures)
#
#
# OLDSubset by Team Size -----------------------------------------------------

#ks.small <- (ks.sizes %>%
#               subset(sizeClass == "small")
#             ) %>%
#  group_by(project_id, window_idx) %>%
#  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
#  ungroup
#
#ks.med <- (ks.sizes %>%
#             subset(sizeClass == "medium")
#           ) %>%
#  group_by(project_id, window_idx) %>%
#  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
#  ungroup
#
#ks.large <- (ks.sizes %>%
#               subset(sizeClass == "large")
#             ) %>%
#  group_by(project_id, window_idx) %>%
#  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
#  ungroup
#


# OLDInspect Relationships ---------------------------------------------------

#tapply.shingle <- function(x,s,fn,...) {
#  result <- c()
#  for(l in levels(s)) { 
#    x1 <- x[s > l[1] & s < l[2]]
#    result <- c(result, fn(x1,...))
#  }
#  result
#  }
#logit <- function(x) {
#  log(x/(1-x))
#}
#
#my.intervals <- cbind(1:29-0.5,1:29+1.5)
#
#smallResponse <- ks.small$leavesNextQ
#medResponse <- ks.med$leavesNextQ
#largeResponse <- ks.large$leavesNextQ
#
#ks.small$Gen <- ifelse(ks.small$gender=="female",1,0)
#ks.med$Gen <- ifelse(ks.med$gender=="female",1,0)
#ks.large$Gen <- ifelse(ks.large$gender=="female",1,0)
#
#
#size.x <- with(ks.large,tapply.shingle(num_team,
#                                      shingle(num_team,my.intervals),mean))
#size.y <- with(ks.large,tapply.shingle(largeResponse,
#                                      shingle(num_team,my.intervals),mean))
#plot(size.x,logit(size.y))
#
#ght.x <- with(ks.large,tapply.shingle(github_tenures,
#                                          shingle(github_tenures,my.intervals),mean))
#ght.y <- with(ks.large,tapply.shingle(largeResponse,
#                                            shingle(github_tenures,my.intervals),mean))
#plot(ght.x,logit(ght.y))
##plot(ght.x,ght.y)
#
#gen.y <- with(ks.large,tapply.shingle(largeResponse,
#                                      shingle(Gen,my.intervals),mean))
#gen.x <- with(ks.large,tapply.shingle(Gen,
#                                        shingle(Gen,my.intervals),mean))
#plot(gen.x,gen.y)
#
#ght.x <- with(ks.large,tapply.shingle(github_tenures,
#                                      shingle(Gini_gh_ten,my.intervals),mean))
#ght.y <- with(ks.large,tapply.shingle(largeResponse,
#                                      shingle(Gini_gh_ten,my.intervals),mean))
#plot(ght.x,logit(ght.y))
##plot(ght.x,ght.y)
#
#gen.y <- with(ks.large,tapply.shingle(largeResponse,
#                                      shingle(blau_gender,my.intervals),mean))
#gen.x <- with(ks.large,tapply.shingle(Gen,
#                                      shingle(blau_gender,my.intervals),mean))
#plot(gen.x,logit(gen.y))
#


# Plot Distributions ------------------------------------------------------

## user characteristics
ggplot(myDF,
       aes(x = gender)) +
  geom_bar(stat = "count")

ggplot(myDF,
       aes(x = github_tenure)) +
  geom_histogram() ## kind of normal but not really


ggplot(myDF,
       aes(x = commits)) +
  geom_histogram() ## heavy skew

## group composition
ggplot(myDF,
       aes(x = blau_gender)) +
  geom_histogram() ## heavy skew

ggplot(myDF,
       aes(x = Gini_gh_ten)) + 
  geom_histogram() ## nearly normal, some skew

## project characteristics

wmCheck <- myDF %>%
  distinct(project_id, .keep_all =TRUE)

ggplot(wmCheck,
       aes(x = hasWomenEver)) +
  geom_bar(stat = "count")

ggplot(myDF,
       aes(x = num_team)) + 
  geom_histogram() ## heavy skew

###not sure it makes sense to include these given it's the same num at each timepoint?
ggplot(myDF,
       aes(x = forks)) +
  geom_histogram() ## heavy skew

ggplot(myDF,
       aes(x = watchers)) +
  geom_histogram() ## heavy skew

# Simple Correlations ------------------------------------------------------------

ggpairs(myDF[, c("gender", 
                 "github_tenure", 
                 "blau_gender", 
                 "Gini_gh_ten")])

ggpairs(ks.med[, c("gender", "gh_tenure_scaled", "blau_gender", "Gini_gh_ten")])

ggpairs(ks.large[, c("gender", "gh_tenure_scaled", "blau_gender", "Gini_gh_ten")])

# tmp <- melt(ks.large[, c("leavesNextQ", "gender", "gh_tenure_scaled", "blau_gender", "Gini_gh_ten")],
#             id.vars="leavesNextQ")
# 
# ggplot(tmp, aes(factor(leavesNextQ), y = value, fill=factor(leavesNextQ))) +
#   geom_boxplot() +
#   facet_wrap(~variable, scales="free_y")

# Models -------------------------------------------------------------------


summary(glm(largeResponse ~ github_tenures,ks.large,
            family="binomial"))$deviance

summary(glm(largeResponse ~ log(github_tenures),ks.large,
              family="binomial"))$deviance

summary(glm(largeResponse ~ Gen,ks.large,
            family="binomial"))$deviance

#summary(glm(largeResponse ~ log(Gen),ks.large,
#              family="binomial"))$deviance


largeT.glmm <- glmer(leavesNextQ ~ Gen + log(github_tenures) + log(num_team) +
                      (1 | project_id_f) + 
                      (1 | window_idx), 
                    ks.large,
                    family="binomial",
                    nAGQ = 1)
largeT.glmm

## create three models (one for small, one for medium, one for large)

#mSmall <- glmer(leavesNextQ ~ gender + log(github_tenures) + num_team +
#                  (1 | project_id_f) + (1 | window_idx), 
#                data = ks.small, 
#                family = binomial(link="logit"), 
#                control = glmerControl(optimizer = "bobyqa"),
#                nAGQ = 1)
#
#mMedium <- glmer(leavesNextQ ~ gender + gh_tenure_scaled + blau_gender + Gini_gh_ten +
#                   (1 | project_id_f) + (1 | window_idx), 
#                 data = ks.med, 
#                 family = binomial(link="logit"), 
#                 control = glmerControl(optimizer = "bobyqa"),
#                 nAGQ = 1)

#mLarge <- glmer(leavesNextQ ~ gender + gh_tenure_scaled + blau_gender + Gini_gh_ten +
#                  (1 | project_id_f) + (1 | window_idx), 
#                data = ks.large, 
#                family = binomial(link="logit"), 
#                control = glmerControl(optimizer = "bobyqa"),
#                nAGQ = 1)



#used 10 integration points. 
#As we use more integration points, the approximation becomes more accurate converging to the ML estimates; 
#however, more points are more computationally demanding and can be extremely slow or even intractable with today’s technology.

# print the mod results without correlations among fixed effects
print(mSmall, corr = FALSE)
print(mMedium, corr = FALSE)
print(mLarge, corr = FALSE)

se <- sqrt(diag(vcov(mSmall)))
se <- sqrt(diag(vcov(mMedium)))
se <- sqrt(diag(vcov(mLarge)))

# table of estimates with 95% CI
(tabSmall <- cbind(Est = fixef(mSmall), LL = fixef(mSmall) - 1.96 * se, UL = fixef(mSmall) + 1.96 *
                se))

(tabMed <- cbind(Est = fixef(mMedium), LL = fixef(mMedium) - 1.96 * se, UL = fixef(mMedium) + 1.96 *
                se))

(tabLarge <- cbind(Est = fixef(mLarge), LL = fixef(mLarge) - 1.96 * se, UL = fixef(mLarge) + 1.96 *
                se))

#If we wanted odds ratios instead of coefficients on the logit scale, we could exponentiate the estimates and CIs.

exp(tabSmall)
exp(tabMed)
exp(tabLarge)
