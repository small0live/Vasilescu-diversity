
# Load Packages -----------------------------------------------------------

library(DescTools)
library(GGally)
library(lattice)
library(lme4)

# Prepare Data ------------------------------------------------------------

ks.sizes <- (kitchen.sink %>%
               subset(gender != "None") %>%
               mutate(sizeClass=
                        ifelse(num_team < 11, "small", 
                               ifelse(num_team > 10 & num_team < 30, "medium", 
                                      "large"
                               ))))


wID <- sort(c(unique(ks.sizes$windows), 1))
ks.sizes$window_idx <- factor(as.character(ks.sizes$window_idx), levels = wID)

pID <- (c(unique(ks.sizes$project_id)))
ks.sizes$project_id <- as.character(ks.sizes$project_id)

ks.sizes$project_id_f <- factor(ks.sizes$project_id, levels = pID)

ks.sizes$num_team <- as.numeric(as.character(ks.sizes$num_team))
ks.sizes$github_tenures <- as.numeric(as.character(ks.sizes$github_tenures))
ks.sizes$commits <- as.numeric(as.character(ks.sizes$commits))

ks.sizes$gh_tenure_scaled <- scale(ks.sizes$github_tenures)[, 1]
ks.sizes$gh_tenure_log <- log(ks.sizes$github_tenures)


# Subset by Team Size -----------------------------------------------------

ks.small <- (ks.sizes %>%
               subset(sizeClass == "small")
             ) %>%
  group_by(project_id, window_idx) %>%
  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
  ungroup

ks.med <- (ks.sizes %>%
             subset(sizeClass == "medium")
           ) %>%
  group_by(project_id, window_idx) %>%
  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
  ungroup

ks.large <- (ks.sizes %>%
               subset(sizeClass == "large")
             ) %>%
  group_by(project_id, window_idx) %>%
  mutate(Gini_gh_ten = Gini(github_tenures)) %>%
  ungroup



# Inspect Relationships ---------------------------------------------------

tapply.shingle <- function(x,s,fn,...) {
  result <- c()
  for(l in levels(s)) { 
    x1 <- x[s > l[1] & s < l[2]]
    result <- c(result, fn(x1,...))
  }
  result
  }
logit <- function(x) {
  log(x/(1-x))
}

my.intervals <- cbind(1:29-0.5,1:29+1.5)

smallResponse <- ks.small$leavesNextQ
medResponse <- ks.med$leavesNextQ
largeResponse <- ks.large$leavesNextQ

ks.small$Gen <- ifelse(ks.small$gender=="female",1,0)
ks.med$Gen <- ifelse(ks.med$gender=="female",1,0)
ks.large$Gen <- ifelse(ks.large$gender=="female",1,0)


size.x <- with(ks.large,tapply.shingle(num_team,
                                      shingle(num_team,my.intervals),mean))
size.y <- with(ks.large,tapply.shingle(largeResponse,
                                      shingle(num_team,my.intervals),mean))
plot(size.x,logit(size.y))

ght.x <- with(ks.large,tapply.shingle(github_tenures,
                                          shingle(github_tenures,my.intervals),mean))
ght.y <- with(ks.large,tapply.shingle(largeResponse,
                                            shingle(github_tenures,my.intervals),mean))
plot(ght.x,logit(ght.y))
#plot(ght.x,ght.y)

gen.y <- with(ks.large,tapply.shingle(largeResponse,
                                      shingle(Gen,my.intervals),mean))
gen.x <- with(ks.large,tapply.shingle(Gen,
                                        shingle(Gen,my.intervals),mean))
plot(gen.x,gen.y)

ght.x <- with(ks.large,tapply.shingle(github_tenures,
                                      shingle(Gini_gh_ten,my.intervals),mean))
ght.y <- with(ks.large,tapply.shingle(largeResponse,
                                      shingle(Gini_gh_ten,my.intervals),mean))
plot(ght.x,logit(ght.y))
#plot(ght.x,ght.y)

gen.y <- with(ks.large,tapply.shingle(largeResponse,
                                      shingle(blau_gender,my.intervals),mean))
gen.x <- with(ks.large,tapply.shingle(Gen,
                                      shingle(blau_gender,my.intervals),mean))
plot(gen.x,logit(gen.y))

# Simple Correlations ------------------------------------------------------------

ggpairs(ks.small[, c("gender", "gh_tenure_scaled", "blau_gender", "Gini_gh_ten")])

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
