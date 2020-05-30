## Diversity in GitHub
## Olivia B. Newton
## Summer 2019

## This code is for calculating proportion of leavers by gender and other variables of interest.

# Load Packages -----------------------------------------------------------

library(data.table)
library(dplyr)
library(ggplot2)
library(gplots)
## heatmap.2 is a gplots function
library(RColorBrewer)
library(skimr)
library(tidyr)


# Import Data -------------------------------------------------------------

options(scipen=999)

myDF <- fread("user_data.csv")
myDF.core <- fread("user_data_CoreLabeled.csv")


# Subset Data -------------------------------------------------------------

rates <- myDF %>%
  subset(num_team > 1 & gender != "None" , 
         select = c("project_id", 
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "Gini_gh_ten",
                    "github_tenure",
                    "blau_gender",
                    "gender",
                    "leavesNextQ")) 

rates.core <- myDF.core %>%
  subset(num_team > 1 & gender != "None" & Core2 == 1, 
         select = c("project_id", 
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "Gini_gh_ten",
                    "github_tenure",
                    "blau_gender",
                    "gender",
                    "leavesNextQ")) 
# Data for Men ------------------------------------------------------------

## data frame for men
men.rates <- rates %>%
  subset(gender == "male")

## bin repositories by team size, gender diversity, and tenure diversity 
menOverall <- men.rates %>%
  subset(select = c("project_id",
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "Gini_gh_ten",
                    "github_tenure",
                    "blau_gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         genDiverse =
           ifelse(blau_gender == 0, 0,
             ifelse(blau_gender > 0 & blau_gender <= 0.1, 0.1,
                  ifelse(blau_gender > 0.1 & blau_gender <= 0.2, 0.2,
                         ifelse(blau_gender > 0.2 & blau_gender <= 0.3, 0.3,
                                ifelse(blau_gender > 0.3 & blau_gender <= 0.4, 0.4,
                                       0.5))))),
         #tenDiverse =
         #  ifelse(Gini_gh_ten <= 0.1, 0.1,
         #         ifelse(Gini_gh_ten > 0.1 & Gini_gh_ten <= 0.2, 0.2,
         #                ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.3, 0.3,
         #                       ifelse(Gini_gh_ten > 0.3 & Gini_gh_ten <= 0.4, 0.4,
         #                              ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.5, 0.5,
         #                                     ifelse(Gini_gh_ten > 0.5 & Gini_gh_ten <= 0.6, 0.6,
         #                                            ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.7, 0.7,
         #                                                   ifelse(Gini_gh_ten > 0.7 & Gini_gh_ten <= 0.8 , 0.9, 1))))))))) %>%
         tenDiverse =
           ifelse(Gini_gh_ten == 0, 0,
           ifelse(Gini_gh_ten > 0 & Gini_gh_ten <= 0.2, 0.2,
                  ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.4, 0.4,
                         ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.6, 0.6,
                                ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.8, 0.8, 1
                                )))))) %>%
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "genDiverse",
                    "tenDiverse",
                    "leavesNextQ")) 


## calculate gender diversity 
menGenDiv <- menOverall %>%  
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "genDiverse",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx, teamSize, genDiverse) %>%
  mutate(genCount = dplyr::n(), 
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(project_id, window_idx, teamSize, genDiverse) %>%
  mutate(propLeft = (genCount/sum(genCount))) %>%
  subset(select = c("teamSize",
                    "genDiverse",
                    "propLeft")) %>%
  ungroup %>%
  unique() %>%
  group_by(teamSize, genDiverse) %>%
  mutate(medPropLeft = median(propLeft),
         avgPropLeft = mean(propLeft)) %>%
  subset(select = c("teamSize",
                    "genDiverse",
                    #"propLeft"
                    "medPropLeft"
                    #"avgPropLeft"
                    )) %>%
  ungroup %>%
  unique()


## calculate tenure diversity 
menTenDiv <- menOverall %>%  
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "tenDiverse",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx, teamSize, tenDiverse) %>%
  mutate(tenCount = dplyr::n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(project_id, window_idx, teamSize, tenDiverse) %>%
  mutate(propLeft = (tenCount/sum(tenCount))) %>%
  subset(select = c("teamSize",
                    "tenDiverse",
                    "propLeft")) %>%
  ungroup %>%
  unique() %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(medPropLeft = median(propLeft),
         avgPropLeft = mean(propLeft)) %>%
  subset(select = c("teamSize",
                    "tenDiverse",
                    #"propLeft"
                    "medPropLeft"
                    #"avgPropLeft"
  )) %>%
  ungroup %>%
  unique()


## round to the thousandths for plotting
menGenDiv$medPropLeft <-  round(menGenDiv$medPropLeft, 3)
menTenDiv$medPropLeft <-  round(menTenDiv$medPropLeft, 3)


# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

menGenDiv.WIDE <- spread(menGenDiv, genDiverse, medPropLeft)
menTenDiv.WIDE <- spread(menTenDiv, tenDiverse, medPropLeft)


## reordering rows
menGenDiv.WIDE <- menGenDiv.WIDE[c(3,4,5,1,2),]
menTenDiv.WIDE <- menTenDiv.WIDE[c(3,4,5,1,2),]


# Data for Women ----------------------------------------------------------

women.rates <- rates %>%
  subset(gender == "female")


womenOverall <- women.rates %>%
  subset(select = c("project_id",
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "Gini_gh_ten",
                    "github_tenure",
                    "blau_gender",
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         genDiverse =
           ifelse(blau_gender == 0, 0,
                  ifelse(blau_gender > 0 & blau_gender <= 0.1, 0.1,
                  ifelse(blau_gender > 0.1 & blau_gender <= 0.2, 0.2,
                         ifelse(blau_gender > 0.2 & blau_gender <= 0.3, 0.3,
                                ifelse(blau_gender > 0.3 & blau_gender <= 0.4, 0.4,
                                       0.5))))),
         #tenDiverse =
         #  ifelse(Gini_gh_ten <= 0.1, 0.1,
         #         ifelse(Gini_gh_ten > 0.1 & Gini_gh_ten <= 0.2, 0.2,
         #                ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.3, 0.3,
         #                       ifelse(Gini_gh_ten > 0.3 & Gini_gh_ten <= 0.4, 0.4,
         #                              ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.5, 0.5,
         #                                     ifelse(Gini_gh_ten > 0.5 & Gini_gh_ten <= 0.6, 0.6,
         #                                            ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.7, 0.7,
         #                                                   ifelse(Gini_gh_ten > 0.7 & Gini_gh_ten <= 0.8 , 0.9, 1))))))))) %>%
         tenDiverse =
           ifelse(Gini_gh_ten <= 0.2, 0.2,
                  ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.4, 0.4,
                         ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.6, 0.6,
                                ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.8, 0.8, 1
                                ))))) %>%
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "genDiverse",
                    "tenDiverse",
                    "leavesNextQ")) 


## calculate gender diversity 
womenGenDiv <- womenOverall %>%  
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "genDiverse",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx, teamSize, genDiverse) %>%
  mutate(genCount = dplyr::n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(project_id, window_idx, teamSize, genDiverse) %>%
  mutate(propLeft = (genCount/sum(genCount))) %>%
  subset(select = c("teamSize",
                    "genDiverse",
                    "propLeft")) %>%
  ungroup %>%
  unique() %>%
  group_by(teamSize, genDiverse) %>%
  mutate(medPropLeft = median(propLeft),
         avgPropLeft = mean(propLeft)) %>%
  subset(select = c("teamSize",
                    "genDiverse",
                    #"propLeft"
                    "medPropLeft"
                    #"avgPropLeft"
  )) %>%
  ungroup %>%
  unique()


## calculate tenure diversity 
womenTenDiv <- womenOverall %>%  
  subset(select = c("project_id",
                    "window_idx",
                    "teamSize",
                    "tenDiverse",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx, teamSize, tenDiverse) %>%
  mutate(tenCount = dplyr::n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(project_id, window_idx, teamSize, tenDiverse) %>%
  mutate(propLeft = (tenCount/sum(tenCount))) %>%
  subset(select = c("teamSize",
                    "tenDiverse",
                    "propLeft")) %>%
  ungroup %>%
  unique() %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(medPropLeft = median(propLeft),
         avgPropLeft = mean(propLeft)) %>%
  subset(select = c("teamSize",
                    "tenDiverse",
                    #"propLeft"
                    "medPropLeft"
                    #"avgPropLeft"
  )) %>%
  ungroup %>%
  unique()
  
womenGenDiv$medPropLeft <-  round(womenGenDiv$medPropLeft, 3)
womenTenDiv$medPropLeft <-  round(womenTenDiv$medPropLeft, 3)


# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

womenGenDiv.WIDE <- spread(womenGenDiv, genDiverse, medPropLeft)
womenTenDiv.WIDE <- spread(womenTenDiv, tenDiverse, medPropLeft)

womenGenDiv.WIDE <- womenGenDiv.WIDE[c(3,4,5,1,2),]
womenTenDiv.WIDE <- womenTenDiv.WIDE[c(3,4,5,1,2),]


# Scatterplots ------------------------------------------------------------

#menGenDiv$teamSize <- factor(menGenDiv$teamSize, 
#                             levels = c("2", "3 to 5", "6 to 9", "10 to 15", "16 or more"))
#
#ggplot(menGenDiv, 
#       aes(x=genDiverse, 
#           color=factor(teamSize), 
#           y=propLeft)) + 
#  geom_point() 
#
#ggplot(menGenDiv, 
#       aes(x=genDiverse, 
#           color=factor(teamSize), 
#           y=medPropLeft)) + 
#  geom_point() 
#
#ggplot(menGenDiv, 
#       aes(x=genDiverse, 
#           color=factor(teamSize), 
#           y=avgPropLeft)) + 
#  geom_point()


# Transform Data into Matrix Format ---------------------------------------

## WOMEN - GENDER DIVERSITY
colnames(womenGenDiv.WIDE)[colnames(womenGenDiv.WIDE)=="teamSize"] <- "rnames"
rnames <- womenGenDiv.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(womenGenDiv.WIDE[,2:ncol(womenGenDiv.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data) <- unlist(rnames)                  # assign row names


## WOMEN - TENURE DIVERSITY
colnames(womenTenDiv.WIDE)[colnames(womenTenDiv.WIDE)=="teamSize"] <- "rnames"
rnames0 <- womenTenDiv.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data0 <- data.matrix(womenTenDiv.WIDE[,2:ncol(womenTenDiv.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data0) <- unlist(rnames0)                  # assign row names


## MEN - GENDER DIVERSITY
colnames(menGenDiv.WIDE)[colnames(menGenDiv.WIDE)=="teamSize"] <- "rnames"
rnames1 <- menGenDiv.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data1 <- data.matrix(menGenDiv.WIDE[,2:ncol(menGenDiv.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data1) <- unlist(rnames1)                  # assign row names


## MEN - TENURE DIVERSITY
colnames(menTenDiv.WIDE)[colnames(menTenDiv.WIDE)=="teamSize"] <- "rnames"
rnames2 <- menTenDiv.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data2 <- data.matrix(menTenDiv.WIDE[,2:ncol(menTenDiv.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data2) <- unlist(rnames2)                  # assign row names


# Customize and Plot Heat Map ---------------------------------------------


coul = brewer.pal(4, "PRGn")
## I can add more tones to this palette with the following code:
coul = colorRampPalette(coul)(50)

# (optional) defines the color breaks manually for a "skewed" color transition
#col_breaks = c(seq(1,0.81,length=100),  # for green
#               seq(0.8, 0.01,length=100),           # for yellow
#               seq(0,-1,length=100)  # for red
#               )             # for green


## GENDER DIVERSITY

# creates a 5 x 5 inch image
png("heatmap-womenBLAU.png",    # create PNG for the heat map
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          xlab="Blau Index (gender diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device

png("heatmap-menBLAU.png",    # create PNG for the heat map
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data1,
          cellnote = mat_data1,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          xlab="Blau Index (gender diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device


## TENURE DIVERSITY

png("heatmap-womenGINI.png",    # create PNG for the heat map
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data0,
          cellnote = mat_data0,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          xlab="Gini Coefficient (tenure diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device

png("heatmap-menGINI.png",    # create PNG for the heat map
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data2,
          cellnote = mat_data2,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          xlab="Gini Coefficient (tenure diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device



# All over again but with core --------------------------------------------

men.core.rates <- rates.core %>%
  subset(gender == "male")

menOverall.core <- men.core.rates %>%
  subset(select = c("domain",
                    "language",
                    "num_team",
                    "Gini_gh_ten",
                    "blau_gender",
                    "num_commits",
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  # 2	3-5	6-9	10+
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9",
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         #genDiverse =
         #  ifelse(blau_gender <= 0.1, 0.1,
         #         ifelse(blau_gender > 0.1 & blau_gender <= 0.2, 0.2,
         #                ifelse(blau_gender > 0.2 & blau_gender <= 0.3, 0.3,
         #                       ifelse(blau_gender > 0.3 & blau_gender <= 0.4, 0.4,
         #                              0.5))))
         tenDiverse =
           ifelse(Gini_gh_ten <= 0.2, 0.2,
                  ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.4, 0.4,
                         ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.6, 0.6,
                                ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.8, 0.8, 1
                                ))))
         ) %>%
  subset(select = c("teamSize",
                    #"genDiverse",
                    "tenDiverse",
                    "leavesNextQ")) %>%
  #group_by(teamSize, genDiverse) %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  #group_by(teamSize, genDiverse) %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(propLeft = (Count/sum(Count))) %>%
  subset(select = c("teamSize",
                    #"genDiverse",
                    "tenDiverse",
                    "propLeft")) %>%
  unique()

menOverall.core$propLeft <-  round(menOverall.core$propLeft, 3)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

#menOverall.core.WIDE <- spread(menOverall.core, genDiverse, propLeft)
menOverall.core.WIDE <- spread(menOverall.core, tenDiverse, propLeft)

menOverall.core.WIDE <- menOverall.core.WIDE[c(3,4,5,1,2),]

women.core.rates <- rates.core %>%
  subset(gender == "female")

womenOverall.core <- women.core.rates %>%
  subset(select = c("domain",
                    "language",
                    "num_team",
                    "Gini_gh_ten",
                    "blau_gender",
                    "num_commits",
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  # 2	3-5	6-9	10+
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9",
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         #genDiverse =
         #  ifelse(blau_gender <= 0.1, 0.1,
         #         ifelse(blau_gender > 0.1 & blau_gender <= 0.2, 0.2,
         #                ifelse(blau_gender > 0.2 & blau_gender <= 0.3, 0.3,
         #                       ifelse(blau_gender > 0.3 & blau_gender <= 0.4, 0.4,
         #                              0.5))))
         tenDiverse =
           ifelse(Gini_gh_ten <= 0.2, 0.2,
                  ifelse(Gini_gh_ten > 0.2 & Gini_gh_ten <= 0.4, 0.4,
                         ifelse(Gini_gh_ten > 0.4 & Gini_gh_ten <= 0.6, 0.6,
                                ifelse(Gini_gh_ten > 0.6 & Gini_gh_ten <= 0.8, 0.8, 1
                                ))))
  ) %>%
  subset(select = c("teamSize",
                    #"genDiverse",
                    "tenDiverse",
                    "leavesNextQ")) %>%
  #group_by(teamSize, genDiverse) %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  #group_by(teamSize, genDiverse) %>%
  group_by(teamSize, tenDiverse) %>%
  mutate(propLeft = (Count/sum(Count))) %>%
  subset(select = c("teamSize",
                    #"genDiverse",
                    "tenDiverse",
                    "propLeft")) %>%
  unique()

womenOverall.core$propLeft <-  round(womenOverall.core$propLeft, 3)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

#womenOverall.core.WIDE <- spread(womenOverall.core, genDiverse, propLeft)
womenOverall.core.WIDE <- spread(womenOverall.core, tenDiverse, propLeft)

womenOverall.core.WIDE <- womenOverall.core.WIDE[c(3,4,5,1,2),]

#########################################################
### B) Reading in data and transform it into matrix format
#########################################################



# Transform the data ------------------------------------------------------


colnames(menOverall.core.WIDE)[colnames(menOverall.core.WIDE)=="teamSize"] <- "rnames"

rnames2 <- menOverall.core.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data2 <- data.matrix(menOverall.core.WIDE[,2:ncol(menOverall.core.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data2) <- unlist(rnames2)                  # assign row names

colnames(womenOverall.core.WIDE)[colnames(womenOverall.core.WIDE)=="teamSize"] <- "rnames"

rnames3 <- womenOverall.core.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data3 <- data.matrix(womenOverall.core.WIDE[,2:ncol(womenOverall.core.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data3) <- unlist(rnames3)                  # assign row names


# Heatmaps ----------------------------------------------------------------


# creates a 5 x 5 inch image
#png("heatmap-womenCoreBLAU.png",    # create PNG for the heat map
    png("heatmap-womenCoreGINI.png",    # create PNG for the heat map
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data3,
          cellnote = mat_data3,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          #xlab="Blau Index (gender diversity)",
          xlab="Gini Coefficient (tenure diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device

#png("heatmap-menCoreBLAU.png",    # create PNG for the heat map
    png("heatmap-menCoreGINI.png",
    width = 5*300,        # 5 x 300 pixels
    height = 5*300,
    res = 300,            # 300 pixels per inch
    pointsize = 6)        # smaller font size
heatmap.2(mat_data2,
          cellnote = mat_data2,  # same data set for cell labels
          notecol="white",      # change font color of cell labels to black
          trace="none",         # turns off trace lines inside the heat map
          margins =c(6,8),     # widens margins around plot
          col=rev(coul),
          notecex=1.65,
          #breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="none",     # only draw a row dendrogram
          Rowv=FALSE,
          #xlab="Blau Index (gender diversity)",
          xlab="Gini Coefficient (tenure diversity)",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
          lhei=c(2.5, 8),
          lwid=c(1, 7, 1))
dev.off()               # close the PNG device
