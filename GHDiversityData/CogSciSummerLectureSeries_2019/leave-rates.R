## Diversity in GitHub
## Olivia B. Newton
## Summer 2019

## This code is for calculating proportion of leavers by 
## gender and other variables of interest.


# Load Packages -----------------------------------------------------------

library(data.table)
library(gplots) ## heatmap.2 is a gplots function
library(RColorBrewer)
library(tidyr)


# Import Data -------------------------------------------------------------

myDF <- fread("user_data.csv")
# myDF.core <- fread("user_data_CoreLabeled.csv")


# Subset Data -------------------------------------------------------------


rates <- myDF %>%
  subset(num_team > 1 & gender != "None" , 
         select = c("project_id", 
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "github_tenure",
                    "gender",
                    "leavesNextQ")) 

#rates.core <- myDF.core %>%
#  subset(num_team > 1 & gender != "None" & Core2 == 1, 
#         select = c("project_id", 
#                    "window_idx",
#                    "domain",
#                    "language",
#                    "num_team",
#                    "num_commits",
#                    "github_tenure",
#                    "gender",
#                    "leavesNextQ")) 

# Data for Men ------------------------------------------------------------

men.rates <- rates %>%
  subset(gender == "male")

menOverall <- men.rates %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "gender",
                    "leavesNextQ")
         ) %>%
  mutate(teamSize =  ## team size bins: 2	3-5	6-9	10+
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         commitSize = ## project size bins: 1-10 11-100 101-500 500-1000 1001-5000 5000-10000 10001-15000 15001-25000
           ifelse(num_commits < 11, 10,
                  ifelse(num_commits > 10 & num_commits < 101, 100,
                         ifelse(num_commits > 100 & num_commits < 501, 500,
                                ifelse(num_commits > 500 & num_commits < 1001, 1000,
                                       ifelse(num_commits > 1000 & num_commits < 5001, 5000,
                                              ifelse(num_commits > 5000 & num_commits < 10001, 10000,
                                                     ifelse(num_commits > 10000 & num_commits < 15001, 15000, 25000
                                                     )))))))
         ) %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "teamSize",
                    "commitSize",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)
         ) %>%
  ungroup #%>%
#  group_by(project_id, window_idx) %>%
#  mutate(propLeft = (Count/sum(Count))
#         ) %>%
#  subset(select = c("teamSize",
#                    "commitSize",
#                    "propLeft")
#         ) %>%
#  ungroup() %>%
#  unique() %>%
#  group_by(teamSize, commitSize) %>%
#  mutate(medPropLeft = median(propLeft)
#         #avgPropLeft = mean(propLeft)
#         ) %>%
#  subset(select = c("teamSize",
#                    "commitSize",
#                    #"propLeft"
#                    "medPropLeft"
#                    #"avgPropLeft"
#  )) %>%
#  ungroup %>%
#  unique()


## round to hundreths place
menOverall$medPropLeft <- round(menOverall$medPropLeft, 3)


# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

menOverall.WIDE <- spread(menOverall, commitSize, medPropLeft)

## reorder rows
menOverall.WIDE <- menOverall.WIDE[c(3,4,5,1,2),]


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
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  ## team size bins: 2	3-5	6-9	10+
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         commitSize = ## project size bins: 1-10 11-100 101-500 500-1000 1001-5000 5000-10000 10001-15000 15001-25000
           ifelse(num_commits < 11, 10,
                  ifelse(num_commits > 10 & num_commits < 101, 100,
                         ifelse(num_commits > 100 & num_commits < 501, 500,
                                ifelse(num_commits > 500 & num_commits < 1001, 1000,
                                       ifelse(num_commits > 1000 & num_commits < 5001, 5000,
                                              ifelse(num_commits > 5000 & num_commits < 10001, 10000,
                                                     ifelse(num_commits > 10000 & num_commits < 15001, 15000, 25000
                                                     )))))))) %>%
  subset(select = c("project_id", 
                    "window_idx",
                    "teamSize",
                    "commitSize",
                    "leavesNextQ")) %>%
  group_by(project_id, window_idx, teamSize, commitSize) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(project_id, window_idx, teamSize, commitSize) %>%
  mutate(propLeft = (Count/sum(Count))) %>%
  subset(select = c("teamSize",
                    "commitSize",
                    "propLeft")) %>%
  ungroup() %>%
  unique() %>%
  group_by(teamSize, commitSize) %>%
  mutate(medPropLeft = median(propLeft)
         #avgPropLeft = mean(propLeft)
  ) %>%
  subset(select = c("teamSize",
                    "commitSize",
                    #"propLeft"
                    "medPropLeft"
                    #"avgPropLeft"
  )) %>%
  ungroup %>%
  unique()

## round to hundreths place
womenOverall$medPropLeft <- round(womenOverall$medPropLeft, 3)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

womenOverall.WIDE <- spread(womenOverall, commitSize, medPropLeft)

womenOverall.WIDE <- womenOverall.WIDE[c(3,4,5,1,2),]

#########################################################
### B) Reading in data and transform it into matrix format
#########################################################


colnames(menOverall.WIDE)[colnames(menOverall.WIDE)=="teamSize"] <- "rnames"

rnames1 <- menOverall.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data1 <- data.matrix(menOverall.WIDE[,2:ncol(menOverall.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data1) <- unlist(rnames1)                  # assign row names


colnames(womenOverall.WIDE)[colnames(womenOverall.WIDE)=="teamSize"] <- "rnames"

rnames <- womenOverall.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(womenOverall.WIDE[,2:ncol(womenOverall.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data) <- unlist(rnames)                  # assign row names


#########################################################
### C) Customizing and plotting the heat map
#########################################################

coul = brewer.pal(4, "PRGn") 
## I can add more tones to this palette with the following code:
coul = colorRampPalette(coul)(50)

# creates a 5 x 5 inch image
png("heatmap-womenOverall.png",    # create PNG for the heat map        
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
          xlab="Number of Commits",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)), 
          lhei=c(2.5, 8), 
          lwid=c(1, 7, 1))  
dev.off()               # close the PNG device

png("heatmap-menOverall.png",    # create PNG for the heat map        
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
          xlab="Number of Commits",
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
  subset(select = c(
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  # 2	3-5	6-9	10+
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         commitSize = 
           ifelse(num_commits < 11, 10,
                  ifelse(num_commits > 10 & num_commits < 101, 100,
                         ifelse(num_commits > 100 & num_commits < 501, 500,
                                ifelse(num_commits > 500 & num_commits < 1001, 1000,
                                       ifelse(num_commits > 1000 & num_commits < 5001, 5000,
                                              ifelse(num_commits > 5000 & num_commits < 10001, 10000,
                                                     ifelse(num_commits > 10000 & num_commits < 15001, 15000, 25000
                                                     )))))))) %>%
  subset(select = c(
                    "teamSize",
                    "commitSize",
                    "leavesNextQ")) %>%
  group_by(teamSize, commitSize) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(teamSize, commitSize) %>%
  mutate(propLeft = (Count/sum(Count))) %>%
  subset(select = c("teamSize",
                    "commitSize",
                    "propLeft")) %>%
  ungroup() %>%
  unique() 


menOverall.core$propLeft <-  round(menOverall.core$propLeft, 4)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

menOverall.core.WIDE <- spread(menOverall.core, commitSize, propLeft)

menOverall.core.WIDE <- menOverall.core.WIDE[c(3,4,5,1,2),]


# Data for Women in Core --------------------------------------------------


women.core.rates <- rates.core %>%
  subset(gender == "female")

womenOverall.core <- women.core.rates %>%
  subset(select = c(
                    "domain",
                    "language",
                    "num_team",
                    "num_commits",
                    "gender",
                    "leavesNextQ")) %>%
  mutate(teamSize =  
           ifelse(num_team == 2, "2",
                  ifelse(num_team > 2 & num_team < 6, "3 to 5",
                         ifelse(num_team > 5 & num_team < 10, "6 to 9", 
                                ifelse(num_team > 9 & num_team < 16, "10 to 15", "16 or more"
                                )))),
         commitSize = 
           ifelse(num_commits < 11, 10,
                  ifelse(num_commits > 10 & num_commits < 101, 100,
                         ifelse(num_commits > 100 & num_commits < 501, 500,
                                ifelse(num_commits > 500 & num_commits < 1001, 1000,
                                       ifelse(num_commits > 1000 & num_commits < 5001, 5000,
                                              ifelse(num_commits > 5000 & num_commits < 10001, 10000,
                                                     ifelse(num_commits > 10000 & num_commits < 15001, 15000, 25000
                                                     )))))))) %>%
  subset(select = c(
                    "teamSize",
                    "commitSize",
                    "leavesNextQ")) %>%
  group_by(teamSize, commitSize) %>%
  mutate(Count = n(),
         Sum = sum(leavesNextQ)) %>%
  ungroup %>%
  group_by(teamSize, commitSize) %>%
  mutate(propLeft = (Count/sum(Count))) %>%
  subset(select = c("teamSize",
                    "commitSize",
                    "propLeft")) %>%
  ungroup() %>%
  unique() 
# %>%
#   group_by(teamSize, commitSize) %>%
#   mutate(medPropLeft = median(propLeft)
#          #avgPropLeft = mean(propLeft)
#   ) %>%
#   subset(select = c("teamSize",
#                     "commitSize",
#                     #"propLeft"
#                     "medPropLeft"
#                     #"avgPropLeft"
#   )) %>%
#   ungroup %>%
#   unique()

womenOverall.core$propLeft <-  round(womenOverall.core$propLeft, 4)

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

womenOverall.core.WIDE <- spread(womenOverall.core, commitSize, propLeft)

womenOverall.core.WIDE <- womenOverall.core.WIDE[c(3,4,5,1,2),]

#########################################################
### B) Reading in data and transform it into matrix format
#########################################################


colnames(menOverall.core.WIDE)[colnames(menOverall.core.WIDE)=="teamSize"] <- "rnames"

rnames2 <- menOverall.core.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data2 <- data.matrix(menOverall.core.WIDE[,2:ncol(menOverall.core.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data2) <- unlist(rnames2)                  # assign row names



colnames(womenOverall.core.WIDE)[colnames(womenOverall.core.WIDE)=="teamSize"] <- "rnames"

rnames3 <- womenOverall.core.WIDE[,1]                            # assign labels in column 1 to "rnames"
mat_data3 <- data.matrix(womenOverall.core.WIDE[,2:ncol(womenOverall.core.WIDE)])  # transform column 2-5 into a matrix
rownames(mat_data3) <- unlist(rnames3)                  # assign row names



#########################################################
### C) Customizing and plotting the heat map
#########################################################

coul = brewer.pal(4, "PRGn")
## I can add more tones to this palette with the following code:
coul = colorRampPalette(coul)(50)


# creates a 5 x 5 inch image
png("heatmap-womenCore.png",    # create PNG for the heat map        
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
          xlab="Number of Commits",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)), 
          lhei=c(2.5, 8), 
          lwid=c(1, 7, 1))  
dev.off()               # close the PNG device

png("heatmap-menCore.png",    # create PNG for the heat map        
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
          xlab="Number of Commits",
          ylab="Team Size",
          Colv="NA",
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)), 
          lhei=c(2.5, 8), 
          lwid=c(1, 7, 1))  
dev.off()  
