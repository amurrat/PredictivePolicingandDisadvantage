
#load necessary libraries
library(ggplot2)
library(sf)
library(segregation) 
library(dplyr)
library(readr)
library(patchwork)



# get the data inside R: the raw stuff of the racial demographics for Decatur according to 2020 census
Dekalb_seg <- read.csv("DEKALB_SEG.csv")
#transform the csv into a matrix, and the matrix into long form 
ds <- as.matrix(Dekalb_seg)
dss <- matrix_to_long(ds, group = "race", unit = "Census_Blocktract")
print(dss)


#calculate the local segregation for each block group, gain Confidence Intervals
local_se <- mutual_local(dss, "race", "Census_Blocktract", weight = "n", wide = TRUE, 
                        se = TRUE,   n_bootstrap = 500 )


local_dis <- dissimilarity_expected(dss, group = c("white","black"), "Census_Blocktract", fixed_margins =  TRUE, n_bootstrap = 100)

#segregation visualization##
segviz <- segregation::segplot(dss, group = "race", unit = "Census_Blocktract", weight = "n", bar_space = 0.005) +
  plot_annotation(title = "Segregation in Decatur by Race")

##
disIndex <- dss %>%
  filter(race %in% c("White","Black")) %>%
  group_modify(~
                 dissimilarity(.x, 
                               group = "race",
                               unit = "Census_Blocktract",
                               weight = "n")) %>%
  arrange(desc(est))

disIndex
## DIS index for entire area for black white is 63%

local_se <- local_se[,-4]
local_se<- local_se [,-7]

?segregation
write.csv(local_se, "C:/Users/lanip/OneDrive/Documents/RFiles/DekalbSeg.csv", row.names = TRUE)





#run the dissimualrity index

# track it back to the data





