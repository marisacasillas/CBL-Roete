#Loading packages and setting paths
library(ggplot2)
library(lme4)
library(grid)
library(gridExtra)
library(stringr)
library(jtools)
library(lattice)
library(plotrix)

#load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/03-raw_data/03-analysis_scripts/CBL-master/results/test/")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/03-raw_data/03-analysis_scripts/CBL-master/results/test/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask_keep_all-modified2.csv")

for(file in filenames){
  data <- read.csv(file,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE)
  data <- subset(data, select = c(2:13))
  new_filename <- str_replace(file,"-modified2","")
  write.csv(data, new_filename)
}

