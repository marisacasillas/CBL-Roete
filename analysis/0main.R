library(ggplot2)
library(lme4)
library(grid)
library(gridExtra)
library(stringr)
library(jtools)
library(lattice)
library(plotrix)

#Plot layout settings
basic.theme <- theme(
  panel.background = element_rect(
    fill = "transparent",colour = NA),
  panel.grid.major = element_line(colour = "grey95"),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    fill = "transparent",colour = NA),
  legend.background = element_rect(
    fill="transparent"),
  legend.text = element_text(size=24),
  legend.title = element_text(size=30),
  legend.key.height = unit(2, "lines"),
  legend.key = element_rect(colour = NA, fill = NA),
  axis.text.x = element_text(size=30, angle=45, hjust=1),
  axis.title.x = element_text(size=30),
  axis.text.y = element_text(size=28),
  axis.title.y = element_text(size=32),
  strip.text = element_text(size=30),
  panel.spacing = unit(2, "lines"))

# Set this source file's directory as the working directory
# NOTE: If you want the following command to work, use Source in Rstudio rather than Run
here <- dirname(parent.frame(2)$ofile)
setwd(here)

# Global variables
rawdata.local.path <- "data/main/local/"
rawdata.cumulative.path <- "data/main/cumulative/"
childuttdata.path <- "data/main/childutt/"
suppl.rawdata.local.path <- "data/suppl/local/"
suppl.rawdata.cumulative.path <- "data/suppl/cumulative/"
plot.path <- "plots/"
print.model.output <- "Y"

# Read in local simulation data
filenames <- list.files(path = rawdata.local.path, pattern="*productiontask-modified.csv")
local.data.list <- lapply(paste0(rawdata.local.path,filenames),na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
local.data <- do.call(rbind, local.data.list)

# Prepare data for analyses
local.data$num = 1:nrow(local.data) #overwrite utterance number to avoid double numbers
local.data$age <- gsub("_", ".", local.data$age) #converting age variable to numeric values and months into years
local.data$age <- gsub("6", "5", local.data$age)
local.data$age <- as.numeric(local.data$age)
local.data <- subset(local.data, select = c(2:13))

# Read in cumulative simulation data
filenames <- list.files(path = rawdata.cumulative.path, pattern="*productiontask-modified.csv")
cumu.data.list <- lapply(paste0(rawdata.cumulative.path,filenames),na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
cumu.data <- do.call(rbind, cumu.data.list)

# Prepare data for analyses
cumu.data$num = 1:nrow(cumu.data) #overwrite utterance number to avoid double numbers
cumu.data$age <- gsub("_", ".", cumu.data$age) #converting age variable to numeric values and months into years
cumu.data$age <- gsub("6", "5", cumu.data$age)
cumu.data$age <- as.numeric(cumu.data$age)
cumu.data <- subset(cumu.data, select = c(2:13))

# Read in child utterances from input data
filenames <- list.files(path=childuttdata.path, pattern="*.txt")

childutt.data <- NULL
for (file in filenames){
  temp.data <- read.delim(paste0(childuttdata.path, file))
  colnames(temp.data) <- c("utterance")
  temp.data$child <- unlist(strsplit(unlist(strsplit(file, "_age"))[1],"child"))[2]
  temp.data$age <- unlist(strsplit(unlist(strsplit(file, "_age"))[2],".txt"))
  childutt.data <- rbind(childutt.data,temp.data)
}

# Prepare data for analyses
childutt.data$age <- gsub("_", ".", childutt.data$age) #converting age variable to numeric values and months into years
childutt.data$age <- gsub("6", "5", childutt.data$age)
childutt.data$age <- as.numeric(childutt.data$age)
childutt.data$numwords <- childutt.data$numwords <- str_count(childutt.data$utterance," ")

## Read in local sample data for suppl materials
filenames <- list.files(path = suppl.rawdata.local.path,pattern="*productiontask_keep_all-modified.csv")
suppl.local.data.list <- lapply(paste0(suppl.rawdata.local.path,filenames),na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
suppl.local.data <- do.call(rbind, suppl.local.data.list)

# Prepare data for analyses
suppl.local.data$num = 1:nrow(suppl.local.data) # overwrite utterance number to avoid double numbers
suppl.local.data$age <- gsub("_", ".", suppl.local.data$age) #converting age variable to numeric values and months into years
suppl.local.data$age <- gsub("6", "5", suppl.local.data$age)
suppl.local.data$age <- as.numeric(suppl.local.data$age)
suppl.local.data <- subset(suppl.local.data, select = c(2:13))

## Read in cumulative sample data for suppl materials
filenames <- list.files(path = suppl.rawdata.cumulative.path,pattern="*productiontask_keep_all-modified.csv")
suppl.cumu.data.list <- lapply(paste0(suppl.rawdata.cumulative.path,filenames),na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
suppl.cumu.data <- do.call(rbind, suppl.cumu.data.list)

# Prepare data for analyses
suppl.cumu.data$num = 1:nrow(suppl.cumu.data) # overwrite utterance number to avoid double numbers
suppl.cumu.data$age <- gsub("_", ".", suppl.cumu.data$age) #converting age variable to numeric values and months into years
suppl.cumu.data$age <- gsub("6", "5", suppl.cumu.data$age)
suppl.cumu.data$age <- as.numeric(suppl.cumu.data$age)
suppl.cumu.data <- subset(suppl.cumu.data, select = c(2:13))

# Run models and generate plots
#source("1-UncorrectedAccuracy.R")  #TODO: check analysis output
#source("2-CorrectedAccuracy.R") #TODO: check analysis output/ check numwords variable
source("3-UnseenWords.R") #TODO: check analysis output/ check numwords variable
source("4-ChilduttAnalysis.R") #CHECKED: CORRECT!
source("5-SupplMaterials.R") #CHECKED: CORRECT

# Print model output if requested in the global variables
if (print.model.output == "Y") {
  # Uncorrected accuracy
  print ("##### Uncorrected accuracy: Local #####")
  print(summary(model_local_uncorrected))
  print ("##### Uncorrected accuracy: Cumulative #####")
  print(summary(model_cumu_uncorrected))
  print ("##### Uncorrected accuracy: Local (original Mc & C) #####")
  print(summary(model_local_uncorrected_suppl))
  print ("##### Uncorrected accuracy: Cumulative (original Mc & C) #####")
  print(summary(model_cumu_uncorrected_suppl))
  # Corrected accuracy
  print ("##### Corrected accuracy: Local #####")
  print(summary(model_local_corrected))
  print ("##### Corrected accuracy: Cumulative #####")
  print(summary(model_cumu_corrected))
  print ("##### Corrected accuracy: Local (original Mc & C) #####")
  print(summary(model_local_corrected_suppl))
  print ("##### Corrected accuracy: Cumulative (original Mc & C) #####")
  print(summary(model_cumu_corrected_suppl))
  # Unseen words
  print ("##### Unseen words: Local #####")
  print(summary(model_local_unseenwords))
  print ("##### Unseen words: Cumulative #####")
  print(summary(model_cumu_unseenwords))
}
