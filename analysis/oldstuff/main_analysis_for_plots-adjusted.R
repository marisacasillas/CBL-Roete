#Loading packages and setting paths
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

#load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$numwords <- str_count(data$bow,",")+1
data <- subset(data, select = c(2:15))

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13,14))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

# #Model with controlledage as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
# model_age <- lmer(correctedscore ~ age + (age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
# #NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
# #is included still
# sink("corrected_reconstruction_local.txt")
# summary(model_age)
# sink()
# summary(model_age)
# ranef(model_age)
# 
# #Check for severeness of non-convergence:
# #Following-up on non-convergence of the model: it's not a serious warning, see below:
# relgrad <- with(model_age@optinfo$derivs,solve(Hessian,gradient))
# max(abs(relgrad))

#Model with Y (1: if correctly reconstructed) as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("uncorrected_reconstruction_local.txt")
summary(model_age_uncorrected)
sink()
summary(model_age_uncorrected)
ranef(model_age_uncorrected)

## Rerun model with recentered age
newdata$recentered_age <- newdata$age - 2.5
#Model with controlledage as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_recentered_age <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("corrected_reconstruction_local_recentered_age.txt")
summary(model_recentered_age)
sink()
summary(model_recentered_age)
ranef(model_recentered_age)


##Plot with x-axis: age, and y-axis: percentage of utterances with repetitions
newdata$repetition <- ifelse(newdata$repetition == "True", 1,0)
plotdata4 <- aggregate(newdata$repetition, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_repetitions"
plotdata5 <- aggregate(newdata$repetition, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_repetitions/plotdata4$total_num_utterances)*100

#Make plot 
plot.local.repetitions_perc <- ggplot(plotdata4, 
                                         aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:"), values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrep_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.repetitions_perc+theme_apa()
dev.off()
plot.local.repetitions_perc+theme_apa()

#Plot with x-axis age, y-axis absolute number of utterances containing repetitions
plot.local.repetitions_abs <- ggplot(plotdata4, 
                                      aes(x=age, y = total_num_repetitions, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  #coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Number of utterances\n containing repetitions\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrep_abs.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.repetitions_abs+theme_apa()
dev.off()
plot.local.repetitions_abs+theme_apa()

##Plot with x-axis: utterance length in words, and y-axis: percentage of corrected reconstructed utterances
plotdata_temp1 <- aggregate(newdata$Y, by = c(list(numwords = newdata$numwords)),FUN=sum)
colnames(plotdata_temp1)[2] <- "total_num_corr_reconstructions"
plotdata_temp2 <- aggregate(newdata$Y, by = c(list(numwords = newdata$numwords)), FUN = function(x){NROW(x)})
colnames(plotdata_temp2)[2] <- "total_num_utterances"
plotdata_1 <- merge(plotdata_temp1,plotdata_temp2, by = c("numwords"))
plotdata_1$percentages <- (plotdata_1$total_num_corr_reconstructions/plotdata_1$total_num_utterances)*100

#Make plot 
plot.local.reconstruction_numwords <- ggplot(data = plotdata_1, 
                                         aes(x=numwords, y = percentages)) + 
  geom_bar(stat = "identity") + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  xlab("\n Number of words in child utterance") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrecon_numwords.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction_numwords+theme_apa()
dev.off()
plot.local.reconstruction_numwords+theme_apa()

##Make plot: x-axis: age, y-axis: percentage of correctly reconstructed utterances
plotdata4 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_corr_reconstructions"
plotdata5 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_corr_reconstructions/plotdata4$total_num_utterances)*100

#Make plot 
plot.local.reconstruction_perc <- ggplot(plotdata4, 
                                    aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction_perc+theme_apa()
dev.off()
plot.local.reconstruction_perc+theme_apa()

#Make plot for paper: x-axis: age, y-axis: average length-and-repetition controlled reconstruction score
plotdata <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

#Make plot 
plot.local.reconstruction <- ggplot(plotdata, 
                                    aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-0.5,0.5))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Local sampling") + 
  geom_hline(yintercept=0) +
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrecon.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction+theme_apa()
dev.off()
plot.local.reconstruction+theme_apa()

#Compute average reconstruction score and average percentage of correctly reconstructed utterances 
means_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x)

# Plot x-axis: age, y-axis: percentage of utterances with repetitions, collapsed over all children
plotdata_local <- aggregate(newdata$repetition, by = c(list(age=newdata$age)),FUN = sum)
colnames(plotdata_local)[2] <- "total_num_repetitions"
plotdata_temp <- aggregate(newdata$repetition, by = c(list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata_temp)[2] <- "total_num_utterances"
plotdata_local<- merge(plotdata_local, plotdata_temp, by = c("age"))
plotdata_local$percentages <- (plotdata_local$total_num_repetitions/plotdata_local$total_num_utterances)*100

#Make plot 
plot.local.repetitions_perc_collapsed <- ggplot(plotdata_local, 
                                      aes(x=age, y = percentages)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  #ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocalrep_perc_collapsed.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.repetitions_perc_collapsed+theme_apa()
dev.off()
plot.local.repetitions_perc_collapsed+theme_apa()

###CUMULATIVE DATA
#For cumulatively sampled corpus
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$numwords <- str_count(data$bow,",")+1
data <- subset(data, select = c(2:15))

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13,14))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

##NOTE: As the model did not converge initially, here we use a different optimizer.
#Model with controlledage as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_cumulative <- lmer(correctedscore ~ age + (age|child), data = newdata, control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("corrected_reconstruction_cumulative.txt")
summary(model_age_cumulative)
sink()
summary(model_age_cumulative)
ranef(model_age_cumulative)

#Check for severeness of non-convergence:
#Following-up on non-convergence of the model: it's not a serious warning, see below:
relgrad <- with(model_age_cumulative@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

#Model with Y (1: if correctly reconstructed) as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("uncorrected_reconstruction_cumulative.txt")
summary(model_age_uncorrected)
sink()
summary(model_age_uncorrected)
ranef(model_age_uncorrected)

relgrad <- with(model_age_uncorrected@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

##Plot with x-axis: uage, and y-axis: percentage of utterances with repetitions
newdata$repetition <- ifelse(newdata$repetition == "True", 1,0)
plotdata4 <- aggregate(newdata$repetition, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_repetitions"
plotdata5 <- aggregate(newdata$repetition, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_repetitions/plotdata4$total_num_utterances)*100

#Make plot 
plot.cumu.repetitions_perc <- ggplot(plotdata4, 
                                      aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotcumurep_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumu.repetitions_perc+theme_apa()
dev.off()
plot.cumu.repetitions_perc+theme_apa()

#Plot with x-axis age, y-axis absolute number of utterances containing repetitions
plot.cumu.repetitions_abs <- ggplot(plotdata4, 
                                     aes(x=age, y = total_num_repetitions, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  #coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Number of utterances\n containing repetitions\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotcumurep_abs.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumu.repetitions_abs+theme_apa()
dev.off()
plot.cumu.repetitions_abs+theme_apa()

##Plot with x-axis: utterance length in words, and y-axis: percentage of corrected reconstructed utterances
plotdata_temp1 <- aggregate(newdata$Y, by = c(list(numwords = newdata$numwords)),FUN=sum)
colnames(plotdata_temp1)[2] <- "total_num_corr_reconstructions"
plotdata_temp2 <- aggregate(newdata$Y, by = c(list(numwords = newdata$numwords)), FUN = function(x){NROW(x)})
colnames(plotdata_temp2)[2] <- "total_num_utterances"
plotdata_1 <- merge(plotdata_temp1,plotdata_temp2, by = c("numwords"))
plotdata_1$percentages <- (plotdata_1$total_num_corr_reconstructions/plotdata_1$total_num_utterances)*100

#Make plot 
plot.acc.reconstruction_numwords <- ggplot(data = plotdata_1, 
                                             aes(x=numwords, y = percentages)) + 
  geom_bar(stat = "identity") + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  xlab("\n Number of words in child utterance") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotaccrecon_numwords.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.reconstruction_numwords+theme_apa()
dev.off()
plot.acc.reconstruction_numwords+theme_apa()

##Plot: x-axis: age, y-axis: percentage correctly reconstructed utterances
plotdata4 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_corr_reconstructions"
plotdata5 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_corr_reconstructions/plotdata4$total_num_utterances)*100

#Make plot 
plot.acc.reconstruction_perc <- ggplot(plotdata4, 
                                         aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotaccrecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.reconstruction_perc+theme_apa()
dev.off()
plot.acc.reconstruction_perc+theme_apa()

#Plot: x-axis: age, y-axis: average length-and-repetition controlled score
plotdata <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

#Make plot 
plot.acc.reconstruction <- ggplot(plotdata, 
                                  aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(-0.5,0.5))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Cumulative sampling") + 
  geom_hline(yintercept=0) +
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotaccrecon.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.reconstruction + theme_apa()
dev.off()
plot.acc.reconstruction + theme_apa()

#Compute average reconstruction score and average percentage of correctly reconstructed utterances
means_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
sd_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = sd)
mean_of_sd_by_child_recon_perc <- mean(sd_by_child_recon_perc$x)*100
se_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_recon_perc <- mean(se_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
sd_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = sd)
mean_of_sd_by_child_recon_score <- mean(sd_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x) 

newdata$recentered_age <- newdata$age - 2.5
#Model with controlledage as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_recentered_age_cumu <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("corrected_reconstruction_cumu_recentered_age.txt")
summary(model_recentered_age_cumu)
sink()
summary(model_recentered_age_cumu)
ranef(model_recentered_age_cumu)

#Plot x-axis: age, y-axis: percentage of utterances with repetitions, collapsed over all children
plotdata_cumu <- aggregate(newdata$repetition, by = c(list(age=newdata$age)),FUN = sum)
colnames(plotdata_cumu)[2] <- "total_num_repetitions"
plotdata_temp <- aggregate(newdata$repetition, by = c(list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata_temp)[2] <- "total_num_utterances"
plotdata_cumu <- merge(plotdata_cumu, plotdata_temp, by = c("age"))
plotdata_cumu$percentages <- (plotdata_cumu$total_num_repetitions/plotdata_cumu$total_num_utterances)*100

#Make plot 
plot.cumulative.repetitions_perc_collapsed <- ggplot(plotdata_cumu, 
                                                aes(x=age, y = percentages)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  #ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotcumulativerep_perc_collapsed.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumulative.repetitions_perc_collapsed+theme_apa()
dev.off()
plot.cumulative.repetitions_perc_collapsed+theme_apa()

#COMBINE LOCAL + CUMULATIVE PLOTS
plot.both.repetitions_perc_collapsed <- ggplot() +
  geom_line(data = plotdata_local, aes(x=age, y = percentages, color = "local"), lwd = 2) +
  geom_line(data = plotdata_cumu, aes(x=age, y = percentages, color = "cumulative"), lwd = 2) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) +
  coord_cartesian(ylim=(c(0,50))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  ggtitle("Chunk repetitions in child utterances") + 
  guides(color = guide_legend(reverse = TRUE)) +
  basic.theme + theme(axis.text.x = element_text(size=22)) +
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt"))) +
  guides(color=guide_legend(title="Sample"))

#Save plot
png(paste(plot.path,
          "plotbothrep_perc_collapsed.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.both.repetitions_perc_collapsed+theme_apa()
dev.off()
plot.both.repetitions_perc_collapsed+theme_apa()


#set working directory and output directory
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus/"

#open and merge csvfiles
filenames <- list.files(pattern="productiontask-modified*.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$utterance <- as.character(data$utterance)
data$numwords <- (str_count(data$utterance,"Word")-1)
data <- subset(data, select = c(2:15))

#For analysis:
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
newdata <- subset(data,select=c(1,3,4,5,6,12,14))
newdata$Y <- ifelse(newdata$skipped == "True", 1,0)


#### HERE CORRECTED FOR NUMBER OF WORDS PER UTTERANCE ####
model_numwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child)
sink("corrected_unknown_local.txt")
summary(model_numwords)
sink()
summary(model_numwords)
ranef(model_numwords)

#Plot figure 2
#Compute percentage of skipped utterances per child per age
aggdata <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
aggdata2 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(aggdata)[3] <- "absolute_score"
colnames(aggdata2)[3] <- "total_num_utterances"
total <- merge(aggdata, aggdata2, by = c("child","age"))
total$percentages <- (total$absolute_score / total$total_num_utterances)*100

#Make plot
plot.local.unknown <- ggplot(total, 
                             aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) +
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "plotlocalunknown.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.unknown + theme_apa()
dev.off()
plot.local.unknown + theme_apa()

#Compute average and sd percentage of utterances containing unknown words
means_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_unknown_perc <- mean(means_by_child_unknown_perc$x)*100
sd_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = sd)
mean_of_sd_by_child_unknown_perc <- mean(sd_by_child_unknown_perc$x)*100
se_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_unknown_perc <- mean(se_by_child_unknown_perc$x)*100

##For cumulatively sampled corpus
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/accumulativesampledcorpus/"

#open and merge csvfiles
filenames <- list.files(pattern="productiontask-modified*.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$utterance <- as.character(data$utterance)
data$numwords <- (str_count(data$utterance,"Word")-1)
data <- subset(data, select = c(2:15))

#For analysis:
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
newdata <- subset(data,select=c(1,3,4,5,6,12,14))
newdata$Y <- ifelse(newdata$skipped == "True", 1,0)

#### HERE CORRECTED FOR NUMBER OF WORDS PER UTTERANCE ####
model_numwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child)
sink("corrected_unknown_cumulative.txt")
summary(model_numwords)
sink()
summary(model_numwords)
ranef(model_numwords)

#Plot figure 2
#Compute percentage of skipped utterances per child per age
aggdata <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = sum)
aggdata2 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(aggdata)[3] <- "absolute_score"
colnames(aggdata2)[3] <- "total_num_utterances"
total <- merge(aggdata, aggdata2, by = c("child","age"))
total$percentages <- (total$absolute_score / total$total_num_utterances)*100

#Make plot
plot.acc.unknown <- ggplot(total, 
                           aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) +
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))


#Save plot
png(paste(plot.path,
          "plotaccunknown.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.acc.unknown + theme_apa()
dev.off()
plot.acc.unknown + theme_apa()

#Compute average and sd percentage of utterances containing unknown words
means_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_unknown_perc <- mean(means_by_child_unknown_perc$x)*100
sd_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = sd)
mean_of_sd_by_child_unknown_perc <- mean(sd_by_child_unknown_perc$x)*100
se_by_child_unknown_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_unknown_perc <- mean(se_by_child_unknown_perc$x)*100


## Code for age X utterancelength-plot
#load the child utterance data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/03-raw_data/03-analysis_scripts/CBL-master/localsampledcorpus/childcopy/")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/results/localsampledcorpus/"

#open file and extract child name and age
filenames <- list.files(pattern="*.txt")
data <- NULL
for (file in filenames){
  temp.data <- read.delim(file)
  colnames(temp.data) <- c("utterance")
  temp.data$child <- unlist(strsplit(unlist(strsplit(file, "_age"))[1],"child"))[2]
  temp.data$age <- unlist(strsplit(unlist(strsplit(file, "_age"))[2],".txt"))
  data <- rbind(data,temp.data)
}

#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data$numwords <- data$numwords <- str_count(data$utterance," ")

#plot
##Make plot: x-axis: age, y-axis: utterance length in words, collapsed over all children
plotdata_local_uttlength <- aggregate(data$numwords, by = c(list(age=data$age)),FUN=sum)
colnames(plotdata_local_uttlength)[2] <- "total_uttlength"
plotdata_temp <- aggregate(data$numwords, by = c(list(age=data$age)), FUN = function(x){NROW(x)})
colnames(plotdata_temp)[2] <- "total_num_utterances"
plotdata_local_uttlength <- merge(plotdata_local_uttlength,plotdata_temp, by = c("age"))
plotdata_local_uttlength$averagelength <- (plotdata_local_uttlength$total_uttlength/plotdata_temp$total_num_utterances)

#TODO:
#Compute range of utterance lengths, mean and median utterance length
min <- min(data$numwords)
max <- max(data$numwords)
mean <- mean(data$numwords)
median <- median(data$numwords)

#Make plot
plot.local.uttlength <- ggplot(plotdata_local_uttlength, 
                               aes(x=age, y = averagelength)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Average number of words \n in child utterance\n") + 
  ggtitle("Child utterance length ") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

#Save plot
png(paste(plot.path,
          "plotlocaluttlength.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.uttlength+theme_apa()
dev.off()
plot.local.uttlength+theme_apa()

### MERGE PLOTS

# COMBINE plot.local.reconstruction &  plot.cumu.reconstrucion

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

arrange_related_x_axes <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE, 
                    main=NULL, sub=NULL, plot=TRUE) { 
  dots <- list(...) 
  n <- length(dots) 
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)} 
  if(is.null(nrow)) { nrow = ceiling(n/ncol)} 
  if(is.null(ncol)) { ncol = ceiling(n/nrow)} 
  fg <- frameGrob(layout=grid.layout(nrow,ncol)) 
  ii.p <- 1 
  for(ii.row in seq(1, nrow)){ 
    ii.table.row <- ii.row       
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1} 
    for(ii.col in seq(1, ncol)){ 
      ii.table <- ii.p 
      if(ii.p > n) break 
      fg <- placeGrob(fg, ggplotGrob(dots[[ii.table]]), 
                      row=ii.table.row, col=ii.col) 
      ii.p <- ii.p + 1 
    } 
  } 
  if(!is.null(main) | !is.null(sub)){ 
    g <- frameGrob() # large frame to place title(s) and content 
    g <- packGrob(g, fg) 
    if (!is.null(main)) 
      g <- packGrob(g, textGrob(main), side="top") 
    if (!is.null(sub)) 
      g <- packGrob(g, textGrob(sub, gp=gpar(fontsize=30), vjust=-1, hjust=0), side="bottom") 
  } else { 
    g <- fg 
  } 
  if(plot) grid.draw(g) 
  invisible(g) 
} 

#TODO: How to make these two graphs of similar sizes, even though only the right one has a legend (plus, the legend looks funky)
# MC: done
plot.local.uttlength.noxtitle <- plot.local.uttlength +
  xlab("\n") +
  ylab("Average number of words\nin child utterance\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18))
#  theme_apa(x.font.size = 18, y.font.size = 18, legend.font.size = 18) +
plot.both.repetitions_perc_collapsed.noxtitle <- plot.both.repetitions_perc_collapsed +
  xlab("\n") +
  ylab("\n\nPercentage of utterances\ncontaining repetitions\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.9))
#  theme_apa(x.font.size = 18, y.font.size = 18, legend.font.size = 18) +

png(paste(plot.path,
          "plotbothfactors.png", sep=""),
    width=1500,height=500,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.uttlength.noxtitle,
                       plot.both.repetitions_perc_collapsed.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

#TODO: Remove axes-labels from right plot without distorting the individual graphs' sizes.
# MC: done
plot.local.reconstruction.noxtitle <- plot.local.reconstruction +
  xlab("\n") +
  ylab("Average reconstruction score\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.acc.reconstruction.noxtitle <- plot.acc.reconstruction +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +

png(paste(plot.path,
          "plotbothreconstruction.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.reconstruction.noxtitle,
                       plot.acc.reconstruction.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

#TODO: Remove axes-labels from right plot without distorting the individual graphs' sizes.
# MC: done
plot.local.reconstruction_perc.noxtitle <- plot.local.reconstruction_perc +
  xlab("\n") +
  ylab("Percentage correctly\nreconstructed utterances\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.acc.reconstruction_perc.noxtitle <- plot.acc.reconstruction_perc +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18)

png(paste(plot.path,
          "plotbothreconstruction_perc.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.reconstruction_perc.noxtitle,
                       plot.acc.reconstruction_perc.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

#TODO: Remove axes-labels from right plot without distorting the individual graphs' sizes.
# MC: done
plot.local.unknown.noxtitle <- plot.local.unknown +
  xlab("\n") +
  ylab("% Utterances containing\nnew words\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.acc.unknown.noxtitle <- plot.acc.unknown +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.8))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +

png(paste(plot.path,
          "plotbothunknown.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.unknown.noxtitle,
                       plot.acc.unknown.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

plot.local.reconstruction_numwords <- plot.local.reconstruction_numwords + theme_apa(x.font.size = 18, y.font.size = 18,
                                                               legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                               )
plot.acc.reconstruction_numwords <- plot.acc.reconstruction_numwords + theme_apa(x.font.size = 18, y.font.size = 18,
                                                 legend.font.size = 18) + theme(axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
                                                 )
plot.both.unknown <- grid.arrange(plot.local.reconstruction_numwords, plot.acc.reconstruction_numwords, ncol=2)#grid_arrange_shared_legend(plot.local.reconstruction_numwords, plot.acc.reconstruction_numwords, ncol = 2, nrow = 1)#, position = "right")

png(paste(plot.path,
          "plotboth_reconstruction_numwords.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.both.reconstruction_numwords <- grid.arrange(plot.local.reconstruction_numwords, plot.acc.reconstruction_numwords, ncol=2)
dev.off()
plot.both.reconstruction_numwords

#TODO: Remove axes-labels from right plot without distorting the individual graphs' sizes.
# MC: done
plot.local.repetitions_perc.noxtitle <- plot.local.repetitions_perc +
  xlab("\n") +
  ylab("% Utterances containing\nrepetitions\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.cumu.repetitions_perc.noxtitle <- plot.cumu.repetitions_perc +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.8))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +

png(paste(plot.path,
          "plotboth_repetitions_perc.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.repetitions_perc.noxtitle,
                       plot.cumu.repetitions_perc.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

#TODO: Remove axes-labels from right plot without distorting the individual graphs' sizes.
# MC: done
plot.local.repetitions_abs.noxtitle <- plot.local.repetitions_abs +
  xlab("\n") +
  ylab("Number of utterances\ncontaining repetitions\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.cumu.repetitions_abs.noxtitle  <- plot.cumu.repetitions_abs +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.8))
#    theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +

png(paste(plot.path,
          "plotboth_repetitions_abs.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.repetitions_abs.noxtitle,
                       plot.cumu.repetitions_abs.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()
