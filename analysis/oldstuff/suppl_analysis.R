library(ggplot2)
library(lme4)
library(grid)
library(gridExtra)
library(stringr)
library(jtools)
library(lattice)
library(plotrix)

# plot layout settings
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

##--------Analysis of local sampled data----------##
# load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/localsampledcorpus/"

# open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask_keep_all-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

# data preprocessing
data$num = 1:nrow(data) # overwrite utterance number to avoid double numbers
data$age <- gsub("_", ".", data$age) #converting age variable to numeric values and months into years
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
##NOTE: uncomment the next line if you've generated your own set of model output files
#data <- subset(data, select = c(2:14))

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

# descriptive statistics
means_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x)

# statistical analysis

# model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
sink("uncorrected_reconstruction_local.txt")
summary(model_age_uncorrected)
sink()
summary(model_age_uncorrected)
ranef(model_age_uncorrected)

# recenter child age
newdata$recentered_age <- newdata$age - 2.5

# model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_recentered_age <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink("corrected_reconstruction_local_recentered_age.txt")
summary(model_recentered_age)
sink()
summary(model_recentered_age)
ranef(model_recentered_age)

# generate plots

# 1) plot average controlled score over age, per child

# collect data for plot
plot1.local.data <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plot1.local.data)[3] <- "total_score"
plot1.local.data.temp <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plot1.local.data.temp)[3] <- "total_num_utterances"
plotdata <- merge(plot1.local.data, plot1.local.data.temp, by = c("child","age"))
plotdata$averagescore <- plot1.local.data$total_score/plot1.local.data$total_num_utterances

# generate plot
plot.local.suppl <- ggplot(plot1.local.data, 
                                    aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(-0.5,0.5))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Local sampling") + 
  geom_hline(yintercept=0) +
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# save plot
png(paste(plot.path,
          "plotsuppllocalrecon_score.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.suppl+theme_apa()
dev.off()
plot.local.suppl+theme_apa()

# 2) plot average percentage of correctly reconstructed utterances over age, per child

# collect data for plot
plot2.local.data <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plot2.local.data)[3] <- "total_num_corr_reconstructions"
plot2.local.data.temp <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plot2.local.data.temp)[3] <- "total_num_utterances"
plotdata4 <- merge(plot2.local.data, plot2.local.data.temp, by = c("child","age"))
plotdata4$percentages <- (plot2.local.data$total_num_corr_reconstructions/plot2.local.data$total_num_utterances)*100

# generate plot
plot.suppl.local.reconstruction_perc <- ggplot(plot2.local.data, 
                                         aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# save plot
png(paste(plot.path,
          "plotsuppllocalrecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.suppl.local.reconstruction_perc+theme_apa()
dev.off()
plot.suppl.local.reconstruction_perc+theme_apa()


##--------Analysis of cumulative sampled data----------##
# load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask_keep_all-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

# data preprocessing
data$num = 1:nrow(data) # overwrite utterance number to avoid double numbers
data$age <- gsub("_", ".", data$age) #converting age variable to numeric values and months into years
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
##NOTE: uncomment the next line if you've generated your own set of model output files
#data <- subset(data, select = c(2:14))

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

# descriptive statistics
means_by_child_recon_perc <- aggregate(newdata$Y, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(newdata$correctedscore, by = c(list(child = newdata$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x)

# statistical analysis

# model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
sink("uncorrected_reconstruction_cumulative.txt")
summary(model_age_uncorrected)
sink()
summary(model_age_uncorrected)
ranef(model_age_uncorrected)

# recenter child age
newdata$recentered_age <- newdata$age - 2.5

# model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_recentered_age_cumulative <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink("corrected_reconstruction_cumulative_recentered_age.txt")
summary(model_recentered_age_cumulative)
sink()
summary(model_recentered_age_cumulative)
ranef(model_recentered_age_cumulative)

##TODO: REMOVE?
relgrad <- with(model_age_cumulative@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

# generate plots

# 1) plot average controlled score over age, per child

# collect data for plot
plot1.cumu.data <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plot1.cumu.data)[3] <- "total_score"
plot1.cumu.data.temp <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plot1.cumu.data.temp)[3] <- "total_num_utterances"
plot1.cumu.data <- merge(plot1.cumu.data, plot1.cumu.data.temp, by = c("child","age"))
plot1.cumu.data$averagescore <- plot1.cumu.data$total_score/plot1.cumu.data$total_num_utterances

# generate plot
plot.cumu.suppl.recon_score <- ggplot(plot1.cumu.data, 
                                  aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
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
plot.cumu.suppl.recon_score + theme_apa()
dev.off()
plot.cumu.suppl.recon_score + theme_apa()

# 2) plot average percentage of correctly reconstructed utterances over age, per child

# collect data for plot
plot2.cumu.data <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plot2.cumu.data)[3] <- "total_num_corr_reconstructions"
plot2.cumu.data.temp <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plot2.cumu.data.temp)[3] <- "total_num_utterances"
plot2.cumu.data <- merge(plot2.cumu.data, plot2.cumu.data.temp, by = c("child","age"))
plot2.cumu.data$percentages <- (plot2.cumu.data$total_num_corr_reconstructions/plot2.cumu.data$total_num_utterances)*100

# generate plot
plot.suppl.cumulative.reconstruction_perc <- ggplot(plot2.cumu.data, 
                                       aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# save plot
png(paste(plot.path,
          "plotsupplcumurecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.suppl.cumulative.reconstruction_perc+theme_apa()
dev.off()
plot.suppl.cumulative.reconstruction_perc+theme_apa()

##--------Generate combined plots with results from both sampling methods----------##

# function to merge two graphs that share a legend into one figure
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


# function to remove the x-axis of the right-hand side graph without distorted graph sizes, 
# the x-axis is the same as the x-axis of the left-hand side graph
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

# arrange two graphs (local and cumulative sampled data) into one figure

# 1) plot 1: average reconstruction score x age, per child
plot.local.suppl.noxtitle <- plot.local.suppl +
xlab("\n") +
  ylab("Average reconstruction score\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.acc.suppl.noxtitle <- plot.acc.suppl +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))

png(paste(plot.path,
          "suppl_bothreconstruction.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.suppl.noxtitle,
                       plot.acc.suppl.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()


# 2) plot 2: average reconstruction percentage x age, per child
plot.suppl.local.reconstruction_perc.noxtitle <- plot.suppl.local.reconstruction_perc +
  xlab("\n") +
  ylab("Percentage correctly\nreconstructed utterances\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.suppl.cumulative.reconstruction_perc.noxtitle <- plot.suppl.cumulative.reconstruction_perc +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))

png(paste(plot.path,
          "suppl_bothreconperc.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.suppl.local.reconstruction_perc.noxtitle,
                       plot.suppl.cumulative.reconstruction_perc.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()