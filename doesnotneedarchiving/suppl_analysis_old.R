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

##Set colours
#groupcolours <- c("deepskyblue2","aquamarine3","aquamarine4","deepskyblue","deepskyblue3","lightskyblue")

#load the production task data
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/localsampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/localsampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask_keep_all-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data <- subset(data, select = c(2:14))

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

#Model with Y as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age <- lmer(correctedscore ~ age + (age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept 
#is included still
sink("corrected_reconstruction_analysis.txt")
summary(model_age)
sink()
summary(model_age)
ranef(model_age)

#Check for severeness of non-convergence:
#Following-up on non-convergence of the model: it's not a serious warning, see below:
relgrad <- with(model_age@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

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

#Make plot for poster
plotdata <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

# #Make plot 
# plot.local.suppl <- ggplot(plotdata, 
#                 aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
#   geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #scale_colour_manual(name = "Child:", values = groupcolours) + 
#   coord_cartesian(ylim=(c(-1,1))) + 
#   xlab("\nAge (years)") + 
#   ylab("Average reconstruction \nscore\n") + 
#   ggtitle("Local sampling") + 
#   geom_hline(yintercept=0) +
#   basic.theme + theme(axis.text.x = element_text(size=22)) +
#   theme(legend.position = "none") +
#   theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# plot.local.suppl+theme_apa()
# #Save plot
# #png(paste(plot.path,
# #          "posterplot.png", sep=""),
# #    width=900,height=500,units="px",
# #    bg = "transparent")
# #plot.local.suppl <-plot.local.suppl + theme_apa()
# #dev.off()
# #plot.local.suppl


plot.local.suppl <- ggplot(plotdata, 
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
plot.local.suppl+theme_apa()
dev.off()
plot.local.suppl+theme_apa()

## Other plot
##Make other plot
plotdata4 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_corr_reconstructions"
plotdata5 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_corr_reconstructions/plotdata4$total_num_utterances)*100

# #Make plot 
# plot.suppl.local.reconstruction_perc <- ggplot(plotdata4, 
#                                          aes(x=age, y = percentages, group = child, linetype = child, colour= child)) + 
#   geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #scale_colour_manual(name = "Child:", values = groupcolours) + 
#   coord_cartesian(ylim=(c(0,100))) + 
#   xlab("\nAge (years)") + 
#   ylab("Percentage correctly\n reconstructed utterances\n") + 
#   ggtitle("Local sampling") + 
#   basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "plotsuppllocalrecon_perc.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.suppl.local.reconstruction_perc+theme_apa()
# dev.off()
# plot.suppl.local.reconstruction_perc+theme_apa()

#Make plot 
plot.suppl.local.reconstruction_perc <- ggplot(plotdata4, 
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
          "plotsuppllocalrecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.suppl.local.reconstruction_perc+theme_apa()
dev.off()
plot.suppl.local.reconstruction_perc+theme_apa()

#Descriptive statistics
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

###CUMULATIVE DATA
#For cumulatively sampled corpus
setwd("/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus")
plot.path <- "/Volumes/ladd/workspaces/ld-ingroe/projects/modeling_roete/04-analysis/paper/supplementary_materials/accumulativesampledcorpus/"

#open and merge productiontask csvfiles
filenames <- list.files(pattern="*productiontask_keep_all-modified.csv")
data.list <- lapply(filenames,na.strings=c("NaN","Nan"), stringsAsFactors = FALSE,read.csv)
data <- do.call(rbind, data.list)

#overwrite utterance number to avoid double numbers
data$num = 1:nrow(data)
#converting age variable to numeric values and months into years
data$age <- gsub("_", ".", data$age)
data$age <- gsub("6", "5", data$age)
data$age <- as.numeric(data$age)
data <- subset(data, select = c(2:14))

#Analysis
#Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
#Y=0 if not. Exclude all skipped utterance.
newdata <- subset(data,select=c(1,3,4,5,6,10,11,12,13))
newdata$Y <- ifelse(newdata$reconstructed == "True", 1,0)
newdata <- subset(newdata, data$skipped == "False")

#Model with Y as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_age_cumulative <- lmer(correctedscore ~ age + (age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept 
#is included still
sink("corrected_reconstruction_analysis.txt")
summary(model_age_cumulative)
sink()
summary(model_age_cumulative)
ranef(model_age_cumulative)

#Following-up on non-convergence of the model: it's not a serious warning, see below:

relgrad <- with(model_age_cumulative@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

model_age_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = newdata)
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("uncorrected_reconstruction_cumulative.txt")
summary(model_age_uncorrected)
sink()
summary(model_age_uncorrected)
ranef(model_age_uncorrected)

## Rerun model with recentered age
newdata$recentered_age <- newdata$age - 2.5
#Model with controlledage as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_recentered_age_cumulative <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = newdata, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
#NOTE: the glmer package automatically turns (age|child) into (1+age|child), so the by-child random intercept
#is included still
sink("corrected_reconstruction_cumulative_recentered_age.txt")
summary(model_recentered_age_cumulative)
sink()
summary(model_recentered_age_cumulative)
ranef(model_recentered_age_cumulative)

#Make plot for poster
plotdata <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata)[3] <- "total_score"
plotdata2 <- aggregate(newdata$controlledscore, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata2)[3] <- "total_num_utterances"
plotdata <- merge(plotdata, plotdata2, by = c("child","age"))
plotdata$averagescore <- plotdata$total_score/plotdata$total_num_utterances

# #Make plot 
# plot.acc.suppl <- ggplot(plotdata, 
#                     aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
#   geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #scale_colour_manual(name = "Child:", values = groupcolours) + 
#   coord_cartesian(ylim=(c(-1,1))) + 
#   xlab("\nAge (years)") + 
#   ylab("Average reconstruction \nscore\n") + 
#   ggtitle("Cumulative sampling") + 
#   basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #theme(legend.position = "none") +
#   geom_hline(yintercept=0) +
#   theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "posterplot_cumulative.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.acc.suppl <- plot.acc.suppl + theme_apa()
# dev.off()
# plot.acc.suppl + theme_apa()

plot.acc.suppl <- ggplot(plotdata, 
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
plot.acc.suppl + theme_apa()
dev.off()
plot.acc.suppl + theme_apa()

##Make other plot
plotdata4 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age=newdata$age)),FUN = sum)
colnames(plotdata4)[3] <- "total_num_corr_reconstructions"
plotdata5 <- aggregate(newdata$Y, by = c(list(child = newdata$child),list(age = newdata$age)), FUN = function(x){NROW(x)})
colnames(plotdata5)[3] <- "total_num_utterances"
plotdata4 <- merge(plotdata4, plotdata5, by = c("child","age"))
plotdata4$percentages <- (plotdata4$total_num_corr_reconstructions/plotdata4$total_num_utterances)*100

#Make plot 
# plot.suppl.cumulative.reconstruction_perc <- ggplot(plotdata4, 
#                                               aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
#   geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   #scale_colour_manual(name = "Child:", values = groupcolours) + 
#   coord_cartesian(ylim=(c(0,100))) + 
#   xlab("\nAge (years)") + 
#   ylab("Percentage correctly\n reconstructed utterances\n") + 
#   ggtitle("Cumulative sampling") + 
#   basic.theme + theme(axis.text.x = element_text(size=22)) + 
#   theme(plot.title = element_text(size=32, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))
# 
# #Save plot
# png(paste(plot.path,
#           "plotsupplcumurecon_perc.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.suppl.cumulative.reconstruction_perc+theme_apa()
# dev.off()
# plot.suppl.cumulative.reconstruction_perc+theme_apa()

plot.suppl.cumulative.reconstruction_perc <- ggplot(plotdata4, 
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
          "plotsupplcumurecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.suppl.cumulative.reconstruction_perc+theme_apa()
dev.off()
plot.suppl.cumulative.reconstruction_perc+theme_apa()


#Descriptive statistics
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

### COMBINE FIGURES

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
# plot.local.suppl <- plot.local.suppl + theme_apa(x.font.size = 18, y.font.size = 18,
#                                                  legend.font.size = 18) + theme(legend.key.width = unit(2, "cm"),axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
#                                                  )
# plot.acc.suppl <- plot.acc.suppl + theme_apa(x.font.size = 18, y.font.size = 18,
#                                              legend.font.size = 18) + theme(legend.key.width = unit(2, "cm"),axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
#                                              )
# 
# plot.combined <- grid_arrange_shared_legend(plot.local.suppl, plot.acc.suppl, ncol = 2, nrow = 1, position = "right")
# 
# png(paste(plot.path,
#           "suppl_bothreconscore.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.combined <- grid_arrange_shared_legend(plot.local.suppl, plot.acc.suppl, ncol = 2, nrow = 1, position = "right")
# dev.off()
# plot.combined

plot.local.suppl.noxtitle <- plot.local.suppl +
xlab("\n") +
  ylab("Average reconstruction score\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.acc.suppl.noxtitle <- plot.acc.suppl +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +

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


# plot.suppl.local.reconstruction_perc <- plot.suppl.local.reconstruction_perc + theme_apa(x.font.size = 18, y.font.size = 18,
#                                                  legend.font.size = 18) + theme(legend.key.width = unit(2, "cm"),axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
#                                                  )
# plot.suppl.cumulative.reconstruction_perc <- plot.suppl.cumulative.reconstruction_perc + theme_apa(x.font.size = 18, y.font.size = 18,
#                                              legend.font.size = 18) + theme(legend.key.width = unit(2, "cm"),axis.text.x = element_text(size=18),axis.text.y = element_text(size=18)
#                                              )
# 
# plot.both.cumulative.reconstruction_perc <- grid_arrange_shared_legend(plot.suppl.local.reconstruction_perc , plot.suppl.cumulative.reconstruction_perc, ncol = 2, nrow = 1, position = "right")
# 
# png(paste(plot.path,
#           "suppl_bothreconperc.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.both.cumulative.reconstruction_perc <- grid_arrange_shared_legend(plot.suppl.local.reconstruction_perc, plot.suppl.cumulative.reconstruction_perc, ncol = 2, nrow = 1, position = "right")
# dev.off()
# plot.both.cumulative.reconstruction_perc

plot.suppl.local.reconstruction_perc.noxtitle <- plot.suppl.local.reconstruction_perc +
  xlab("\n") +
  ylab("Percentage correctly\nreconstructed utterances\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18) +
plot.suppl.cumulative.reconstruction_perc.noxtitle <- plot.suppl.cumulative.reconstruction_perc +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))
#  theme_apa(x.font.size = 18, y.font.size = 18,legend.font.size = 18)

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