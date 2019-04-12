########## Supplementary materials analysis ##########

# ANALYSIS

## Local sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
subset.suppl.local.data <- subset(suppl.localdata,select=c(1,3,4,5,6,10,11,12,13))
subset.suppl.local.data$Y <- ifelse(subset.suppl.local.data$reconstructed == "True", 1,0)
subset.suppl.local.data <- subset(subset.suppl.local.data, suppl.localdata$skipped == "False")

# descriptive statistics
means_by_child_recon_perc <- aggregate(subset.suppl.local.data$Y, by = c(list(child = subset.suppl.local.data$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(subset.suppl.local.data$correctedscore, by = c(list(child = subset.suppl.local.data$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(subset.suppl.local.data$correctedscore, by = c(list(child = subset.suppl.local.data$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x)

# model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_local_uncorrected_suppl <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = subset.suppl.local.data)
sink("local_uncorrected_suppl.txt")
summary(model_local_uncorrected_suppl)
sink()
summary(model_local_uncorrected_suppl)
ranef(model_local_uncorrected_suppl)

# recenter child age
subset.suppl.local.data$recentered_age <- subset.suppl.local.data$age - 2.5

# model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_local_corrected_suppl <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = subset.suppl.local.data, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink("local_corrected_suppl.txt")
summary(model_local_corrected_suppl)
sink()
summary(model_local_corrected_suppl)
ranef(model_local_corrected_suppl)

## cumulative sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
subset.suppl.cumu.data <- subset(suppl.cumu.data,select=c(1,3,4,5,6,10,11,12,13))
subset.suppl.cumu.data$Y <- ifelse(subset.suppl.cumu.data$reconstructed == "True", 1,0)
subset.suppl.cumu.data <- subset(subset.suppl.cumu.data, suppl.cumu.data$skipped == "False")

# descriptive statistics
means_by_child_recon_perc <- aggregate(subset.suppl.cumu.data$Y, by = c(list(child = subset.suppl.cumu.data$child)),FUN = mean)
mean_of_means_by_child_recon_perc <- mean(means_by_child_recon_perc$x)*100
min <- min(means_by_child_recon_perc$x)*100
max <- max(means_by_child_recon_perc$x)*100

means_by_child_recon_score <- aggregate(subset.suppl.cumu.data$correctedscore, by = c(list(child = subset.suppl.cumu.data$child)),FUN = mean)
mean_of_means_by_child_recon_score <- mean(means_by_child_recon_score$x)
se_by_child_recon_score <- aggregate(subset.suppl.cumu.data$correctedscore, by = c(list(child = subset.suppl.cumu.data$child)),FUN = std.error)
mean_of_se_by_child_recon_score <- mean(se_by_child_recon_score$x)

# model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_cumu_uncorrected_suppl <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = subset.suppl.cumu.data)
sink("cumu_uncorrected_suppl.txt")
summary(model_cumu_uncorrected_suppl)
sink()
summary(model_cumu_uncorrected_suppl)
ranef(model_cumu_uncorrected_suppl)

# recenter child age
subset.suppl.cumu.data$recentered_age <- subset.suppl.cumu.data$age - 2.5

# model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_cumu_corrected_suppl <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = subset.suppl.cumu.data, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink("cumu_corrected_suppl.txt")
summary(model_cumu_corrected_suppl)
sink()
summary(model_cumu_corrected_suppl)
ranef(model_cumu_corrected_suppl)

# PLOTS

# 1) plot average controlled score over age, per child

## local sample
# collect data for plot
plot1.local.data <- aggregate(subset.suppl.local.data$controlledscore, by = c(list(child = subset.suppl.local.data$child),list(age=subset.suppl.local.data$age)),FUN = sum)
colnames(plot1.local.data)[3] <- "total_score"
plot1.local.data.temp <- aggregate(subset.suppl.local.data$controlledscore, by = c(list(child = subset.suppl.local.data$child),list(age = subset.suppl.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.local.data.temp)[3] <- "total_num_utterances"
plot1.data <- merge(plot1.local.data, plot1.local.data.temp, by = c("child","age"))
plot1.data$averagescore <- plot1.local.data$total_score/plot1.local.data$total_num_utterances

# generate plot
plot.local.suppl.recon_score <- ggplot(plot1.local.data, 
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
plot.local.suppl.recon_score+theme_apa()
dev.off()
plot.local.suppl.recon_score+theme_apa()

## cumulative sample

# collect data for plot
plot1.cumu.data <- aggregate(subset.suppl.cumu.data$controlledscore, by = c(list(child = subset.suppl.cumu.data$child),list(age=subset.suppl.cumu.data$age)),FUN = sum)
colnames(plot1.cumu.data)[3] <- "total_score"
plot1.cumu.data.temp <- aggregate(subset.suppl.cumu.data$controlledscore, by = c(list(child = subset.suppl.cumu.data$child),list(age = subset.suppl.cumu.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.cumu.data.temp)[3] <- "total_num_utterances"
plot1.cumu.data <- merge(plot1.cumu.data, plot1.cumu.data.temp, by = c("child","age"))
plot1.cumu.data$averagescore <- plot1.cumu.data$total_score/plot1.cumu.data$total_num_utterances

# generate plot
plot1.cumu.suppl.recon_score <- ggplot(plot1.cumu.data, 
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
          "plotsupplcumurecon_score.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumu.suppl.recon_score + theme_apa()
dev.off()
plot.cumu.suppl.recon_score + theme_apa()

# 2) plot average percentage of correctly reconstructed utterances over age, per child

## local sample
# collect data for plot
plot2.local.data <- aggregate(subset.suppl.local.data$Y, by = c(list(child = subset.suppl.local.data$child),list(age=subset.suppl.local.data$age)),FUN = sum)
colnames(plot2.local.data)[3] <- "total_num_corr_reconstructions"
plot2.local.data.temp <- aggregate(subset.suppl.local.data$Y, by = c(list(child = subset.suppl.local.data$child),list(age = subset.suppl.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot2.local.data.temp)[3] <- "total_num_utterances"
plot2.local.data <- merge(plot2.local.data, plot2.local.data.temp, by = c("child","age"))
plot2.local.data$percentages <- (plot2.local.data$total_num_corr_reconstructions/plot2.local.data$total_num_utterances)*100

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

## cumulative sample

# collect data for plot
plot2.cumu.data <- aggregate(subset.suppl.cumu.data$Y, by = c(list(child = subset.suppl.cumu.data$child),list(age=subset.suppl.cumu.data$age)),FUN = sum)
colnames(plot2.cumu.data)[3] <- "total_num_corr_reconstructions"
plot2.cumu.data.temp <- aggregate(subset.suppl.cumu.data$Y, by = c(list(child = subset.suppl.cumu.data$child),list(age = subset.suppl.cumu.data$age)), FUN = function(x){NROW(x)})
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


## Combine local and cumulative plot

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
plot.local.suppl.recon_score.noxtitle <- plot.local.suppl.recon_score +
  xlab("\n") +
  ylab("Average reconstruction score\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.cumu.suppl.recon_score.noxtitle <- plot.cumu.suppl.recon_score +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))

png(paste(plot.path,
          "suppl_bothreconstruction_score.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.suppl.recon_score.noxtitle,
                       plot.cumu.suppl.recon_score.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()
plot.both.reconscore.suppl <- arrange_related_x_axes(plot.local.suppl.recon_score.noxtitle,
                                                     plot.cumu.suppl.recon_score.noxtitle,
                                                     nrow=1, ncol = 2, as.table=TRUE,
                                                     sub="Age (years)")

ggsave(paste0(plot.path, "suppl_bothreconstruction_score.png"), plot = plot.both.reconscore.suppl)

# 2) plot 2: average reconstruction percentage x age, per child
plot.suppl.local.reconstruction_perc.noxtitle <- plot.suppl.local.reconstruction_perc +
  xlab("\n") +
  ylab("Percentage correctly\nreconstructed utterances\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.suppl.cumu.reconstruction_perc.noxtitle <- plot.suppl.cumu.reconstruction_perc +
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
                       plot.suppl.cumu.reconstruction_perc.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

plot.both.reconperc.suppl <- arrange_related_x_axes(plot.suppl.local.reconstruction_perc.noxtitle,
                                                    plot.suppl.cumu.reconstruction_perc.noxtitle,
                                                    nrow=1, ncol = 2, as.table=TRUE,
                                                    sub="Age (years)")

ggsave(paste0(plot.path, "suppl_bothreconperc.png"), plot = plot.both.reconperc.suppl)
