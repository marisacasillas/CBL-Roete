########## Uncorrected accuracy for the local and cumulative samples ##########

# MAIN TEXT ANALYSES

## Local sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
subset.local.data <- subset(local.data,select=c(1,3,4,5,6,10,11,12))
subset.local.data$Y <- ifelse(subset.local.data$reconstructed == "True", 1,0)
subset.local.data <- subset(subset.local.data, local.data$skipped == "False")

# Descriptive statistics
local_means_uncorrected_by_child <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child)),FUN = mean)
local_mean_of_means_uncorrected <- mean(local_means_uncorrected_by_child$x)*100
min <- min(local_means_uncorrected_by_child$x)*100
max <- max(local_means_uncorrected_by_child$x)*100

# Model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_local_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = subset.local.data)
sink("local_uncorrected_reconstruction.txt")
summary(model_local_uncorrected)
sink()
summary(model_local_uncorrected)
ranef(model_local_uncorrected)

## Cumulative sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not.
subset.cumu.data <- subset(cumu.data,select=c(1,3,4,5,6,10,11,12))
subset.cumu.data$Y <- ifelse(subset.cumu.data$reconstructed == "True", 1,0)
subset.cumu.data <- subset(subset.cumu.data, cumu.data$skipped == "False")

# Descriptive statistics
cumu_means_uncorrected_by_child <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child)),FUN = mean)
cumu_mean_of_means_uncorrected <- mean(cumu_means_uncorrected_by_child$x)*100
min <- min(cumu_means_uncorrected_by_child$x)*100
max <- max(cumu_means_uncorrected_by_child$x)*100

# Model with the binary uncorrected score as dependent variable, age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_cumu_uncorrected <- glmer(Y ~ age + (age|child), family=binomial(link = 'logit'), data = subset.cumu.data)
sink("cumu_uncorrected_reconstruction.txt")
summary(model_cumu_uncorrected)
sink()
summary(model_cumu_uncorrected)
ranef(model_cumu_uncorrected)

## MAIN TEXT PLOTS

# 1) Plot with x-axis: age, and y-axis: percentage of utterances with repetitions, collapsed over all children

## Local sample

# Collect data for plot

## TODO: FIX HERE
plot1.local.data <- aggregate(subset.local.data$repetition, by = c(list(age=subset.local.data$age)),FUN = sum)
colnames(plot1.local.data)[2] <- "total_num_repetitions"
plot1.local.data.temp <- aggregate(subset.local.data$repetition, by = c(list(age = subset.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.local.data.temp)[2] <- "total_num_utterances"
plot1.local.data<- merge(plot1.local.data, plot1.local.data.temp, by = c("age"))
plot1.local.data$percentages <- (plot1.local.data$total_num_repetitions/plot1.local.data$total_num_utterances)*100

# Generate plot
plot.local.repetitions_perc_collapsed <- ggplot(plot1.local.data, 
                                                aes(x=age, y = percentages)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# Save plot
png(paste(plot.path,
          "plotlocalrep_perc_collapsed.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.repetitions_perc_collapsed+theme_apa()
dev.off()
plot.local.repetitions_perc_collapsed+theme_apa()

## Cumulative sample

# Collect data for plot
plot1.cumu.data <- aggregate(subset.cumu.data$repetition, by = c(list(age=subset.cumu.data$age)),FUN = sum)
colnames(plot1.cumu.data)[2] <- "total_num_repetitions"
plot1.cumu.data.temp <- aggregate(subset.cumu.data$repetition, by = c(list(age = subset.cumu.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.cumu.data.temp)[2] <- "total_num_utterances"
plot1.cumu.data <- merge(plot1.cumu.data, plot1.cumu.data.temp, by = c("age"))
plot1.cumu.data$percentages <- (plot1.cumu.data$total_num_repetitions/plot1.cumu.data$total_num_utterances)*100

# Generate plot
plot.cumulative.repetitions_perc_collapsed <- ggplot(plot1.cumu.data, 
                                                     aes(x=age, y = percentages)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage of utterances\n containing repetitions\n") + 
  #ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# Save plot
png(paste(plot.path,
          "plotcumulativerep_perc_collapsed.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumulative.repetitions_perc_collapsed+theme_apa()
dev.off()
plot.cumulative.repetitions_perc_collapsed+theme_apa()

## Combine local and cumulative sample

# Generate plot
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

# # Save plot
# png(paste(plot.path,
#           "plotbothrep_perc_collapsed.png", sep=""),
#     width=900,height=500,units="px",
#     bg = "transparent")
# plot.both.repetitions_perc_collapsed+theme_apa()
# dev.off()
# plot.both.repetitions_perc_collapsed+theme_apa()

ggsave(paste0(plot.path, "plotbothrep_perc_collapsed.png"), plot = (plot.both.repetitions_perc_collapsed+theme_apa()))

# 2) Plot with x-axis age, y-axis percentage of correctly reconstructed utterances, per child

## Local sample

# Collect data for plot
plot2.local.data <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child),list(age=subset.local.data$age)),FUN = sum)
colnames(plot2.local.data)[3] <- "total_num_corr_reconstructions"
plot2.local.data.temp <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child),list(age = subset.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot2.local.data.temp)[3] <- "total_num_utterances"
plot2.local.data <- merge(plot2.local.data, plot2.local.data.temp, by = c("child","age"))
plot2.local.data$percentages <- (plot2.local.data$total_num_corr_reconstructions/plot2.local.data$total_num_utterances)*100

# Generate plot 
plot.local.reconstruction_perc <- ggplot(plot2.local.data, 
                                         aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  #scale_colour_manual(name = "Child:", values = groupcolours) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# Save plot
png(paste(plot.path,
          "plotlocalrecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.reconstruction_perc+theme_apa()
dev.off()
plot.local.reconstruction_perc+theme_apa()

## Cumulative sample

# Collect data for plot
plot2.cumu.data <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child),list(age=subset.cumu.data$age)),FUN = sum)
colnames(plot2.cumu.data)[3] <- "total_num_corr_reconstructions"
plot2.cumu.data.temp <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child),list(age = subset.cumu.data$age)), FUN = function(x){NROW(x)})
colnames(plot2.cumu.data.temp)[3] <- "total_num_utterances"
plot2.cumu.data <- merge(plot2.cumu.data, plot2.cumu.data.temp, by = c("child","age"))
plot2.cumu.data$percentages <- (plot2.cumu.data$total_num_corr_reconstructions/plot2.cumu.data$total_num_utterances)*100

# Generate plot 
plot.cumu.reconstruction_perc <- ggplot(plot2.cumu.data, 
                                       aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("Percentage correctly\n reconstructed utterances\n") + 
  ggtitle("Cumulative sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# Save plot
png(paste(plot.path,
          "plotcumurecon_perc.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumu.reconstruction_perc+theme_apa()
dev.off()
plot.cumu.reconstruction_perc+theme_apa()

## Combine local and cumulative sample

# Function to combine two plot that share a legend
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

# Function to remove the x-axis of the right-hand side graph without distorted graph sizes, 
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

# Generate combined plot
plot.local.repetitions_perc.noxtitle <- plot.local.repetitions_perc +
  xlab("\n") +
  ylab("% Utterances containing\nrepetitions\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.cumu.repetitions_perc.noxtitle <- plot.cumu.repetitions_perc +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.8))

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

plotboth_repetitions_perc <- arrange_related_x_axes(plot.local.repetitions_perc.noxtitle,
                                                    plot.cumu.repetitions_perc.noxtitle,
                                                    nrow=1, ncol = 2, as.table=TRUE,
                                                    sub="Age (years)")
ggsave(paste0(plot.path, "plotboth_repetitions_perc.png"), plot = plotboth_repetitions_perc)

