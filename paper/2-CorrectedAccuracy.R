########## Corrected accuracy for the local and cumulative samples ##########

# MAIN TEXT ANALYSES

## local sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not. 
subset.local.data <- subset(local.data,select=c(1,3,4,5,6,10,11,12))
subset.local.data$Y <- ifelse(subset.local.data$reconstructed == "True", 1,0)
subset.local.data <- subset(subset.local.data, subset.local.data$skipped == "False")

# Descriptive statistics
local_means_corrected_by_child <- aggregate(subset.local.data$correctedscore, by = c(list(child = subset.local.data$child)),FUN = mean)
local_mean_of_means_corrected_by_child <- mean(local_means_corrected_by_child$x)
local_se_corrected_by_child <- aggregate(subset.local.data$correctedscore, by = c(list(child = subset.local.data$child)),FUN = std.error)
local_mean_of_se_corrected_by_child <- mean(local_se_corrected_by_child$x)

# Recenter child age
subset.local.data$recentered_age <- subset.local.data$age - 2.5

# Model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_local_corrected <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = subset.local.data, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink(paste0(plot.path,"local_corrected_reconstruction.txt"))
summary(model_local_corrected)
sink()
summary(model_local_corrected)
ranef(model_local_corrected )

## cumulative sample

# Select subset of the data, excluding all skipped utterance, and generate binary reconstruction score:
# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is correctly reconstructed,
# Y=0 if not.
subset.cumu.data <- subset(cumu.data,select=c(1,3,4,5,6,10,11,12))
subset.cumu.data$Y <- ifelse(subset.cumu.data$reconstructed == "True", 1,0)
subset.cumu.data <- subset(subset.cumu.data, subset.cumu.data$skipped == "False")

# Descriptive statistics
cumu_means_corrected_by_child <- aggregate(subset.cumu.data$correctedscore, by = c(list(child = subset.cumu.data$child)),FUN = mean)
cumu_mean_of_means_corrected_by_child <- mean(cumu_means_corrected_by_child$x)
cumu_se_corrected_by_child <- aggregate(subset.cumu.data$correctedscore, by = c(list(child = subset.cumu.data$child)),FUN = std.error)
cumu_mean_of_se_corrected_by_child <- mean(cumu_se_corrected_by_child$x)

# Recenter child age
subset.cumu.data$recentered_age <- subset.cumu.data$age - 2.5

# Model with the corrected score as dependent variable, recentered age as independent variable (fixed effect), by-child random intercept and random slopes of age.
model_cumu_corrected <- lmer(correctedscore ~ recentered_age + (recentered_age|child), data = subset.cumu.data, control=lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1000000)))
sink(paste0(plot.path,"cumu_corrected_reconstruction.txt"))
summary(model_cumu_corrected)
sink()
summary(model_cumu_corrected)
ranef(model_cumu_corrected )

# MAIN TEXT PLOTS

# 1) Plot with x-axis: age, y-axis: average length-and-repetition controlled reconstruction score

## Local sample

# Collect data
plot1.local.data <- aggregate(subset.local.data$correctedscore, by = c(list(child = subset.local.data$child),list(age=subset.local.data$age)),FUN = sum)
colnames(plot1.local.data)[3] <- "total_score"
plot1.local.data.temp <- aggregate(subset.local.data$correctedscore, by = c(list(child = subset.local.data$child),list(age = subset.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.local.data.temp)[3] <- "total_num_utterances"
plot1.local.data <- merge(plot1.local.data, plot1.local.data.temp, by = c("child","age"))
plot1.local.data$averagescore <- plot1.local.data$total_score/plot1.local.data$total_num_utterances

# Generate plot 
plot.local.reconstruction <- ggplot(plot1.local.data, 
                                    aes(x=age, y = averagescore, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(-0.5,0.5))) + 
  xlab("\nAge (years)") + 
  ylab("Average reconstruction \nscore\n") + 
  ggtitle("Local sampling") + 
  geom_hline(yintercept=0) +
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

ggsave(paste0(plot.path, "plotlocalrecon.png"), plot = plot.local.reconstruction+theme_apa())

## Cumulative sample

# Collect data
plot1.cumu.data <- aggregate(subset.cumu.data$correctedscore, by = c(list(child = subset.cumu.data$child),list(age=subset.cumu.data$age)),FUN = sum)
colnames(plot1.cumu.data)[3] <- "total_score"
plot1.cumu.data.temp <- aggregate(subset.cumu.data$correctedscore, by = c(list(child = subset.cumu.data$child),list(age = subset.cumu.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.cumu.data.temp)[3] <- "total_num_utterances"
plot1.cumu.data <- merge(plot1.cumu.data, plot1.cumu.data.temp, by = c("child","age"))
plot1.cumu.data$averagescore <- plot1.cumu.data$total_score/plot1.cumu.data$total_num_utterances

# Generate plot
plot.cumu.reconstruction <- ggplot(plot1.cumu.data, 
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

ggsave(paste0(plot.path, "plotcumurecon.png"), plot = plot.cumu.reconstruction + theme_apa())

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
plot.local.reconstruction.noxtitle <- plot.local.reconstruction +
  xlab("\n") +
  ylab("Average reconstruction score\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.cumu.reconstruction.noxtitle <- plot.cumu.reconstruction +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.25))

png(paste(plot.path,
          "plotbothreconstruction.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.reconstruction.noxtitle,
                       plot.cumu.reconstruction.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()