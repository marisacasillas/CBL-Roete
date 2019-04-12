########## Unseen words for the local and cumulative samples ##########

# MAIN TEXT ANALYSES

## local sample
local.data$utterance <- as.character(local.data$utterance)
local.data$numwords <- (str_count(local.data$utterance,"Word")-1)

# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
subset.local.data <- subset(local.data,select=c(1,3,4,5,6,12,14))
subset.local.data$Y <- ifelse(subset.local.data$skipped == "True", 1,0)

# Descriptive statistics
local_means_unseen_by_child <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child)),FUN = mean)
local_mean_of_means_unseen <- mean(local_means_unseen_by_child$x)*100
local_se_unseen_by_child <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child)),FUN = std.error)
local_mean_of_se_unseen <- mean(local_se_unseen_by_child$x)*100

# Model with the binary skipped/not skipped as dependent variable, age and number of word in an utterance as independent 
# variables (fixed effect), by-child random intercept and random slopes of age.
model_local_unseenwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = subset.local.data)
sink("local_unseenwords.txt")
summary(model_local_unseenwords)
sink()
summary(model_local_unseenwords)
ranef(model_local_unseenwords)

## cumulative sample
cumu.data$utterance <- as.character(cumu.data$utterance)
cumu.data$numwords <- (str_count(cumu.data$utterance,"Word")-1)

# Yi,j,k for the i-th utterance of the j-th child at age k: Y = 1 if utterance is skipped, Y=0 if not skipped.
subset.cumu.data <- subset(cumu.data,select=c(1,3,4,5,6,12,14))
subset.cumu.data$Y <- ifelse(subset.cumu.data$skipped == "True", 1,0)

# Descriptive statistics
cumu_means_unseen_by_child <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child)),FUN = mean)
cumu_mean_of_means_unseen <- mean(cumu_means_unseen_by_child$x)*100
cumu_se_unseen_by_child <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child)),FUN = std.error)
cumu_mean_of_se_unseen <- mean(cumu_se_unseen_by_child$x)*100

# Model with the binary skipped/not skipped as dependent variable, age and number of word in an utterance as independent 
# variables (fixed effect), by-child random intercept and random slopes of age.
model_cumu_unseenwords <- glmer(Y ~ age + (age|child) + numwords, family=binomial(link = 'logit'), data = subset.cumu.data)
sink("cumu_unseenwords.txt")
summary(model_cumu_unseenwords)
sink()
summary(model_cumu_unseenwords)
ranef(model_cumu_unseenwords)

# MAIN TEXT PLOTS

# 1) Plot with x-axis= age and y-axis = percentage of skipped utterances, per child

## local sample

# collect data
plot1.local.data <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child),list(age = subset.local.data$age)), FUN = sum)
plot1.local.data.temp <- aggregate(subset.local.data$Y, by = c(list(child = subset.local.data$child),list(age = subset.local.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.local.data)[3] <- "absolute_score"
colnames(plot1.local.data.temp)[3] <- "total_num_utterances"
plot1.local.data <- merge(plot1.local.data, plot1.local.data.temp, by = c("child","age"))
plot1.local.data$percentages <- (plot1.local.data$absolute_score / plot1.local.data$total_num_utterances)*100

# generate plot
plot.local.unseen <- ggplot(plot1.local.data, 
                             aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("Local sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# save plot 

png(paste(plot.path,
          "plotlocalunseen.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.local.unseen + theme_apa()
dev.off()
plot.local.unseen + theme_apa()

## cumulative sample

# collect data
plot1.cumu.data <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child),list(age = subset.cumu.data$age)), FUN = sum)
plot1.cumu.data.temp <- aggregate(subset.cumu.data$Y, by = c(list(child = subset.cumu.data$child),list(age = subset.cumu.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.cumu.data)[3] <- "absolute_score"
colnames(plot1.cumu.data.temp)[3] <- "total_num_utterances"
plot1.cumu.data <- merge(plot1.cumu.data, plot1.cumu.data.temp, by = c("child","age"))
plot1.cumu.data$percentages <- (plot1.cumu.data$absolute_score / plot1.cumu.data$total_num_utterances)*100

# generate plot
plot.cumu.unseen <- ggplot(plot1.cumu.data, 
                            aes(x=age, y = percentages, group = child, linetype = child, colour = child)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  coord_cartesian(ylim=(c(0,100))) + 
  xlab("\nAge (years)") + 
  ylab("% Utterances containing \nnew words\n") + 
  ggtitle("cumu sampling") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# save plot 

png(paste(plot.path,
          "plotcumuunseen.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.cumu.unseen + theme_apa()
dev.off()
plot.cumu.unseen + theme_apa()

## combine local and cumulative sample in one plot

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

# generate combined plot 

plot.local.unseen.noxtitle <- plot.local.unseen +
  xlab("\n") +
  ylab("% Utterances containing\nnew words\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position="none")

plot.cumu.unseen.noxtitle <- plot.cumu.unseen +
  xlab("\n") +
  ylab("\n") +
  theme(legend.key.width = unit(2, "cm"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.8))

# save plot
png(paste(plot.path,
          "plotbothunknown.png", sep=""),
    width=1500,height=700,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.local.unseen.noxtitle,
                       plot.cumu.unseen.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()

plot.both.unseen <- arrange_related_x_axes(plot.local.unseen.noxtitle,
                                           plot.cumu.unseen.noxtitle,
                                           nrow=1, ncol = 2, as.table=TRUE,
                                           sub="Age (years)")

ggsave(paste0(plot.path, "plotbothunknown.png"), plot = plot.both.unseen)