########## Analysis of child utterances in model input ##########

# MAIN TEXT ANALYSES

# Compute range of child utterance lengths, as well as mean and median utterance length
min <- min(childutt.data$numwords)
max <- max(childutt.data$numwords)
mean <- mean(childutt.data$numwords)
median <- median(childutt.data$numwords)

# MAIN TEXT PLOTS

# 1) Plot with x-axis: age, y-axis: utterance length in words, collapsed over all children

# Collect information for plot
plot1.data <- aggregate(childutt.data$numwords, by = c(list(age=childutt.data$age)),FUN=sum)
colnames(plot1.data)[2] <- "total_uttlength"
plot1.data.temp <- aggregate(childutt.data$numwords, by = c(list(age=childutt.data$age)), FUN = function(x){NROW(x)})
colnames(plot1.data.temp)[2] <- "total_num_utterances"
plot1.data <- merge(plot1.data,plot1.data.temp, by = c("age"))
plot1.data$averagelength <- (plot1.data$total_uttlength/plot1.data$total_num_utterances)

# Generate plot
plot.childuttlength <- ggplot(plot1.data, 
                               aes(x=age, y = averagelength)) + 
  geom_line(size = 1.5) + basic.theme + theme(axis.text.x = element_text(size=22)) + 
  xlab("\nAge (years)") + 
  ylab("Average number of words \n in child utterance\n") + 
  ggtitle("Child utterance length ") + 
  basic.theme + theme(axis.text.x = element_text(size=22)) + 
  theme(plot.title = element_text(size=30, face = "bold.italic", hjust = 0.5, margin=margin(b = 30, unit = "pt")))

# Save plot
ggsave(paste0(plot.path, "plotchilduttlength.png"), plot = (plot.childuttlength+theme_apa()))

# 2) Merge plot with graph depicting percentgae of utterances containing repetitions x age
# (both local and cumulative as seperate lines in graph)

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

plot.childuttlength.noxtitle <- plot.childuttlength +
  xlab("\n") +
  ylab("Average number of words\nin child utterance\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18))

plot.both.repetitions_perc_collapsed.noxtitle <- plot.both.repetitions_perc_collapsed +
  xlab("\n") +
  ylab("\n\nPercentage of utterances\ncontaining repetitions\n") +
  theme(axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        legend.position=c(0.75,0.9))


png(paste(plot.path,
          "plotbothfactors.png", sep=""),
    width=1500,height=500,units="px",
    bg = "transparent")
grid.newpage()
arrange_related_x_axes(plot.childuttlength.noxtitle,
                       plot.both.repetitions_perc_collapsed.noxtitle,
                       nrow=1, ncol = 2, as.table=TRUE,
                       sub="Age (years)")
dev.off()