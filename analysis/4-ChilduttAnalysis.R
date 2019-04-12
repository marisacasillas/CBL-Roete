########## Analysis of child utterances in model input ##########

# MAIN TEXT ANALYSES

# Compute range of child utterance lengths, as well as mean and median utterance length
min <- min(childutt.data$numwords)
max <- max(childutt.data$numwords)
mean <- mean(childutt.data$numwords)
median <- median(child.uttdata$numwords)

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
png(paste(plot.path,
          "plotchilduttlength.png", sep=""),
    width=900,height=500,units="px",
    bg = "transparent")
plot.childuttlength+theme_apa()
dev.off()
plot.childuttlength+theme_apa()

ggsave(paste0(plot.path, "plotchilduttlength.png"), plot = (plot.childuttlength+theme_apa()))
