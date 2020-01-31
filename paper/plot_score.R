N <- rep(2:6, 4)
Reconstruction <- c(rep("Correct reconstruction",10), rep("Incorrect reconstruction",10))
Repeated <- c(rep("no chunck repeated",5), rep("one chunck repeated",5), rep("no chunck repeated",5), rep("one chunck repeated",5))
Score <- c(-log(1/factorial(2:6)), -log(2/factorial(2:6)), log(1-1/factorial(2:6)), NA, log(1-2/factorial(3:6)))

score_data <- data.frame(N, Reconstruction, Repeated, Score)

ggplot(data = score_data, aes(N, Score, colour=Repeated, shape=Repeated)) +
    facet_wrap(~Reconstruction, nrow=1, scales="free_y") +
    geom_line(size=.8) +
    geom_point(size=2) +
    xlab("Utterance length") +
    ylab("Reconstruction score") +
    theme(legend.title = element_blank()) +
    theme(legend.position = "top")
