# Used for fancy plots
library(ggplot2)
library(data.table)
library(plyr)
library(bigsnpr)
library(ggpubr)
library(ggpattern)
library(dplyr)
library(reshape2)
library(gridExtra)

# In inches!
imgWidth <- 8
imgHeight <- 8
imgWidthBig <- 16
imgHeightBig <- 16

lineWidth <- 2
pointSize <- 2

# Reads in with header names
report <- read.csv('report.csv')
# Rename Algorithms
report[report=="sameoutput"] <- "FD"
report[report=="greedyWalker"] <- "GW"
report[report=="greedyResumeWalker"] <- "GRW"
report[report=="queueWalker"] <- "BFW"
report[report=="stepBackWalker"] <- "GFW"
report[report=="probeWalker"] <- "PW"
report[report=="regressor"] <- "BW"
report[report=="partialRegressor"] <- "PBW"
report[report=="hillClimberWalker"] <- "HCW"

# Culmin Graph
    minValue = min(report$reformulation_time) / 1000;
    maxValue = max(report$reformulation_time) / 1000;
    xSeq = seq_log(minValue, maxValue, 1000)

    uniqueAlgorithm = unique(report$algorithm);

    timeAvg <- as.data.table(report)[,list(time=mean(reformulation_time) / 1000),c('domain', 'algorithm', 'problem')]

    maxRowCount <- 0;
    vecs <- list()
    for (i in seq_along(xSeq)) {
      for (t in seq_along(uniqueAlgorithm)) {
        vec <- list()
        vec[[length(vec)+1]] = as.numeric(xSeq[i]);
        rowCount = nrow(subset(timeAvg, timeAvg$algorithm == uniqueAlgorithm[t] & timeAvg$time < xSeq[i]));
        vec[[length(vec)+1]] = uniqueAlgorithm[t];
        vec[[length(vec)+1]] = as.numeric(rowCount);
        vecs[[length(vecs)+1]] = vec;
        if (rowCount > maxRowCount) {
          maxRowCount <- rowCount
        }
      }
    }

    culDF <- as.data.frame(matrix(unlist(vecs), nrow=length(unlist(vecs[1]))))
    culDF <- as.data.frame(t(culDF))
    colnames(culDF) <- c('value', 'algorithm', 'count')

    culDF$value <- as.numeric(as.character(culDF$value))
    culDF$count <- as.numeric(as.character(culDF$count))

    reformulationTimeCulPlot <- ggplot() + 
      geom_line(data=subset(culDF, algorithm != "FD"), aes(x=value, y=count, color=algorithm, linetype=algorithm),linewidth=lineWidth) + 
      geom_line(data=subset(culDF, algorithm == "FD"), aes(x=value, y=count, color=algorithm, linetype=algorithm), linewidth=lineWidth) + 
      scale_color_grey() + 
      scale_x_continuous(trans='log10') +
      labs(linetype="Algorithm", color="Algorithm") +
      ggtitle("Average Reformulation Time (Cumulative)") + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(4,"line")
      ) +
      xlab("Time (Seconds)") + 
      ylab("Problems Solved") + 
      scale_linetype_manual(values=c(2,1,3,4,2,3,4))
    ggsave(plot=reformulationTimeCulPlot, filename="reformulationTimeCulm.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=reformulationTimeCulPlot, filename="reformulationTimeCulm_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Culmin Graph the 2nd
    minValue = min(report$search_time);
    maxValue = max(report$search_time);
    xSeq = seq_log(minValue, maxValue, 1000)

    uniqueAlgorithm = unique(report$algorithm);

    timeAvg <- as.data.table(report)[,list(time=mean(search_time)),c('domain', 'algorithm', 'problem')]

    maxRowCount <- 0;
    vecs <- list()
    for (i in seq_along(xSeq)) {
      for (t in seq_along(uniqueAlgorithm)) {
        vec <- list()
        vec[[length(vec)+1]] = as.numeric(xSeq[i]);
        rowCount = nrow(subset(timeAvg, timeAvg$algorithm == uniqueAlgorithm[t] & timeAvg$time < xSeq[i]));
        vec[[length(vec)+1]] = uniqueAlgorithm[t];
        vec[[length(vec)+1]] = as.numeric(rowCount);
        vecs[[length(vecs)+1]] = vec;
        if (rowCount > maxRowCount) {
          maxRowCount <- rowCount
        }
      }
    }

    culDF <- as.data.frame(matrix(unlist(vecs), nrow=length(unlist(vecs[1]))))
    culDF <- as.data.frame(t(culDF))
    colnames(culDF) <- c('value', 'algorithm', 'count')

    culDF$value <- as.numeric(as.character(culDF$value))
    culDF$count <- as.numeric(as.character(culDF$count))

    searchTimeCulPlot <- ggplot() + 
      geom_line(data=subset(culDF, algorithm != "FD"), aes(x=value, y=count, color=algorithm, linetype=algorithm),linewidth=lineWidth) + 
      geom_line(data=subset(culDF, algorithm == "FD"), aes(x=value, y=count, color=algorithm, linetype=algorithm), linewidth=lineWidth) + 
      scale_color_grey() + 
      labs(linetype="Algorithm", color="Algorithm") +
      scale_x_continuous(trans='log10') +
      ggtitle("Average Search Time (Cumulative)") + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(4,"line")
      ) +
      xlab("Time (Seconds)") + 
      ylab("Problems Solved") + 
      scale_linetype_manual(values=c(2,1,3,4,2,3,4))
    print(searchTimeCulPlot)
    ggsave(plot=searchTimeCulPlot, filename="searchTimeCulm.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=searchTimeCulPlot, filename="searchTimeCulm_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Macro Quality graphs
    macroSubset <- subset(report, algorithm != "FD")
    macroSubset$domain <- sub("_easy", "", macroSubset$domain)
    macroSubset$domain <- sub("_medium", "", macroSubset$domain)
    macroSubset$domain <- sub("_hard", "", macroSubset$domain)
    macroSubset$domain <- sub("_insane", "", macroSubset$domain)
    macroQualityReport <- as.data.table(macroSubset)[,list(macroQuality=(mean(unique_macros_used) / mean(macros_generated))*100),c('domain', 'algorithm')]
    macroQualityPlot <- ggplot(macroQualityReport, aes(x=domain, y=macroQuality)) + 
      geom_col_pattern(aes(pattern = algorithm, pattern_angle=algorithm), fill='white', color='black', pattern_spacing =0.03, position='dodge') + 
      ggtitle("Average Macro Quality (%)") + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(3,"line")
      ) +
      scale_pattern_manual(values=c('stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch')) +
      scale_pattern_angle_manual(values=c(0, 20, 40, 60, 80, 100, 120, 140,160)) +
      xlab("Domain") + 
      ylab("Macro Quality (%)") +
      scale_fill_grey();
    ggsave(plot=macroQualityPlot, filename="macroQualityPlot.pdf", width=imgWidth * 2, height=imgHeight / 2)
    ggsave(plot=macroQualityPlot, filename="macroQualityPlot_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Macro Generated graphs
    macroSubset <- subset(report, algorithm != "FD")
    macroSubset$domain <- sub("_easy", "", macroSubset$domain)
    macroSubset$domain <- sub("_medium", "", macroSubset$domain)
    macroSubset$domain <- sub("_hard", "", macroSubset$domain)
    macroSubset$domain <- sub("_insane", "", macroSubset$domain)
    macroGeneratedReport <- as.data.table(macroSubset)[,list(generated=mean(macros_generated)),c('domain', 'algorithm')]
    macroGeneratedPlot <- ggplot(macroGeneratedReport, aes(x=domain, y=generated)) + 
      geom_col_pattern(aes(pattern = algorithm, pattern_angle=algorithm), fill='white', color='black', pattern_spacing =0.03, position='dodge') + 
      ggtitle("Average Macros Generated") + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.key.size = unit(3,"line")
      ) +
      scale_pattern_manual(values=c('stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch')) +
      scale_pattern_angle_manual(values=c(0, 20, 40, 60, 80, 100, 120, 140,160)) +
      xlab("Domain") + 
      ylab("Macros") +
      scale_fill_grey();
    ggsave(plot=macroGeneratedPlot, filename="macrosGeneratedPlot.pdf", width=imgWidth * 2, height=imgHeight / 2)
    ggsave(plot=macroGeneratedPlot, filename="macrosGeneratedPlot_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Make a combined graph
    #combined <- ggarrange(searchTimeCulPlot, reformulationTimeCulPlot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

    #ggsave(plot=combined, filename="combinedPlot.pdf", width=imgWidth * 2, height=imgHeight)

# Sum Reformulation time
    timeAvg <- as.data.table(report)[,list(time=mean(reformulation_time) / 1000),c('domain', 'algorithm', 'problem')]
    agg <- aggregate(timeAvg$time, list(timeAvg$algorithm), FUN=sum) 
    sRT<-ggplot(data=agg, aes(x=Group.1, y=x)) + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5)
      ) +
      geom_bar(stat="identity") +
      xlab("Algorithm (s)") + 
      ylab("Sum of Reformulation Time (s)")
    ggsave(plot=sRT, filename="SumReformTime.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=sRT, filename="SumReformTime_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Sum Search time
    #timeAvg <- as.data.table(report)[,list(time=mean(search_time) / 1000),c('domain', 'algorithm', 'problem')]
    #agg <- aggregate(timeAvg$time, list(timeAvg$algorithm), FUN=sum) 
    #sRT<-ggplot(data=agg, aes(x=Group.1, y=x)) + 
    #  theme(
    #    text=element_text(size=20),
    #    plot.title = element_text(hjust = 0.5)
    #  ) +
    #  geom_bar(stat="identity") +
    #  xlab("Algorithm (s)") + 
    #  ylab("Sum of Search Time (s)")
    #ggsave(plot=sRT, filename="SumSearchTime.pdf", width=imgWidth, height=imgHeight)
    #ggsave(plot=sRT, filename="SumSearchTimeBig.pdf", width=imgWidthBig, height=imgHeightBig)

# Overall speed 
    report_hard <- report[report$domain %like% "_hard", ]
    report_medium <- report[report$domain %like% "_medium", ]
    report_easy <- report[report$domain %like% "_easy", ]
    if (nrow(report_hard) > 0 && nrow(report_medium) > 0 && nrow(report_easy) > 0 ){
        averageSearchTimeData <- as.data.table(report_hard)[,list(Hard=mean(search_time)),c('algorithm')]  
        averageSearchTimeData <- merge(as.data.table(report_medium)[,list(Medium=mean(search_time)),c('algorithm')], averageSearchTimeData)
        averageSearchTimeData <- merge(as.data.table(report_easy)[,list(Easy=mean(search_time)),c('algorithm')], averageSearchTimeData)
        averageSearchTimeData <- as.data.table(averageSearchTimeData)[,list(Algorithm=algorithm, Hard=round(Hard, digits=2),Medium=round(Medium, digits=2),Easy=round(Easy, digits=2))]  
        pdf("overall_speed_table.pdf", height=2.5, width=3.2)
        grid.table(averageSearchTimeData)
        dev.off()
    }

# Walker speeds Graphs
    walkerSpeedSet <- subset(report, algorithm != "FD")

    walkerPerformance <- as.data.table(walkerSpeedSet)[,list(generated=mean(total_walker_steps)),c('algorithm')]
    walkerPerformancePlot <- ggplot(walkerPerformance, aes(x=algorithm, y=generated)) + 
      geom_col_pattern(aes(pattern = algorithm, pattern_angle = algorithm), fill='white', color='black', pattern_spacing =0.03, position='dodge') + 
      ggtitle("Walker Performance") + 
      theme(
        text=element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.size = unit(2,"line")
      ) + 
      scale_pattern_manual(values=c('stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch', 'stripe', 'circle', 'crosshatch')) +
      scale_pattern_angle_manual(values=c(0, 20, 40, 60, 80, 100, 120, 140,160)) +
      ylab("Average steps") +
      scale_fill_grey();

    ggsave(plot=walkerPerformancePlot, filename="walkerPerformance.pdf", width=imgWidth, height=imgHeight / 2)
    ggsave(plot=walkerPerformancePlot, filename="walkerPerformance_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Evaluations Graphs
    easyProblems = report
    easyProblems = easyProblems[!grepl('_easy', easyProblems$domain),]
    easyProblems = easyProblems[!grepl('_medium', easyProblems$domain),]
    easyProblems = easyProblems[!grepl('_hard', easyProblems$domain),]
    easyProblems = easyProblems[!grepl('_insane', easyProblems$domain),]
    easyProblems <- rbind(report[report$domain %like% "_easy", ], easyProblems)
    if (nrow(easyProblems) > 0)
      easyProblems$domain <- paste0("1. Easy")

    mediumProblems <- report[report$domain %like% "_medium", ]
    if (nrow(mediumProblems) > 0)
      mediumProblems$domain <- paste0("2. Medium")

    hardProblems <- report[report$domain %like% "_hard", ]
    if (nrow(hardProblems) > 0)
      hardProblems$domain <- paste0("3. Hard")

    allProblems = rbind(easyProblems,mediumProblems,hardProblems)

    algo1 = "FD";

    set1 <- subset(allProblems, algorithm == algo1)
    set1 <- as.data.table(set1)[,list(yvalue=mean(evaluations)),c('problem','domain')]

    uniqueSet <- unique(subset(allProblems, algorithm != algo1)$algorithm)
    plots <- vector('list', 0)
    for (i in uniqueSet) {
      set2 <- subset(allProblems, algorithm == i)
      set2 <- as.data.table(set2)[,list(xvalue=mean(evaluations)),c('problem','domain')]
  
      set <- merge(set1, set2, fill=TRUE)
      setInteresting <- subset(set, mean(xvalue) < mean(yvalue))
      set <- anti_join(set, setInteresting)
  
      plots[[i]] <- local({
        i <- i
        plot <- 
          ggplot() + 
          geom_point(data=set, aes(x=xvalue, y=yvalue),color='gray') +
          geom_point(data=setInteresting, aes(x=xvalue, y=yvalue, shape=domain, color=domain), size=3) +
          xlab(i) + 
          ylab(algo1) +
          theme(
            text=element_text(size=20),
            plot.title = element_blank(),
            legend.title = element_blank(),
            legend.key.size = unit(3,"line")
          ) + 
          ggtitle("Evaluations") + 
          scale_x_log10(limits=c(min(set2$xvalue,set1$yvalue),max(set2$xvalue,set1$yvalue))) +
          scale_y_log10(limits=c(min(set2$xvalue,set1$yvalue),max(set2$xvalue,set1$yvalue))) +
          labs(domain="Domain") +
          scale_fill_grey() +
          geom_abline(intercept = 0, slope = 1);
        ggsave(plot=plot, filename=paste("evalPlot_",i,".pdf", sep=""), width=imgWidth, height=imgHeight)
        ggsave(plot=plot, filename=paste("evalPlot_",i,"_big.pdf", sep=""), width=imgWidth, height=imgHeight) 
        plot <- plot
      }) 
    }

    combined <- ggarrange(plotlist=plots,
                          ncol = 3, nrow = 2, common.legend = TRUE, legend = "right")
    ggsave(plot=combined, filename="evalPlot_big.pdf", width=imgWidth * 2, height=imgHeight)  

# Walker valid vs. invlaid paths
    #walkerPathsSet <- subset(report, algorithm != "FD")
    #walkerInvalidPaths <- as.data.table(walkerPathsSet)[,list(Valid=mean(total_walker_paths),Invalid=mean(total_walker_invalid_paths)),c('algorithm')]
    #walkerInvalidPaths <- melt(walkerInvalidPaths, id.vars='algorithm')
    #walkerInvalidPathsPlot <- ggplot(walkerInvalidPaths, aes(x=algorithm, y=value, fill=variable)) + 
    #  geom_col_pattern(color='black', pattern_spacing =0.03,position = "dodge") + 
    #  scale_fill_grey() +
    #  ggtitle("Valid vs. Invalid paths") + 
    #  theme(plot.title = element_text(hjust = 0.5)) + 
    #  xlab("Walker") + 
    #  ylab("Number of paths") +
    #  labs(fill='Paths');
    #print (walkerInvalidPathsPlot);
    #ggsave(plot=walkerInvalidPathsPlot, filename="validvsinvalidpaths.pdf", width=imgWidth, height=imgHeight)
    #ggsave(plot=walkerInvalidPathsPlot, filename="validvsinvalidpaths_big.pdf", width=imgWidthBig, height=imgHeightBig)
