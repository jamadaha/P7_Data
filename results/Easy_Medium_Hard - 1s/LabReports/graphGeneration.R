# Used for fancy plots
library(ggplot2)
library(data.table)
library(plyr)
library(bigsnpr)
library(ggpubr)
library(ggpattern)

# In inches!
imgWidth <- 4
imgHeight <- 4
imgWidthBig <- 8
imgHeightBig <- 8

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

    reformulationTimeCulPlot <- ggplot(data=culDF, aes(x=value, y=count, group=algorithm)) + 
        geom_line(aes(linetype=algorithm, color=algorithm),linewidth=lineWidth) + 
        scale_color_grey() + 
        scale_x_continuous(trans='log10') +
        labs(linetype="Algorithm", color="Algorithm") +
        ggtitle("Average Reformulation Time (Cumulative)") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("Time (Seconds)") + 
        ylab("Problems Solved")
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

    searchTimeCulPlot <- ggplot(data=culDF, aes(x=value, y=count)) + 
        geom_line(aes(linetype=algorithm, color=algorithm),linewidth=lineWidth) + 
        scale_color_grey() + 
        labs(linetype="Algorithm", color="Algorithm") +
        scale_x_continuous(trans='log10') +
        ggtitle("Average Search Time (Cumulative)") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("Time (Seconds)") + 
        ylab("Problems Solved");
    ggsave(plot=searchTimeCulPlot, filename="searchTimeCulm.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=searchTimeCulPlot, filename="searchTimeCulm_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Macro Quality graphs
    macroSubset <- subset(report, algorithm != "FD")
    macroQualityReport <- as.data.table(macroSubset)[,list(macroQuality=(mean(unique_macros_used) / mean(macros_generated))*100),c('domain', 'algorithm')]
    macroQualityPlot <- ggplot(macroQualityReport, aes(x=domain, y=macroQuality, fill=domain)) + 
        geom_col_pattern(aes(pattern = algorithm, pattern_angle = algorithm, pattern_spacing = algorithm), fill='white', color='black', pattern_spacing =0.03, position='dodge') + 
        scale_fill_grey() +
        ggtitle("Average Macro Quality (%)") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("Domain") + 
        ylab("Macro Quality (%)") +
        labs(pattern_spacing="Algorithm", pattern_angle="Algorithm", pattern="Algorithm");
    ggsave(plot=macroQualityPlot, filename="macroQualityPlot.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=macroQualityPlot, filename="macroQualityPlot_big.pdf", width=imgWidthBig, height=imgHeightBig)
    
# Search vs. Reformulation_time
    noFD <- subset(report, algorithm != "Fast Downward")
    SearchOverReformulationReport <- as.data.table(macroSubset)[,list(reformulation_time=reformulation_time / 1000),c('search_time','algorithm')]
    SearchOverReformulationPlot <- ggplot(SearchOverReformulationReport, aes(x=reformulation_time, y=search_time, shape=algorithm, color=algorithm, linetype=algorithm)) + 
        geom_point(size=pointSize) + 
        geom_smooth(method=lm, se=FALSE, aes(linetype=algorithm)) +    
        ggtitle("Search Time vs. Reformulation Time") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_x_continuous(trans='log10') +
        scale_y_continuous(trans='log10') +
        xlab("Reformulation Time (s)") + 
        ylab("Search Time (s)") +
        labs(shape="Algorithm", color="Algorithm", linetype="Algorithm") +
        scale_color_grey();
    ggsave(plot=SearchOverReformulationPlot, filename="searchTimeOverReformulationTime.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=SearchOverReformulationPlot, filename="searchTimeOverReformulationTime_big.pdf", width=imgWidthBig, height=imgHeightBig)

# Make a combined graph
    combined <- ggarrange(searchTimeCulPlot, reformulationTimeCulPlot,
              ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

    ggsave(plot=combined, filename="combinedPlot.pdf", width=imgWidth * 2, height=imgHeight)
    
# Sum Reformulation time
  timeAvg <- as.data.table(report)[,list(time=mean(reformulation_time) / 1000),c('domain', 'algorithm', 'problem')]
  agg <- aggregate(timeAvg$time, list(timeAvg$algorithm), FUN=sum) 
  sRT<-ggplot(data=agg, aes(x=Group.1, y=x)) + 
  geom_bar(stat="identity") +
  xlab("Algorithm (s)") + 
  ylab("Sum of Reformulation Time (s)")
  ggsave(plot=sRT, filename="SumReformTime.pdf", width=imgWidth, height=imgHeight)
  ggsave(plot=sRT, filename="SumReformTime_big.pdf", width=imgWidthBig, height=imgHeightBig)
    
# Sum Search time
  timeAvg <- as.data.table(report)[,list(time=mean(search_time) / 1000),c('domain', 'algorithm', 'problem')]
  agg <- aggregate(timeAvg$time, list(timeAvg$algorithm), FUN=sum) 
  sRT<-ggplot(data=agg, aes(x=Group.1, y=x)) + 
  geom_bar(stat="identity") +
  xlab("Algorithm (s)") + 
  ylab("Sum of Search Time (s)")
  ggsave(plot=sRT, filename="SumSearchTime.pdf", width=imgWidth, height=imgHeight)
  ggsave(plot=sRT, filename="SumSearchTimeBig.pdf", width=imgWidthBig, height=imgHeightBig)

# Expansion & eval Graph
  # Get row Domain - Problem - FD Expan - Al Expan
  #         x.pddl - px.pddl - xxxxxxxx - xxxxxxxx
  # Only generate if there are two algorithms
  if (length(uniqueAlgorithm) == 2) {
    DPPairs <-  as.data.table(report)[,list(domain=domain),c('problem')]
    algo1 <- uniqueAlgorithm[1];
    algo2 <- uniqueAlgorithm[2];
    exp <- as.data.table(report)[,list(expansions=mean(expansions)),c('domain', 'algorithm', 'problem')]
    algo1Subset <- subset(exp, algorithm == algo1);
    algo2Subset <- subset(exp, algorithm == algo2);
    tab <- merge(algo1Subset, algo2Subset, by=c('domain', 'problem'))
    minVal <- min(min(algo1Subset$expansions), min(algo2Subset$expansions));
    maxVal <- max(max(algo1Subset$expansions), max(algo2Subset$expansions));
    expPlot <- ggplot(data=tab, aes(x=expansions.x, y=expansions.y)) + 
        geom_point(size=2, shape=23) +
        xlim(minVal, maxVal) +
        ylim(minVal, maxVal) +
        xlab(algo1) + 
        ylab(algo2) +
        geom_abline(intercept = 0, slope = 1);
    ggsave(plot=expPlot, filename="expPlot.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=expPlot, filename="expPlot_big.pdf", width=imgWidthBig, height=imgHeightBig)

    generated <- as.data.table(report)[,list(generated=mean(generated)),c('domain', 'algorithm', 'problem')]
    algo1Subset <- subset(generated, algorithm == algo1);
    algo2Subset <- subset(generated, algorithm == algo2);
    tab <- merge(algo1Subset, algo2Subset, by=c('domain', 'problem'))
    minVal <- min(min(algo1Subset$generated), min(algo2Subset$generated));
    maxVal <- max(max(algo1Subset$generated), max(algo2Subset$generated));
    genPlot <- ggplot(data=tab, aes(x=generated.x, y=generated.y)) + 
        geom_point(size=2, shape=23) +
        xlim(minVal, maxVal) +
        ylim(minVal, maxVal) +
        xlab(algo1) + 
        ylab(algo2) +
        geom_abline(intercept = 0, slope = 1);
    ggsave(plot=genPlot, filename="genPlot.pdf", width=imgWidth, height=imgHeight)
    ggsave(plot=genPlot, filename="genPlot_big.pdf", width=imgWidthBig, height=imgHeightBig)
  }
  
# Walker speeds Graphs
  walkerSpeedSet <- subset(report, algorithm != "FD")
  
  walkerPerformance <- as.data.table(walkerSpeedSet)[,list(generated=mean(total_walker_steps)),c('algorithm')]
  walkerPerformancePlot <- ggplot(walkerPerformance, aes(x=algorithm, y=generated, fill=domain)) + 
    geom_col_pattern(aes(pattern = algorithm, pattern_angle = algorithm, pattern_spacing = algorithm), fill='white', color='black', pattern_spacing =0.03, position='dodge') + 
    scale_fill_grey() +
    ggtitle("Walker Performance") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Walker") + 
    ylab("Average steps the walker made") +
    labs(pattern_spacing="Algorithm", pattern_angle="Algorithm", pattern="Algorithm");
  print (walkerPerformancePlot);
  
  ggsave(plot=walkerPerformancePlot, filename="walkerPerformance.pdf", width=imgWidth, height=imgHeight)
  ggsave(plot=walkerPerformancePlot, filename="walkerPerformance_big.pdf", width=imgWidthBig, height=imgHeightBig)
  
  