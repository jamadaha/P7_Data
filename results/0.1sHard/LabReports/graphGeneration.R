# Used for fancy plots
library(ggplot2)
library(data.table)
library(plyr)
library(bigsnpr)
library(ggpubr)
library(ggpattern)

imgWidth <-8
imgHeight <- 8
# Reads in with header names
report <- read.csv('report.csv')
# Rename Algorithms
    report[report=="sameoutput"] <- "Fast Downward"
    report[report=="greedyWalker"] <- "Greedy Walker"
    report[report=="greedyResumeWalker"] <- "Greedy Resume Walker"
    report[report=="queueWalker"] <- "Queue Walker"
    report[report=="stepBackWalker"] <- "Step Back Walker"
    report[report=="probeWalker"] <- "Probe Walker"
    report[report=="regressor"] <- "Regression Walker"
    report[report=="partialRegressor"] <- "Partial Regression Walker"
    report[report=="hillClimberWalker"] <- "Hill Climber Walker"

# Macro Things
  # Removes rows for sameouput
#  macroSubset <- subset(report, algorithm != "Fast Downward")
#  avgMacroGeneratedReport <- as.data.table(report)[,list(macros_generated=mean(macros_generated)),c('domain', 'algorithm', 'problem')]
#  avgMacroUseReport <- as.data.table(report)[,list(macros_used=mean(macros_used)),c('domain', 'algorithm', 'problem')]
  
#  macroGeneratedPlot <- ggplot(avgMacroGeneratedReport, aes(domain, macros_generated)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=macroGeneratedPlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)
  
#  macroUsePlot <- ggplot(avgMacroUseReport, aes(domain, macros_used)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=macroUsePlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)

# Evaluation Things
#  avgEvaluationsReport <- as.data.table(report)[,list(evaluations=mean(evaluations)),c('domain', 'algorithm', 'problem')]
#  evaluationsPlot <- ggplot(avgEvaluationsReport, aes(algorithm, evaluations)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=evaluationsPlot, filename="evaluationsPlot.pdf", width=imgWidth, height=imgHeight)

# Expansion Things
#  avgExpansionsReport <- as.data.table(report)[,list(expansions=mean(expansions)),c('domain', 'algorithm', 'problem')]
#  expansionsPlot <- ggplot(avgExpansionsReport, aes(algorithm, expansions)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=expansionsPlot, filename="expansionsPlot.pdf", width=imgWidth, height=imgHeight)
  
# Generated Things
#  avgGeneratedReport <- as.data.table(report)[,list(generated=mean(generated)),c('domain', 'algorithm', 'problem')]
#  generatedPlot <- ggplot(avgGeneratedReport, aes(algorithm, generated)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=generatedPlot, filename="generatedPlot.pdf", width=imgWidth, height=imgHeight)

# PlanLength Things
#  avgPlanLengthReport <- as.data.table(report)[,list(plan_length=mean(plan_length)),c('domain', 'algorithm', 'problem')]
#  planLengthPlot <- ggplot(avgPlanLengthReport, aes(algorithm, plan_length)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=generatedPlot, filename="planLengthPlot.pdf", width=imgWidth, height=imgHeight)
  
# PlannerTime Things
#  avgPlannerTimeReport <- as.data.table(report)[,list(planner_time=mean(planner_time)),c('domain', 'algorithm', 'problem')]
#  plannerTimePlot <- ggplot(avgPlannerTimeReport, aes(algorithm, planner_time)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=plannerTimePlot, filename="plannerTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# SearchTime Things
#  avgSearchTimeReport <- as.data.table(report)[,list(search_time=mean(search_time)),c('domain', 'algorithm', 'problem')]
#  searchTimePlot <- ggplot(avgSearchTimeReport, aes(algorithm, search_time)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=searchTimePlot, filename="searchTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# ReformulationTime Things
#  avgReformulationTimeReport <- as.data.table(report)[,list(reformulation_time=mean(reformulation_time)),c('domain', 'algorithm', 'problem')]
#  reformulationPlot <- ggplot(avgReformulationTimeReport, aes(algorithm, reformulation_time)) + geom_boxplot() + scale_color_grey()
#  ggsave(plot=reformulationPlot, filename="reformulationPlot.pdf", width=imgWidth, height=imgHeight)

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
        geom_line(aes(linetype=algorithm, color=algorithm)) + 
        scale_color_grey() + 
        scale_x_continuous(trans='log10') +
        labs(linetype="Algorithm", color="Algorithm") +
        ggtitle("Average Reformulation Time (Cumulative)") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("Time (Seconds)") + 
        ylab("Problems Solved")
    ggsave(plot=reformulationTimeCulPlot, filename="reformulationTimeCulm.pdf", width=imgWidth, height=imgHeight)

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
        geom_line(aes(linetype=algorithm, color=algorithm)) + 
        scale_color_grey() + 
        labs(linetype="Algorithm", color="Algorithm") +
        scale_x_continuous(trans='log10') +
        ggtitle("Average Search Time (Cumulative)") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlab("Time (Seconds)") + 
        ylab("Problems Solved");
    ggsave(plot=searchTimeCulPlot, filename="searchTimeCulm.pdf", width=imgWidth, height=imgHeight)

# Macro Quality graphs
    macroSubset <- subset(report, algorithm != "Fast Downward")
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
    
# Search vs. Reformulation_time
    noFD <- subset(report, algorithm != "Fast Downward")
    SearchOverReformulationReport <- as.data.table(macroSubset)[,list(reformulation_time=reformulation_time / 1000),c('search_time','algorithm')]
    SearchOverReformulationPlot <- ggplot(SearchOverReformulationReport, aes(x=reformulation_time, y=search_time, shape=algorithm, color=algorithm, linetype=algorithm)) + 
        geom_point() + 
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

# Make a combined graph
    combined <- ggarrange(searchTimeCulPlot, reformulationTimeCulPlot,
              ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

    ggsave(plot=combined, filename="combinedPlot.pdf", width=imgWidth * 2, height=imgHeight)
    