# Used for fancy plots
library(ggplot2)
library(data.table)
library(plyr)

imgWidth <-8
imgHeight <- 8
# Reads in with header names
report <- read.csv('report.csv')
report <- na.omit(report)

# Macro Things
  # Removes rows for sameouput
  macroSubset <- subset(report, algorithm != "sameoutput")
  avgMacroGeneratedReport <- as.data.table(report)[,list(macros_generated=mean(macros_generated)),c('domain', 'algorithm', 'problem')]
  avgMacroUseReport <- as.data.table(report)[,list(macros_used=mean(macros_used)),c('domain', 'algorithm', 'problem')]
  
  macroGeneratedPlot <- ggplot(avgMacroGeneratedReport, aes(domain, macros_generated)) + geom_boxplot()
  ggsave(plot=macroGeneratedPlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)
  
  macroUsePlot <- ggplot(avgMacroUseReport, aes(domain, macros_used)) + geom_boxplot()
  ggsave(plot=macroUsePlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)

# Evaluation Things
  avgEvaluationsReport <- as.data.table(report)[,list(evaluations=mean(evaluations)),c('domain', 'algorithm', 'problem')]
  evaluationsPlot <- ggplot(avgEvaluationsReport, aes(algorithm, evaluations)) + geom_boxplot()
  ggsave(plot=evaluationsPlot, filename="evaluationsPlot.pdf", width=imgWidth, height=imgHeight)

# Expansion Things
  avgExpansionsReport <- as.data.table(report)[,list(expansions=mean(expansions)),c('domain', 'algorithm', 'problem')]
  expansionsPlot <- ggplot(avgExpansionsReport, aes(algorithm, expansions)) + geom_boxplot()
  ggsave(plot=expansionsPlot, filename="expansionsPlot.pdf", width=imgWidth, height=imgHeight)
  
# Generated Things
  avgGeneratedReport <- as.data.table(report)[,list(generated=mean(generated)),c('domain', 'algorithm', 'problem')]
  generatedPlot <- ggplot(avgGeneratedReport, aes(algorithm, generated)) + geom_boxplot()
  ggsave(plot=generatedPlot, filename="generatedPlot.pdf", width=imgWidth, height=imgHeight)

# PlanLength Things
  #avgPlanLengthReport <- as.data.table(report)[,list(search_time=mean(plan_length)),c('domain', 'algorithm', 'problem')]
  #planLengthPlot <- ggplot(avgPlanLengthReport, aes(algorithm, plan_length)) + geom_boxplot()
  #ggsave(plot=planLengthPlot, filename="planLengthPlot.pdf", width=imgWidth, height=imgHeight)
  
# PlannerTime Things
  #avgPlannerTimeReport <- as.data.table(report)[,list(planner_time=mean(planner_time)),c('domain', 'algorithm', 'problem')]
  #plannerTimePlot <- ggplot(avgPlannerTimeReport, aes(algorithm, planner_time)) + geom_boxplot()
  #ggsave(plot=plannerTimePlot, filename="plannerTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# SearchTime Things
  #avgSearchTimeReport <- as.data.table(report)[,list(search_time=mean(search_time)),c('domain', 'algorithm', 'problem')]
  #searchTimePlot <- ggplot(avgSearchTimeReport, aes(algorithm, search_time)) + geom_boxplot()
  #ggsave(plot=searchTimePlot, filename="searchTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# ReformulationTime Things
  #avgReformulationTimeReport <- as.data.table(report)[,list(search_time=mean(reformulation_time)),c('domain', 'algorithm', 'problem')]
  #reformulationPlot <- ggplot(avgReformulationTimeReport, aes(algorithm, reformulation_time)) + geom_boxplot()
  #ggsave(plot=reformulationPlot, filename="reformulationPlot.pdf", width=imgWidth, height=imgHeight)

# Culmin Graph
minValue = min(report$reformulation_time);
maxValue = max(report$reformulation_time);
xSeq = seq.int(0, maxValue, 200)

uniqueAlgorithm = levels(unique(report$algorithm));

timeAvg <- as.data.table(report)[,list(time=mean(reformulation_time)),c('domain', 'algorithm', 'problem')]

vecs <- list()
for (i in seq_along(xSeq)) {
  for (t in seq_along(uniqueAlgorithm)) {
    vec <- list()
    vec[[length(vec)+1]] = as.integer(xSeq[i]);
    rowCount = nrow(subset(timeAvg, timeAvg$algorithm == uniqueAlgorithm[t] & timeAvg$time < xSeq[i]));
    vec[[length(vec)+1]] = uniqueAlgorithm[t];
    vec[[length(vec)+1]] = rowCount;
    vecs[[length(vecs)+1]] = vec;
  }
}
culDF <- as.data.frame(matrix(unlist(vecs), nrow=length(unlist(vecs[1]))))
culDF <- as.data.frame(t(culDF))
colnames(culDF) <- c('value', 'algorithm', 'count')

algoCul <- ggplot(data=culDF, aes(x=value, y=count, group=algorithm)) + geom_line(aes(color=algorithm)) + scale_x_discrete(limits=culDF$value) + scale_y_discrete(limits=culDF$count);
ggsave(plot=algoCul, filename="algoCul.pdf", width=imgWidth, height=imgHeight)