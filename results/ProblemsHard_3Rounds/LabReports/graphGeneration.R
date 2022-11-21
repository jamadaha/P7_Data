# Used for fancy plots
library(ggplot2)
library(data.table)

imgWidth <-8
imgHeight <- 8
# Reads in with header names
report <- read.csv('report.csv')

# Macro Things
  # Removes rows for sameouput
  macroSubset <- subset(report, algorithm != "sameoutput")
  avgMacroGeneratedReport <- as.data.table(report)
  avgMacroGeneratedReport[,list(macros_generated=mean(macros_generated)),c('domain', 'algorithm', 'problem')]
  avgMacroUseReport <- as.data.table(report)
  avgMacroUseReport[,list(macros_used=mean(macros_used)),c('domain', 'algorithm', 'problem')]
  
  macroGeneratedPlot <- ggplot(avgMacroGeneratedReport, aes(domain, macros_generated)) + geom_boxplot()
  ggsave(plot=macroGeneratedPlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)
  
  macroUsePlot <- ggplot(avgMacroUseReport, aes(domain, macros_used)) + geom_boxplot()
  ggsave(plot=macroUsePlot, filename="macroGeneratedPlot.pdf", width=imgWidth, height=imgHeight)

# Evaluation Things
  avgEvaluationsReport <- as.data.table(report)
  avgEvaluationsReport[,list(evaluations=mean(evaluations)),c('domain', 'algorithm', 'problem')]
  evaluationsPlot <- ggplot(avgEvaluationsReport, aes(algorithm, evaluations)) + geom_boxplot()
  ggsave(plot=evaluationsPlot, filename="evaluationsPlot.pdf", width=imgWidth, height=imgHeight)

# Expansion Things
  avgExpansionsReport <- as.data.table(report)
  avgExpansionsReport[,list(expansions=mean(expansions)),c('domain', 'algorithm', 'problem')]
  expansionsPlot <- ggplot(avgExpansionsReport, aes(algorithm, expansions)) + geom_boxplot()
  ggsave(plot=expansionsPlot, filename="expansionsPlot.pdf", width=imgWidth, height=imgHeight)
  
# Generated Things
  avgGeneratedReport <- as.data.table(report)
  avgGeneratedReport[,list(generated=mean(generated)),c('domain', 'algorithm', 'problem')]
  generatedPlot <- ggplot(avgGeneratedReport, aes(algorithm, generated)) + geom_boxplot()
  ggsave(plot=generatedPlot, filename="generatedPlot.pdf", width=imgWidth, height=imgHeight)

# PlanLength Things
  avgPlanLengthReport <- as.data.table(report)
  avgPlanLengthReport[,list(search_time=mean(plan_length)),c('domain', 'algorithm', 'problem')]
  planLengthPlot <- ggplot(avgPlanLengthReport, aes(algorithm, plan_length)) + geom_boxplot()
  ggsave(plot=planLengthPlot, filename="planLengthPlot.pdf", width=imgWidth, height=imgHeight)
  
# PlannerTime Things
  avgPlannerTimeReport <- as.data.table(report)
  avgPlannerTimeReport[,list(planner_time=mean(planner_time)),c('domain', 'algorithm', 'problem')]
  plannerTimePlot <- ggplot(avgPlannerTimeReport, aes(algorithm, planner_time)) + geom_boxplot()
  ggsave(plot=plannerTimePlot, filename="plannerTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# SearchTime Things
  avgSearchTimeReport <- as.data.table(report)
  avgSearchTimeReport[,list(search_time=mean(search_time)),c('domain', 'algorithm', 'problem')]
  searchTimePlot <- ggplot(avgSearchTimeReport, aes(algorithm, search_time)) + geom_boxplot()
  ggsave(plot=searchTimePlot, filename="searchTimePlot.pdf", width=imgWidth, height=imgHeight)
  
# ReformulationTime Things
  avgReformulationTimeReport <- as.data.table(report)
  avgReformulationTimeReport[,list(search_time=mean(reformulation_time)),c('domain', 'algorithm', 'problem')]
  reformulationPlot <- ggplot(avgReformulationTimeReport, aes(algorithm, reformulation_time)) + geom_boxplot()
  ggsave(plot=reformulationPlot, filename="reformulationPlot.pdf", width=imgWidth, height=imgHeight)

