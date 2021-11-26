library(Amelia)
library(data.table)
library(ggplot2)

aval_melt <- readRDS(file = "data/aval_melt.rds")

####### missing data #########

vars <- unique(dta_melt$variable)

for (i in 1:length(vars)){
  png(file = paste0("D:/MARKETA/git_marketa/Avalanche/missing", vars[i], ".png"), width = 900, height = 900, units = "px")
  missmap(dta_melt[variable == vars[i]])
  
  dev.off()
}

########### histograms #############

colnames(dta_melt)[3] <- "var"

dta_melt2 <- melt(dta_melt, id.vars = c("DATE2", "CAW", "var"))



vars <- unique(dta_melt2$var)



for(i in 1:length(vars)){
  
  ggplot(dta_melt2[var == vars[i]], na.rm = TRUE)+
    geom_histogram(aes( x= value), bins = 30) +
    facet_wrap(var ~ variable, scales = "free_y")
  
  ggsave(filename = paste0("D:/MARKETA/git_marketa/Avalanche/plot_", vars[i], ".pdf"), width = 200, height = 100, units = "mm")
  
}

###### boxplots (outliers) ###########

for(i in 1:length(vars)){
  
  ggplot(dta_melt2[var == vars[i]], na.rm = TRUE)+
    geom_boxplot(aes(y= value)) +
    facet_wrap(var ~ variable, scales = "free_y")
  
  ggsave(filename = paste0("D:/MARKETA/git_marketa/Avalanche/plot_box", vars[i], ".pdf"), width = 100, height = 200, units = "mm")
  
}
