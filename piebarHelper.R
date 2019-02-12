pieBar <- function(Dataset, Stacked = FALSE) {
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(cowplot)
  library(grid)
  
  Dataset[, 2] <- as.numeric(as.character(Dataset[, 2]))
  PieChart <-
    dplyr::filter(Dataset, (Dataset[, 2] == max(Dataset[, 2])))
  
  PieChart[,1] <- as.character(PieChart[,1])
  PieChart <-
    rbind(PieChart, c("Other", (100 - max(
      Dataset[,2]
    ))))
  
  g1 <-
    ggplot(data = PieChart, aes(x = "", y = PieChart[, 2], fill = PieChart[, 1])) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    )
  
  
  
  barChart <-
    dplyr::filter(Dataset, Dataset[, 2] != max(Dataset[, 2]))
  
  if (Stacked) {
    g2 <-
      ggplot (data = barChart, aes (x = "", y = barChart[, 2], fill = barChart[, 1])) +
      geom_bar(stat = "identity") + ggtitle ("Others") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 5),
        axis.ticks = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
      ) + coord_cartesian(ylim = c(0, 15))
    
  } else {g2 <-
    ggplot (data = barChart, aes (x = barChart[, 1], y = barChart[, 2], fill = barChart[, 1])) +
    geom_bar(stat = "identity",
             position = "dodge") + ggtitle ("Others") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8),
      legend.position = "none",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 25, hjust = 1, size = 5),
      axis.ticks = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) + coord_cartesian(ylim = c(0, 15))
  }
  p <-
    plot_grid(
      plot_grid(NULL, g2, rel_widths = c(.4, .6)),
      plot_grid(g1, NULL, rel_widths = c(.8, .2)),
      rel_heights = c(.3, .7), 
      ncol= 1
    )
  
  return(p)
  
}