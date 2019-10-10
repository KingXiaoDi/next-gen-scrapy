library(tidyverse)

get_QB_Passes <- function(all, QB) {
  return (all %>%
            filter(name == QB))
}

make_Composite_Charts <- function(all, QB, week, save) {
  cols <- c("INTERCEPTION" = "red", "TOUCHDOWN" = '#0072E4', "COMPLETE" = "green", "INCOMPLETE" = "gray")
  xmin <- -160/6
  xmax <- 160/6
  ymin <- -10
  ymax <- 60
  hashX <- 18.5/6
  yardMarkers <- c(-10, 'LOS', seq(10,60,10))
  hashY <- seq(ymin,ymax)[which(seq(ymin,ymax)%%5!=0)]
   
  if (QB == 'all') {
    data <- all
  }
  else {
    data <- all %>%
      filter(name == QB)
  }
  plot <- ggplot(data, aes(x, y)) +
    scale_fill_manual(values= cols) + 
    theme_classic() +
    labs(title=sprintf('%s 2019 Passing Chart', QB)) +
    theme(panel.background = element_rect(fill = 'black'),
          axis.line=element_blank(), axis.text.x=element_blank(),  
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          legend.position='None') +
    annotate("text", x = -hashX, y = hashY, 
             label = "_", hjust = 0, vjust = -0.2, color='grey') + 
    annotate("text", x = hashX, y = hashY,
             label = "_", hjust = 1, vjust = -0.2, color='grey') + 
    annotate("segment", x = xmin, y = seq(ymin, ymax, by = 5), 
             xend =  xmax, yend = seq(ymin, ymax, by = 5), color='grey') + 
    annotate("segment", x = c(xmin, xmax), y = c(ymin, ymax), 
             xend = c(xmin, xmax), yend = c(ymax, ymin), colour = "grey", size = 2) +
    geom_hline(yintercept = 0, color='#0072E4', size=1.5) +
    annotate("text", x = -160/6-1, y = seq(ymin, ymax, by = 10), 
             label = yardMarkers, size = 4, color='white') + 
    annotate("text", x = 160/6+1, y = seq(ymin, ymax, by = 10), 
             label = yardMarkers, size = 4, color='white') +
    geom_point(aes(fill=pass_type), shape = 21, size=5)
  ggsave(file=sprintf('%s/%s plot.png', save, QB), plot, width=11.5, height=8)
  return (plot)
}
make_Composite_Charts(all, 'all', save)
all <- read_csv('c:/users/jmost/documents/football/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

all <- read_csv('c:/users/yoshi/documents/football/mallepalle/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

save <- 'c:/users/jmost/documents/football/next-gen-scrapy/compositeCharts/'
save <- 'c:/users/yoshi/documents/football/mallepalle/next-gen-scrapy/compositeCharts/'

lamar <- all %>%
  filter(name == 'Lamar Jackson')

for (each in all %>%
     distinct(name) %>%
     pull(name)) {
  make_Composite_Charts(all, each, save)
}

lamar %>%
  filter(y < 10,
         y> 0)