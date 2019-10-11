library(tidyverse)

get_QB_Passes <- function(all, QB) {
  return (all %>%
            filter(name == QB))
}

make_Chart <- function(data, name) {
  cols <- c("INTERCEPTION" = "red", "TOUCHDOWN" = '#0072E4', "COMPLETE" = "green", "INCOMPLETE" = "gray")
  xmin <- -160/6
  xmax <- 160/6
  ymin <- -10
  ymax <- 60
  hashX <- 18.5/6
  yardMarkers <- c(-10, 'LOS', seq(10,60,10))
  hashY <- seq(ymin,ymax)[which(seq(ymin,ymax)%%5!=0)]
  return (ggplot(data, aes(x, y)) +
            scale_fill_manual(values= cols) + 
            theme_classic() +
            labs(title=sprintf('%s 2019 Passing Chart', name)) +
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
            geom_point(aes(fill=pass_type), shape = 21, size=5))
}

make_Composite_Charts <- function(all, QB, save) {
  if (QB == 'all') {
    data <- all
  }
  else {
    data <- all %>%
      filter(name == QB)
  }
  plot <- make_Chart(data, QB)
  ggsave(file=sprintf('%s/%s plot.png', save, QB), plot, width=11.5, height=8)
  write_Tweet_Content(data)
  return (plot)
}
make_Composite_Charts(all, 'all', save)

all <- read_csv('c:/users/jmost/documents/football/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

all <- read_csv('c:/users/yoshi/documents/football/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

save <- 'c:/users/jmost/documents/football/next-gen-scrapy/compositeCharts/'
save <- 'c:/users/yoshi/documents/football/next-gen-scrapy/compositeCharts/'

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


all %>%
  filter(y>50)

all %>%
  arrange(y)


ravensD <- all %>%
  filter((team == 'miami-dolphins' & week == 1)|
           (team == 'arizona-cardinals' & week == 2)|
           (team == 'kansas-city-chiefs' & week == 3)|
           (team == 'cleveland-browns' & week == 4)|
           team == 'pittsburgh-steelers' & week == 5)

make_Chart(ravensD, "Ravens D")
ggsave(file=sprintf('%s/%s plot.png', save, 'Ravens D'), width=11.5, height=8)


ravensD %>%
  summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
            Att = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'),
            AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
            AvgAirYardsAtt = mean(y, na.rm=T))

write_Tweet_Content <- function(data) {
  for (QB in data %>%
       distinct(name) %>%
       pull(name)) {
    df <- data %>%
      filter(name == QB) %>%
      summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
                Att = n(),
                TD = sum(pass_type == 'TOUCHDOWN'),
                INT = sum(pass_type == 'INTERCEPTION'),
                AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
                AvgAirYardsAtt = mean(y, na.rm=T))
    missing <- data %>%
      filter(name == QB & (is.na(x)|is.na(y))) %>%
      summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
                Att = n(),
                TD = sum(pass_type == 'TOUCHDOWN'),
                INT = sum(pass_type == 'INTERCEPTION'))
    print (missing)
    print (sprintf("%s - %s/%s, %s TD, %s INT, Average AY per Att: %s, Average AY per Com: %s", 
                   QB, df$Com, df$Att, df$TD, df$INT, round(df$AvgAirYardsAtt,2), round(df$AvgAirYards,2)))
    print (sprintf("Missing %s/%s, %s TD, %s INT", 
                   missing$Com, missing$Att, missing$TD, missing$INT))
  }
}
write_Tweet_Content(ravensD)
