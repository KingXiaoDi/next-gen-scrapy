library(tidyverse)

get_QB_Passes <- function(all, QB) {
  return (all %>%
            filter(name == QB))
}

all <- read_csv('c:/users/jmost/documents/football/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

all <- read_csv('c:/users/yoshi/documents/football/mallepalle/next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

lamar <- all %>%
  filter(name == 'Lamar Jackson')

mahomes <- get_QB_Passes(all, 'Patrick Mahomes')

cols <- c("INTERCEPTION" = "red", "TOUCHDOWN" = '#0072E4', "COMPLETE" = "green", "INCOMPLETE" = "gray")

xmin <- -160/6
xmax <- 160/6
ymin <- -10
ymax <- 60
hashX <- 18.5/6

yardMarkers <- c(-10, 'LOS', seq(10,60,10))
hashY <- seq(ymin,ymax)[which(seq(ymin,ymax)%%5!=0)]

ggplot(lamar, aes(x, y)) +
  scale_color_manual(values= cols) + 
  scale_color_manual(values= c('green', 'grey', 'red', '#0072E4')) +
  theme_classic() + 
  labs(title='Lamar 2019 Passing Chart') +
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
  
  annotate("text", x = -160/6-1, y = seq(ymin, ymax, by = 10), 
           label = yardMarkers, size = 4, color='white') + 
  
  annotate("text", x = 160/6+1, y = seq(ymin, ymax, by = 10), 
           label = yardMarkers, size = 4, color='white') +
  
  geom_line(y=0, color='#0072E4', size=1.5) +
  
  geom_point(aes(color=pass_type), size=5)
  
ggplot(mahomes, aes(x, y)) +
  #scale_color_manual(values= c('green', 'grey', '#0072E4', 'red')) +
  scale_color_manual(values= cols) +
  theme_classic() + 
  labs(title='Mahomes 2019 Passing Chart') +
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
  
  annotate("text", x = -160/6-1, y = seq(ymin, ymax, by = 10), 
           label = yardMarkers, size = 4, color='white') + 
  
  annotate("text", x = 160/6+1, y = seq(ymin, ymax, by = 10), 
           label = yardMarkers, size = 4, color='white') +
  
  geom_line(y=0, color='#0072E4', size=1.5) +
  
  geom_point(aes(color=pass_type), size=5)

lamar %>%
  filter(y < 10,
         y> 0)
         
all %>%
  filter(name=='Patrick Mahomes',
         pass_type == 'TOUCHDOWN')

         
ggsave(file = 'Mahomes2019.png', width=11.5, height=8)
