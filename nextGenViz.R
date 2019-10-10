library(tidyverse)

lamar <- read_csv('c:/users/jmost/documents/football/next-gen-scrapy/lamar2019 pass_locations.csv') %>%
  mutate(Complete = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~ 0),
         Complete = as.factor(Complete))

lamar <- read_csv('c:/users/yoshi/documents/football/mallepalle/next-gen-scrapy/lamar2019 pass_locations.csv') %>%
  mutate(Complete = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~ 0),
         Complete = as.factor(Complete))

xmin <- -160/6
xmax <- 160/6
ymin <- -10
ymax <- 60
hashX <- 18.5/6

yardMarkers <- c(-10, 'LOS', seq(10,60,10))
hashY <- seq(ymin,ymax)[which(seq(ymin,ymax)%%5!=0)]

ggplot(lamar, aes(x, y)) +
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
  

lamar %>%
  filter(y < 10,
         y> 0)


passes <- read_csv('c:/users/yoshi/documents/football/ravens/games/2019/season/passesFromStats.csv')
passes %>%
  filter(team == 'Ravens',
         passer == 'L.Jackson',
         Date == '2019.10.06') %>%
  group_by(target) %>%
  tally() %>% 
  arrange(-n)
ggsave('lamar through week 5.png', width=11.5, height=8)  

passes %>%
  filter(team == 'Ravens')
         