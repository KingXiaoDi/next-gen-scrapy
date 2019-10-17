library(tidyverse)

get_QB_Passes <- function(all, QB) {
  return (all %>%
            filter(name == QB))
}

make_Chart <- function(data, name, LoSname) {
  cols <- c("INTERCEPTION" = "red", "TOUCHDOWN" = '#0072E4', "COMPLETE" = "green", "INCOMPLETE" = "gray")
  xmin <- -160/6
  xmax <- 160/6
  ymin <- -10
  ymax <- 60
  hashX <- 18.5/6
  yardMarkers <- c(-10, LoSname, seq(10,60,10))
  dotSize = max(1, 5-as.integer(nrow(data)/1200))
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
            geom_point(aes(fill=pass_type), shape = 21, size=dotSize))
}

make_Composite_Charts <- function(all, QB, save, fileName) {
  if (QB == 'all') {
    data <- all
  }
  else {
    data <- all %>%
      filter(name == QB)
    fileName = QB
  }
  plot <- make_Chart(data, fileName, '0')
  ggsave(file=sprintf('%s/%s plot.png', save, fileName), plot, width=11.5, height=8)
  print (sprintf('%s/%s plot.png', save, fileName))
  write_Tweet_Content(data)
  return (plot)
}

write_Tweet_Content <- function(data) {
  statDF <- data %>%
    summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
              Att = n(),
              TD = sum(pass_type == 'TOUCHDOWN'),
              INT = sum(pass_type == 'INTERCEPTION'),
              AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
              AvgAirYardsAtt = mean(y, na.rm=T))
  print (sprintf("%s/%s, %s TD, %s INT, Avg AY per Att: %s, Avg AY per Com: %s", 
                 statDF$Com, statDF$Att, statDF$TD, statDF$INT, round(statDF$AvgAirYardsAtt,2), round(statDF$AvgAirYards,2)))
  for (QB in data %>%
       distinct(name) %>%
       pull(name)) {
    tempDF <- data %>% 
      filter(name == QB)
    df <- tempDF %>%
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
    print (sprintf("%s - %s/%s, %s TD, %s INT, Avg AY per Att: %s, Avg AY per Com: %s", 
                   QB, df$Com, df$Att, df$TD, df$INT, round(df$AvgAirYardsAtt,2), round(df$AvgAirYards,2)))
    print (sprintf("Missing %s/%s, %s TD, %s INT", 
                   missing$Com, missing$Att, missing$TD, missing$INT))
    print (sprintf("https://nextgenstats.nfl.com/charts/list/pass/%s/2019/%s", 
                   tempDF %>% distinct(team) %>% pull(team), 
                   tempDF %>% distinct(week) %>% pull(week)))
  }
}

all <- read_csv('../next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

lamar <- read_csv('../next-gen-scrapy/lamarWithQT.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

save <- '../next-gen-scrapy/compositeCharts'

lamar <- all %>%
  filter(name == 'Lamar Jackson')

make_Composite_Charts(lamar, 'Lamar Jackson', save, 'Lamar Jackson')
for (each in all %>%
     distinct(name) %>%
     pull(name)) {
  make_Composite_Charts(all, each, save, each)
}

ravensD <- all %>%
  filter((team == 'miami-dolphins' & week == 1)|
           (team == 'arizona-cardinals' & week == 2)|
           (team == 'kansas-city-chiefs' & week == 3)|
           (team == 'cleveland-browns' & week == 4)|
           team == 'pittsburgh-steelers' & week == 5|
           team == 'cincinnati-bengals' & week == 6)

make_Chart(ravensD, "Ravens D", '0')
ggsave(file=sprintf('%s/%s plot.png', save, 'Ravens D'), width=11.5, height=8)

opp <- list('1' = 'cincinnati-bengals',
            '2' = 'pittsburgh-steelers',
            '3' = 'new-orleans-saints',
            '4' = 'arizona-cardinals',
            '5' = 'los-angeles-rams',
            '6' = 'cleveland-browns')

oppD <- all %>%
  filter(week == names(opp)[1] & team == opp[[1]]|
           week == names(opp)[2] & team == opp[[2]]|
           week == names(opp)[3] & team == opp[[3]]|
           week == names(opp)[4] & team == opp[[4]]|
           week == names(opp)[5] & team == opp[[5]]|
           week == names(opp)[6] & team == opp[[6]])

oppD %>%
  summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
            Att = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'),
            AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
            AvgAirYardsAtt = mean(y, na.rm=T))

write_Tweet_Content(oppD)

make_Composite_Charts(all, 'Andrew Dalton', save)


oppD %>%
  filter(pass_type == 'INTERCEPTION')
make_Chart(all, "all", '0')
make_Chart(all %>%
             filter(week == 6), "week 6", '0')

make_Chart(oppD, "seahawksD", 'LoS')
make_Composite_Charts(oppD, 'all', save, 'seahawksD')
make_Composite_Charts(all, 'all', save, 'all')
bengalsD
ravensD %>%
  filter(pass_type == 'TOUCHDOWN')

all %>%
  mutate(C = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~0))

all %>%
  do(fit = lm(C ~ x + y, data = .))

lm(formula= C ~ x + y, data=all)

ggplot(all, aes(x,y)) +
  geom_point(aes(fill=C), shape = 21)


all %>%
  filter(week == 5,
         pass_type %in% c('COMPLETE', 'TOUCHDOWN')) %>%
  arrange(-y)


ngWithQT <- inner_join(df, lamar, by=c('q', 'time', 'detail')) %>%
  select(c(q, time, down, togo, los, passer, target, pass_type, x, y,
           air_yards, yac, gain, success, detail)) %>%
  mutate(toSticks = y-togo)

make_Chart(ngWithQT %>%
             select(-y) %>%
             rename(y = toSticks),
           'Week 5 Lamar To Sticks', '0')
ggsave(file=sprintf('%s/%s plot.png', save, 'Week 5 Lamar To Sticks'), width=11.5, height=8)

ngWithQT %>%
  arrange(toSticks)


all %>%
  filter(week < 3,
         y >= 20) %>%
  group_by(name) %>%
  tally %>%
  arrange(-n)


#lamar %>%
all %>%
  mutate(zone = case_when(y<0~1,
                          y>=0 & y<10~2,
                          y>=10 & y<20~3,
                          y>=20~4),
         complete = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~0),
         INT = case_when(pass_type %in% c('INTERCEPTION')~1,
                              TRUE ~0),
         TD = case_when(pass_type %in% c('TOUCHDOWN')~1,
                              TRUE ~0)) %>%
  group_by(name, zone) %>%
  summarize(Com = sum(complete),
            Att = n(),
            TD = sum(TD),
            INT = sum(INT)) %>%
  arrange(zone, -Att) %>% View()
