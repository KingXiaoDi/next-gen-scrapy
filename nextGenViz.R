library(tidyverse)
library(gam)

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

zone_Data <- function(data) {
  zoned <- data %>%
    filter(!is.na(x),
           !is.na(y)) %>%
    mutate(complete = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                                TRUE~0),
           td = case_when(pass_type %in% c('TOUCHDOWN')~1,
                                TRUE~0),
           int = case_when(pass_type %in% c('INTERCEPTION')~1,
                                TRUE~0),
           horZone = cut(x, breaks=5, labels = c('1', '2', '3', '4', '5')),
           vertZone = cut(y, breaks=seq(-10,60,5),
                          labels=F,
                              right = F),
           vertZone = vertZone - 3)
  return (zoned)
}

summarize_By_Zone <- function(zone) {
  return (zone %>%
            group_by(vertZone) %>%
            summarize(Com = sum(complete),
                      Att = n(),
                      TD = sum(td),
                      INT = sum(int),
                      AvgX = mean(x)) %>%
            mutate(PercentofThrows = Att/sum(Att),
                   PercentofCompletions = Com/sum(Com),
                   PercentofTD = TD/sum(TD),
                   PercentofInt = INT/sum(INT))) %>%
    select(c(vertZone, PercentofTD, PercentofInt, AvgX, PercentofThrows, PercentofCompletions))
}

all <- read_csv('../next-gen-scrapy/pass_locations.csv') %>%
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
           team == 'cincinnati-bengals' & week == 6|
           team == 'seattle-seahawks' & week == 7)

make_Chart(ravensD, "Ravens D", '0')
ggsave(file=sprintf('%s/%s plot.png', save, 'Ravens D'), width=11.5, height=8)

opp <- list('1' = 'pittsburgh-steelers',
            '2' = 'miami-dolphins',
            '3' = 'new-york-jets',
            '4' = 'buffalo-bills',
            '5' = 'washington-redskins',
            '6' = 'new-york-giants',
            '7' = 'new-york-jets')

oppD <- all %>%
  filter(week == names(opp)[1] & team == opp[[1]]|
           week == names(opp)[2] & team == opp[[2]]|
           week == names(opp)[3] & team == opp[[3]]|
           week == names(opp)[4] & team == opp[[4]]|
           week == names(opp)[5] & team == opp[[5]]|
           week == names(opp)[6] & team == opp[[6]]|
           week == names(opp)[7] & team == opp[[7]])

oppD %>%
  summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
            Att = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'),
            AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
            AvgAirYardsAtt = mean(y, na.rm=T))

write_Tweet_Content(oppD)

oppD %>%
  filter(pass_type == 'INTERCEPTION')

oppName = 'patriotsD'
make_Chart(oppD, oppName, 'LoS')
ggsave(file=sprintf('%s/%s plot.png', save, oppName), width=11.5, height=8)

oppD %>%
  filter(y >= 20,
         pass_type %in% c('INTERCEPTION'))

oppD %>%
  filter(y >= 20,
         pass_type %in% c('COMPLETE', 'TOUCHDOWN'))

all %>%
  filter(y >= 20) %>%
  summarize(Att = n(),
            Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            td = sum(pass_type %in% c('TOUCHDOWN')),
            int = sum(pass_type %in% c('INTERCEPTION')))

make_Composite_Charts(oppD, 'all', save, oppName)
make_Composite_Charts(all, 'all', save, 'all')
make_Chart(all, "all", '0')
make_Chart(all %>%
             filter(week == 6), "week 6", '0')

fitData <- all %>%
  mutate(C = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~0))

fit <- lm(formula= C ~ y + x*y, data=fitData)

gamFit <- gam(C ~ x*y + y, data=fitData)
summary(gamFit)

ggplot(fitData, aes(x,y)) +
  geom_point(aes(fill=as.factor(C)), shape = 21) +
  scale_fill_manual(values=c('red', 'blue'))

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


NFL <- gam(C~s(abs(x),df=6)+y, data=fitData)
summary(NFL)

NFLlm <- lm(C~abs(x)+y, data=fitData)
plot(NFLlm, se=T)

NFLlog <- glm(C~abs(x)+y,
              family=binomial,
              data=fitData)
summary(NFLlog)

probs <- predict(NFLlog, type='response')
probs[1:5]

NFLlog
pred <- predict(NFLlog, fitData, type='response')
plotFit <- fitData %>%
  mutate(expC = pred) %>%
  rename(hor=x,
         vert=y) %>%
  group_by(hor, vert) %>%
  summarize(expC = mean(expC))

prep <- as_tibble(expand.grid(x=seq(-27,27,.01),
                              y=seq(-10,30, .1)))
plot <- prep %>%
  mutate(expC = predict(NFLlog, prep, type='response'))

ggplot(plot, aes(x, y)) +
  geom_raster(aes(fill=expC)) +
  scale_fill_gradient2(mid = 'red', high='#01016b')

lamarZone <- summarize_By_Zone(zone_Data(lamar))
seahawks <- summarize_By_Zone(zone_Data(oppD)) 
league <- summarize_By_Zone(zone_Data(all))

write.csv(inner_join(lamarZone, league, by="vertZone",
                     suffix = c('', '_NFL')) %>%
            select(1,5,10,6,11,2,7,3,8,4,9),
          file = 'lamar vs league.csv',
          row.names=F)
getwd()


summarize_By_Zone(zone_Data(all %>%
  filter(name == 'Joseph Flacco')))

flacco <- all %>%
  filter(name == 'Joseph Flacco')

flaccoCom <- flacco %>%
  filter(pass_type %in% c('COMPLETE', 'TOUCHDOWN'))

flaccoCom %>%
  mutate(LTE = case_when(y<=8~1,
                         TRUE ~ 0)) %>%
  group_by(LTE) %>%
  tally
