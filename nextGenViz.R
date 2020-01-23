library(gam)
source('nflFunctions.R')

setwd('c:/users/yoshi/documents/football/ravens/')
setwd('c:/users/jmost/documents/football/ravens/')

setwd('games/2019')
seasonDF <- load_DF_For_Stats('season', 'Ravens') %>%
  mutate(target = recode(target, M.Ingram = 'M.Ingram II', W.Snead = 'W.Snead IV'))
setwd('../..')

save <- '../next-gen-scrapy/compositeCharts'

all <- read_csv('../next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))
lamarQT <- read_csv('../next-gen-scrapy/lamarWithQT.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

teamAbb <- read_csv('teamAbb.csv')
ravensD <- inner_join(all, inner_join(read_csv('ravens.csv'), teamAbb),
                      by = c('week', 'team'))
oppD <- inner_join(all, inner_join(read_csv('opp.csv'), teamAbb),
                   by = c('week', 'team'))

anti_join(seasonDF %>% mutate(date = as.Date(date, '%Y.%m.%d')) %>%
            filter(type == 'pass', passer == 'L.Jackson'),
          lamarQT, by=c('date', 'q', 'time', 'detail'),
          suffix=c('', '_data')) %>%
  filter(possession == 'BLT', !is.na(type)) %>%
  select(date, q, time, detail) %>%
  arrange(date)

lamarWithAllData <- inner_join(seasonDF %>% mutate(date = as.Date(date, '%Y.%m.%d')),
                               lamarQT,
                               by=c('date', 'q', 'time', 'detail'),
                               suffix=c('', '_data'))

for (receiver in lamarWithAllData %>% distinct(target) %>% pull(target)) {
  if ((receiver != 'spike') & !is.na(receiver)) {
    make_Composite_Charts_For_Receiver(lamarWithAllData, receiver, save)
  }
}

for (each in seq(1,4)) {
  make_Composite_Charts_For_QB(lamarWithAllData %>%
                        filter(down == each), 'all', save, sprintf('Lamar/Downs/Down %d', each))
}

lamarTD <- lamarWithAllData %>%
  filter(pass_type == 'TOUCHDOWN')
make_Composite_Charts_For_QB(lamarTD, 'Lamar Jackson', save, 'Lamar Jackson TDs')

for (each in all %>% distinct(name) %>% pull(name)) {
  make_Composite_Charts_For_QB(all, each, save, each)
}

for (each in all %>% distinct(name) %>% pull(name)) {
  make_Composite_Charts_For_QB(all %>% 
                                 filter(pass_type == 'TOUCHDOWN'), each, save, sprintf('TDs %s', each))
}

make_Chart(ravensD, "Ravens D", '0', write_Tweet_Content(ravensD))
ggsave(file=sprintf('%s/%s plot.png', save, 'Ravens D'), width=11.5, height=8)


oppName = 'JetsD'
make_Chart(oppD, oppName, '0', write_Tweet_Content(oppD))
ggsave(file=sprintf('%s/%s plot.png', save, oppName), width=11.5, height=8)

oppD %>% 
  filter(y>=-100,
         y<0) %>%
  summarize(Att = n(),
            Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            td = sum(pass_type %in% c('TOUCHDOWN')),
            int = sum(pass_type %in% c('INTERCEPTION')))

make_Composite_Charts_For_QB(all, 'all', save, 'all')
make_Chart(all, "all", '0')

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
make_Zone_Chart(ravensD)
make_Zone_Chart(oppD)
lamarDepth <- add_Depth_and_Direction(lamarWithAllData)

chartSave <- make_Lamar_Chart(lamarDepth, quo(direction)) %>%
  arrange(direction)
write.csv(chartSave,
          file = 'for Ken/temp.csv',
          row.names=F)
chartSave <- make_Lamar_Chart(lamarDepth, quo(depth)) %>%
  arrange(depth)
write.csv(chartSave,
          file = 'for Ken/temp.csv',
          row.names=F)
chartSave <- make_Lamar_Chart(lamarDepth, quo(zone)) %>%
  arrange(zone)
write.csv(chartSave,
          file = 'for Ken/temp.csv',
          row.names=F)

lamarWithAllData %>%
  filter(y<0,
         x< -80/9) %>%
  select(date, q, time, gain, x, y, detail)
160/9
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

intBehindLoS <- all %>%
  filter(y<0, 
         pass_type == 'INTERCEPTION') %>%
  arrange(week)

xmin <- -160/6
xmax <- 160/6
ymin <- -15
ymax <- 5
hashX <- 18.5/6
yardMarkers <- seq(10,60,10)
hashY <- seq(ymin,ymax)[which(seq(ymin,ymax)%%5!=0)]

cols <- c("Andrew Dalton" = "http://content.sportslogos.net/logos/7/154/thumbs/403.gif", 
          "Baker Mayfield" = 'http://content.sportslogos.net/logos/7/155/thumbs/2ioheczrkmc2ibc42c9r.gif',
          "Elisha Manning" = "http://content.sportslogos.net/logos/7/166/thumbs/919.gif",
          "Gardner Minshew" = 'http://content.sportslogos.net/logos/7/159/thumbs/15988562013.gif',
          "James Garoppolo" = 'http://content.sportslogos.net/logos/7/179/thumbs/17994552009.gif',
          "Jared Goff" = 'http://content.sportslogos.net/logos/7/178/thumbs/1029.gif',
          "Kyle Allen" = 'http://content.sportslogos.net/logos/7/174/thumbs/f1wggq2k8ql88fe33jzhw641u.gif',
          "Kyler Murray" = 'http://content.sportslogos.net/logos/7/177/thumbs/kwth8f1cfa2sch5xhjjfaof90.gif',
          "Lamar Jackson" = 'http://content.sportslogos.net/logos/7/153/thumbs/318.gif',
          "Luke Falk" = 'http://content.sportslogos.net/logos/7/152/thumbs/v7tehkwthrwefgounvi7znf5k.gif', 
          "Matthew Ryan" = 'http://content.sportslogos.net/logos/7/173/thumbs/299.gif')

ggplot(intBehindLoS, aes(x, y)) +
  theme_classic() +
  labs(title=sprintf('%s 2019', "INT Behind LoS")) +
  theme(panel.background = element_rect(fill = 'black'),
        axis.line=element_blank(), axis.text.x=element_blank(),  
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank()) +
  annotate("text", x = -hashX, y = hashY, 
           label = "_", hjust = 0, vjust = -0.2, color='grey') + 
  annotate("text", x = hashX, y = hashY,
           label = "_", hjust = 1, vjust = -0.2, color='grey') + 
  annotate("segment", x = xmin, y = seq(ymin, ymax, by = 5), 
           xend =  xmax, yend = seq(ymin, ymax, by = 5), color='grey') + 
  annotate("segment", x = c(xmin, xmax), y = c(ymin, ymax), 
           xend = c(xmin, xmax), yend = c(ymax, ymin), colour = "grey", size = 2) +
  geom_hline(yintercept = 0, color='#0072E4', size=1.5) +
  annotate("text", x = -160/6-1, y = seq(ymin, ymax, by = 5), 
           label = yardMarkers, size = 4, color='white') + 
  annotate("text", x = 160/6+1, y = seq(ymin, ymax, by = 5), 
           label = yardMarkers, size = 4, color='white') +
  #geom_point(aes(fill=name), shape = 21, size=10) +
  geom_image(aes(image=cols), size=.075)

ggsave(file=sprintf('%s/%s plot.png', save, 'Int Behind LoS'), width=11.5, height=8)

plotData <- lamarWithAllData %>%
  separate(los, c('side', 'yard'), remove=F) %>%
  mutate(yard = as.numeric(yard),
         yard = case_when(is.na(yard)~50,
                          side=='BLT'~yard,
                          TRUE~100-yard)) %>%
  select(c(date, q, time, los, side, yard))

ggplot(plotData, aes(yard)) +
  geom_histogram(binwidth=5)

ggplot(convert_LoS(seasonDF) %>%
         mutate(type = case_when(type %in% c('pass', 'sack', 'scramble')~'dropback',
                                 type %in% c('run', 'fumbledExchange')~'run',
                                 TRUE ~ NA_character_)) %>%
         filter(!is.na(type),
                possession == 'BLT',
                !type %in% c('kickoff', 'XP', 'FGA', 'kneel', '2PT', 'punt')), aes(yard, fill=type)) +
  geom_histogram(binwidth=5)

calculate_qb_rating_Attempts(lamarWithLoSgroup %>%
                               group_by(losGrouped) %>%
                               summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
                                         Att = n(),
                                         Yards = sum(gain),
                                         TD = sum(pass_type == 'TOUCHDOWN'),
                                         INT = sum(pass_type == 'INTERCEPTION'),
                                         AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
                                         AvgAirYardsAtt = mean(y, na.rm=T)) %>%
                               mutate(ComPer = round(100*Com/Att,1)))

plot_Lamar_By_LoS(lamarWithQT, 10)

temp <- convert_LoS(lamarWithAllData) %>%
  filter(type != 'spike') %>%
  mutate(losGrouped = cut(yard, seq(0,100, 10))) %>%
  filter(losGrouped %in% c('(70,80]', '(80,90]', '(90,100]')|yard == 70)
make_Chart(temp, 'Lamar Inside 30', 'LoS', write_Tweet_Content(temp))
ggsave(file=sprintf('%s/Lamar/Inside 30 plot.png', save), width=11.5, height=8)

make_Zone_Chart(all)

all %>%
  filter(pass_type == 'TOUCHDOWN') %>%
  mutate(Depth = as.integer(y/10)) %>% 
  group_by(name, Depth) %>%
  summarize(TDs = n()) %>% 
  pivot_wider(id_cols=name, names_from=Depth, values_from=TDs) %>%
  replace(., is.na(.), 0) %>%
  mutate(Total = `0` + `1` + `2` + `3` + `4` + `5` + `6`) %>%
  arrange(-Total) %>% View()


make_Chart_With_Target_Colors(lamarWithAllData %>% filter(down == 4), 'Lamar 2019 Down 1 Chart')

all <- all %>%
  mutate(complete = case_when(pass_type %in% c('COMPLETE', 'TOUCHDOWN')~1,
                              TRUE ~0))

lamar <- all %>%
  filter(name == 'Lamar Jackson')
mahomes <- all %>%
  filter(name == 'Patrick Mahomes')

lamar %>%
  filter(y >= 10,
         y<20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

mahomes %>%
  filter(y >= 10,
         y<20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

all %>%
  filter(y >= 10,
         y<20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

lamar %>%
  filter(y >=20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

mahomes %>%
  filter(y >=20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

all %>%
  filter(y >=20) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))


lamar %>%
  filter(x <= -15,
         y < 10) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

lamar %>%
  filter(x >= 15,
         y < 10) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

lamar %>%
  filter(abs(x) >= 15,
         y < 10) %>%
  summarize(complete = sum(complete),
            attempt = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))
