library(tidyverse)
library(gam)
source('nflFunctions.R')

setwd('c:/users/yoshi/documents/football/ravens/')
setwd('c:/users/jmost/documents/football/ravens/')

all <- read_csv('../next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

lamarQT <- read_csv('../next-gen-scrapy/lamarWithQT.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

lamarWithAllData <- inner_join(df %>%
                                 mutate(date = as.Date(date, '%Y.%m.%d')),
                               lamarQT,
                               by=c('date', 'q', 'time', 'detail'),
                               suffix=c('', '_data'))

for (each in lamarWithAllData %>%
     distinct(target) %>%
     pull(target)) {
  print (each)
  make_Composite_Charts_For_Receiver(lamarWithAllData, each, save)
}
make_Composite_Charts_For_Receiver(lamarWithAllData,'H.Hurst', save)

save <- '../next-gen-scrapy/compositeCharts'

lamar <- all %>%
  filter(name == 'Lamar Jackson')

make_Composite_Charts_For_QB(lamar, 'Lamar Jackson', save, 'Lamar Jackson')
for (each in all %>%
     distinct(name) %>%
     pull(name)) {
  make_Composite_Charts_For_QB(all, each, save, each)
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
            '7' = 'new-york-jets',
            '8' = 'cleveland-browns')

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
  filter(y>=-100,
         y<0) %>%
  summarize(Att = n(),
            Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            td = sum(pass_type %in% c('TOUCHDOWN')),
            int = sum(pass_type %in% c('INTERCEPTION')))

oppD %>%
  filter(y >= 20,
         pass_type %in% c('INTERCEPTION'))

oppD %>%
  filter(y >= 20,
         pass_type %in% c('COMPLETE', 'TOUCHDOWN'))
oppD %>%
  filter(y >= 20) %>%
  summarize(Att = n(),
            Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            td = sum(pass_type %in% c('TOUCHDOWN')),
            int = sum(pass_type %in% c('INTERCEPTION')))
all %>%
  filter(y >= 20) %>%
  summarize(Att = n(),
            Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            td = sum(pass_type %in% c('TOUCHDOWN')),
            int = sum(pass_type %in% c('INTERCEPTION')))

make_Composite_Charts_For_QB(oppD, 'all', save, oppName)
make_Composite_Charts_For_QB(all, 'all', save, 'all')
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
