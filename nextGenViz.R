library(gam)
source('nflFunctions.R')

setwd('c:/users/yoshi/documents/football/ravens/')
setwd('c:/users/jmost/documents/football/ravens/')

save <- '../next-gen-scrapy/compositeCharts'

all <- read_csv('../next-gen-scrapy/pass_locations.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

lamarQT <- read_csv('../next-gen-scrapy/lamarWithQT.csv') %>%
  mutate(pass_type = factor(pass_type, levels = c('COMPLETE', 'INCOMPLETE', 'INTERCEPTION', 'TOUCHDOWN')))

anti_join(df %>%
            mutate(date = as.Date(date, '%Y.%m.%d')) %>%
            filter(type == 'pass',
                   passer == 'L.Jackson'),
          lamarQT,
          by=c('date', 'q', 'time', 'detail'),
          suffix=c('', '_data')) %>%
  filter(possession == 'BLT',
         !is.na(type)) %>%
  select(date, q, time, detail) %>%
  arrange(date)

lamarWithAllData <- inner_join(df %>%
                                 mutate(date = as.Date(date, '%Y.%m.%d')),
                               lamarQT,
                               by=c('date', 'q', 'time', 'detail'),
                               suffix=c('', '_data'))


for (each in lamarWithAllData %>%
     distinct(target) %>%
     pull(target)) {
  if ((each != 'spike') & !is.na(each)) {
    print (each)
    make_Composite_Charts_For_Receiver(lamarWithAllData, each, save)
  }
}

for (each in seq(1,4)) {
  make_Composite_Charts_For_QB(lamarWithAllData %>%
                        filter(down == each), 'all', save, sprintf('Down %d', each))
}

lamarTD <- lamar %>%
  filter(pass_type == 'TOUCHDOWN')

make_Composite_Charts_For_QB(lamarTD, 'Lamar Jackson', save, 'Lamar Jackson TDs')

for (each in all %>%
     distinct(name) %>%
     pull(name)) {
  make_Composite_Charts_For_QB(all, each, save, each)
}

for (each in all %>%
     distinct(name) %>%
     pull(name)) {
  make_Composite_Charts_For_QB(all %>% 
                                 filter(pass_type == 'TOUCHDOWN'), each, save, sprintf('TDs %s', each))
}
print (sprintf('TDs %s', each))

ravensD <- all %>%
  filter((team == 'miami-dolphins' & week == 1)|
           (team == 'arizona-cardinals' & week == 2)|
           (team == 'kansas-city-chiefs' & week == 3)|
           (team == 'cleveland-browns' & week == 4)|
           team == 'pittsburgh-steelers' & week == 5|
           team == 'cincinnati-bengals' & week == 6|
           team == 'seattle-seahawks' & week == 7|
           team == 'new-england-patriots' & week == 9|
           team == 'cincinnati-bengals' & week == 10)

make_Chart(ravensD, "Ravens D", '0', write_Tweet_Content(ravensD))
ggsave(file=sprintf('%s/%s plot.png', save, 'Ravens D'), width=11.5, height=8)

opp <- list('1' = 'new-orleans-saints',
            '2' = 'jacksonville-jaguars',
            '3' = 'los-angeles-chargers',
            '4' = 'carolina-panthers',
            '5' = 'atlanta-falcons',
            '6' = 'kansas-city-chiefs',
            '7' = 'indianapolis-colts',
            '8' = 'oakland-raiders',
            '9' = 'jacksonville-jaguars')

oppD <- all %>%
  filter(week == names(opp)[1] & team == opp[[1]]|
           week == names(opp)[2] & team == opp[[2]]|
           week == names(opp)[3] & team == opp[[3]]|
           week == names(opp)[4] & team == opp[[4]]|
           week == names(opp)[5] & team == opp[[5]]|
           week == names(opp)[6] & team == opp[[6]]|
           week == names(opp)[7] & team == opp[[7]]|
           week == names(opp)[8] & team == opp[[8]]|
           week == names(opp)[9] & team == opp[[9]])

oppName = 'TexansD'
make_Chart(oppD, oppName, '0', write_Tweet_Content(oppD))
ggsave(file=sprintf('%s/%s plot.png', save, oppName), width=11.5, height=8)

oppD %>%
  summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
            Att = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'),
            AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
            AvgAirYardsAtt = mean(y, na.rm=T))

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

write.csv(lamarWithAllData %>%
  filter(target =='M.Boykin') %>%
  select(c(date, q, time, y, pass_type)) %>%
  arrange(-y),
  file = 'for Ken/temp.csv',
  row.names=F)

df %>%
  filter(passer == 'L.Jackson',
         type == 'pass',
         down == 1,
         date == '2019.09.08')# %>%
#  group_by(date) %>%
  summarize(Com = sum(complete, na.rm=T),
            Att = n(),
            gain = sum(gain, na.rm=T))

df %>%
  filter(passer == 'L.Jackson',
         down == 1,
         gain >= togo)
df %>%
  filter(gain == 21,
         type == 'pass')


all %>%
  filter(y<0) %>%
  summarize(Com = sum(pass_type == 'COMPLETE') + sum(pass_type == 'TOUCHDOWN'),
                           Att = n(),
                           TD = sum(pass_type == 'TOUCHDOWN'),
                           INT = sum(pass_type == 'INTERCEPTION'),
                           AvgAirYards = mean(y[pass_type %in% c('COMPLETE', 'TOUCHDOWN')]),
                           AvgAirYardsAtt = mean(y, na.rm=T))

intBehindLoS <- all %>%
  filter(y<0, 
         pass_type == 'INTERCEPTION') %>%
  arrange(week)

xmin <- -160/6
xmax <- 160/6
ymin <- -15
ymax <- 5
hashX <- 18.5/6
yardMarkers <- c(-15, -10, -5, 0, 5)#, seq(10,60,10))
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

all %>%
  filter(name == 'Lamar Jackson' ) %>%
  mutate(Depth = case_when(y >= 15~ 'deep',
                           y < 15 ~ 'short',
                           TRUE ~ 'NA')) %>%
  group_by(Depth) %>%
  summarize(Com = sum(pass_type %in% c('COMPLETE', 'TOUCHDOWN')),
            Att = n(),
            TD = sum(pass_type == 'TOUCHDOWN'),
            INT = sum(pass_type == 'INTERCEPTION'))

lamar %>%
  mutate(Direction = cut(x, 5)) %>%
  group_by(Direction) %>%
  tally

all %>%
  mutate(Direction = cut(x, 3)) %>%
  filter(!is.na(Direction)) %>%
  group_by(name, Direction) %>%
  summarize(Att = n()) %>%
  mutate(PercentOfThrows = Att/sum(Att)) %>%
  arrange(-PercentOfThrows) %>% View()

combined %>% filter(Skill5 == 89, down==3) %>%
  group_by(type) %>%
  tally %>%
  filter(type %in% c('pass', 'sack', 'scramble', 'run')) %>%
  mutate(Per = n/sum(n)) %>%
  arrange(-n)


lamarWithAllData 


make_Composite_Charts_For_QB(all %>%
                               filter(week > 3), 'Lamar Jackson', save, 'Lamar Since Week 4')
