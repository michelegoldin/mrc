#devtools::install_github("jvcasillas/untidydata")
#devtools::install_github("yihui/xaringan")
library(untidydata)
library(xaringan)
library(plot3D)
library(tidyverse)

#2. Load language diversity dataset
#3 explore variables, tody (long to wide)

str(language_diversity)
head(language_diversity)
unique(language_diversity$Measurement)

ld <- language_diversity%>%
  filter(., Continent == 'Africa')%>%
  spread(., Measurement, Value)%>%
  select(., country = Country, pop = Population, area = Area, lang = Langs)%>%
  mutate(., logArea = log(area), logPop = log(pop))
  
#4. Check normality, trasnform, plot

hist(log(ld$area))
hist(log(ld$pop))     

ld %>%
  ggplot(., aes(x=logPop, y =lang, color = logArea))+
  geom_point()

#5 Fit model (MRC, 3 parameters)
my_mod <- lm(lang ~ logPop + logArea, data = ld)
summary(my_mod)

my_int <- lm(lang ~ logPop + logArea + logPop:logArea, data = ld)
summary(my_int)

#6. Convert to an html presentation
