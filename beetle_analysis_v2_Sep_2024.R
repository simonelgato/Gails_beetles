## Gail's beetle data
## 12 August 2024

## script to do data reformatting and (hopefully) do some modelling

setwd("C:/Users/notne/Research/GitHub/Gails_beetles")
library(ggplot2)
library(dplyr)
library(brms)
library(lme4)

################################################################################

## read in data and make factors for field (field), location of sample (sloc: in crop 
## or in margin) and sample number (snum)

## I've calculated abundance and species richness already  in the input data set
## did that in Excel

beetle <- read.csv("beetles_2_Sep.csv")
beetle$year <- factor(substr(beetle$Year, 8, 12))
beetle$field <- factor(beetle$Field)
beetle$sloc <- factor(beetle$Location)
beetle$snum <- factor(beetle$Trap)

beetle$man <- factor(beetle$Type)
beetle$crp <- factor(beetle$Crop)
beetle$prec <- factor(beetle$Precrop)
levels(beetle$man) <- c("con","int")


## renaming these variables didn't work, ehich is a pain!
#beetle %>%
#  rename(
#    abundance = Abundance,
#    richness = Richness
#    )

################################################################################

## plots

## plot total abundance by field/year

field_year <- beetle %>%
  group_by(field, year) %>%
  summarise(tot = sum(Abundance),
            sdtot = sd(Abundance))

ggplot(field_year, aes(x = year, y = tot)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = tot-(2*sdtot), ymax = tot+(2*sdtot)), width = 0.5) +                                                                                            
  facet_wrap(~field)
  
ggplot(field_year, aes(x = field, y = tot)) +
  geom_point() +
  geom_errorbar(aes(ymin = tot-(2*sdtot), ymax = tot+(2*sdtot)), width = 0.5) +                                                                                            
  facet_wrap(~year)

################################################################################

## as above but split additionally by management type

trt <- beetle %>%
  group_by(field, year, man) %>%
  summarise(meanab = mean(Abundance),
            sdab = sd(Abundance),
            nobs = n())

## grid of plots of conventional versus integrated for each field/year

ggplot(trt, aes(x = man, y = meanab)) +
  geom_point() +
  geom_errorbar(aes(ymin = meanab-(2*sdab), ymax = meanab+(2*sdab), width = 0.25)) +                                                                                            
  facet_grid(field ~ year) +
  xlab("Management type")
  
## plot as above but using all the raw data not averaging over all traps
## and separating crop and margin sites by colour

beetle %>%
  group_by(field, year, man, sloc)

ggplot(beetle, aes(x = man, y = Abundance, colour = sloc)) +
  geom_jitter(width = 0.15) +
  facet_grid(field ~ year) +
  xlab("Management type") +
  scale_color_manual(values = c("crop" = "steelblue", "margin" = "tomato2"),
                     labels = c("crop" = "Crop", "margin" = "Margin")) +
  labs(color = "Location")
  
################################################################################

## plot crop versus abundance 

beetle %>% group_by(crp)

ggplot(beetle, aes(x = crp, y = Abundance, color = man)) +
  geom_jitter(width = 0.15)

ggplot(beetle, aes(x = crp, y = Abundance, color = man)) +
  geom_jitter(width = 0.15) +
  facet_wrap(~field) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(beetle, aes(x = crp, y = Abundance, color = man)) +
  geom_jitter(width = 0.15) +
  facet_grid(field ~ year) +
  theme(axis.text.x = element_text(angle = 90))

################################################################################

## another plot of abundance for each year by field

sumbeet <- beetle %>% 
  group_by(field, year, man) %>%
  summarise(mean = mean(Abundance))

## first do it with one mean for each field/year/management

ggplot(sumbeet, aes(x = man, y = mean, group = year)) +
  geom_point(size=3) +
  geom_line(aes(color = factor(year), group = year)) +
  facet_wrap(~ field) +
  xlab("Management type")

## add in sampling location

sumbeet2 <- beetle %>% 
  group_by(field, year, man, sloc) %>%
  summarise(mean = mean(Abundance))

color_sloc <- c("steelblue", "tomato2")

ggplot(sumbeet2, aes(x = man, y = mean, group = interaction(year, sloc))) +
  facet_wrap(~ field) +
  geom_point(aes(fill=factor(sloc)), size=3, shape=21, stroke=0) +
  scale_fill_manual(values=color_sloc, labels = c("crop" = "Crop", "margin" = "Margin")) +
  geom_line(aes(color = factor(year), group = interaction(year, sloc))) +
  xlab("Management type") +
  labs(fill="Location", colour="Year")
  

################################################################################

## Plots for Gail 3 September 2024

## the one by precrop

sumbee2 <- beetle %>% 
  group_by(prec, year, man) %>%
  summarise(mean = mean(Abundance)) %>%
  filter(year != 2011)

## first do it with one mean for each field/year/management

ggplot(sumbee2, aes(x = man, y = mean, group = year)) +
  geom_point(size=3) +
  geom_line(aes(color = factor(year), group = year)) +
  facet_wrap(~ prec) +
  xlab("Management type") +
  labs( colour="Year")

## add in sampling location

sumbee3 <- beetle %>% 
  group_by(prec, year, man, sloc) %>%
  summarise(mean = mean(Abundance)) %>%
  filter(year != 2011)

color_sloc <- c("steelblue", "tomato2")

ggplot(sumbee3, aes(x = man, y = mean, group = interaction(year, sloc))) +
  facet_wrap(~ prec) +
  geom_point(aes(fill=factor(sloc)), size=3, shape=21, stroke=0) +
  scale_fill_manual(values=color_sloc, labels = c("crop" = "Crop", "margin" = "Margin")) +
  geom_line(aes(color = factor(year), group = interaction(year, sloc))) +
  xlab("Management type") +
  labs(fill="Location", colour="Year")

################################################################################

## the other plot faceted by year and precrop

beetx2011 <- beetle %>%
  group_by(prec, year, man, sloc) %>%
  filter(year != 2011)

ggplot(beetx2011, aes(x = man, y = Abundance, colour = sloc)) +
  geom_jitter(width = 0.15) +
  facet_grid(prec ~ year) +
  xlab("Management type") +
  scale_color_manual(values = c("crop" = "steelblue", "margin" = "tomato2"),
                     labels = c("crop" = "Crop", "margin" = "Margin")) +
  labs(color = "Location")

################################################################################

### DUMP

scale_color_manual(values = c("C" = "blue", "M" = "red"), 
                   guide = guide_legend(title = "Sampling location")) +
  scale_color_manual(values = c("2011" = "green", "2012" = "purple", 
                                "2013" = "orange", "2014" = "brown", 
                                "2015" = "pink", "2016" = "cyan",
                                "2017" = "magenta", "2024" = "grey"))

################################################################################

## Do some models
## initially just use one year of data

d2016 <- beetle %>%
  filter(year == 2016)

## fit a basic model

mod1 <- brm(data = d2016,
            family = poisson,
            formula =
              abundance ~ 1 + (1|man),
            prior = c(
              prior(normal(0, 5), class = "Intercept"),
              prior(cauchy(0, 2), class = "sd")  # Prior for standard deviation of group effects
            )
)
