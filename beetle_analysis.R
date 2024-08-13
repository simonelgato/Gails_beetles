## Gail's beetle data
## 12 August 2024

## script to do data reformatting and (hopefully) do some modelling

setwd("C:/Users/notne/Research/GitHub/Gails_beetles")
library(ggplot2)
library(dplyr)
library(brms)

################################################################################

## read in data and make factors for field (field), location of sample (sloc: in crop 
## or in margin) and sample number (snum)

## I've calculated abundance and species richness already  in the input data set
## did that in Excel

beetle <- read.csv("beetles.csv")
beetle$field <- factor(substr(beetle$Label, 1, 1))
beetle$sloc <- factor(substr(beetle$Label, 4, 4))
beetle$snum <- factor(substr(beetle$Label, 5, 5))
beetle$year <- factor(substr(beetle$Season.and.Year, 8, 12))
beetle$man <- factor(beetle$Management.Type)
beetle$crp <- factor(beetle$Crop)
levels(beetle$man) <- c("con","int")

################################################################################

## plots

## plot total abundance by field/year

field_year <- beetle %>%
  group_by(field, year) %>%
  summarise(tot = sum(abundance),
            sdtot = sd(abundance))

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
  summarise(meanab = mean(abundance),
            sdab = sd(abundance),
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

ggplot(beetle, aes(x = man, y = abundance, colour = sloc)) +
  geom_jitter(width = 0.15) +
  facet_grid(field ~ year) +
  xlab("Management type") +
  scale_color_manual(values = c("C" = "steelblue", "M" = "tomato2"),
                     labels = c("C" = "Crop", "M" = "Margin")) +
  labs(color = "Location")
  
################################################################################

## plot crop versus abundance 

beetle %>% group_by(crp)

ggplot(beetle, aes(x = crp, y = abundance, color = man)) +
  geom_jitter(width = 0.15)

ggplot(beetle, aes(x = crp, y = abundance, color = man)) +
  geom_jitter(width = 0.15) +
  facet_wrap(~field) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(beetle, aes(x = crp, y = abundance, color = man)) +
  geom_jitter(width = 0.15) +
  facet_grid(field ~ year) +
  theme(axis.text.x = element_text(angle = 90))

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
