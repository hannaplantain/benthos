setwd("C:/Users/Hanna/Desktop/Marine Masters Summer Course/Benthos")

install.packages("splancs") 
library(splancs)
install.packages("C:/Users/Hanna/Desktop/Marine Masters Summer Course/Benthos/RandomFields_2.0.66.tar.gz", repos = NULL, type = "source")
install.packages("C:/Users/Hanna/Desktop/Marine Masters Summer Course/Benthos/geoR_1.8-1.tar.gz", repos = NULL, type = "source")
library(ggplot2)
library(dplyr)
library(ggpubr)

dat2021 <- read.csv('2021.csv',header=T,na.strings=c(""))
dat2020 <- read.csv('2020.csv',header=T,na.strings=c(""))
dat2019 <- read.csv('2019.csv',header=T,na.strings=c(""))
dat2018 <- read.csv('2018.csv',header=T,na.strings=c(""))
dat2017 <- read.csv('2017.csv',header=T,na.strings=c(""))
dat2016 <- read.csv('2016.csv',header=T,na.strings=c(""))

##Temporal trend----
ggdensity(dat, x = "afdm_m2",
          add = "mean")
ggqqplot(dat, x="afdm_m2")
          
dat <- read.csv('alldata.csv',header=T,na.strings=c(""))
summary(dat)
dates <- as.Date(dat$date, format = "%d/%m/%Y")
dates <- data.frame(dates)
dat$time <- dat$dates
massmodel<- glm(afdm_m2  ~ time, data = dat)
summary(massmodel)

#id for sample locations
dat$coordid <- dat$x * dat$y
dat$coordid <- as.factor(dat$coordid)
#model with sample locations
massmodel2<- glm(afdm_m2  ~ time + coordid, data = dat)
summary(massmodel2)

speciesmodel <- glm(species_rich ~ time + coordid, data = dat)
summary(speciesmodel)

#plotje biomassa
massplot <-  ggplot(NULL) +
  geom_point(data=dat, aes
             (x=time, y=afdm_m2)) +
  geom_smooth(data=dat, aes
            (x=time, y=afdm_m2))+
  theme_light() + 
  ylim(0,200)
massplot

#plot coordinates
coordplot <-  ggplot(NULL) +
  geom_point(data=dat, aes
             (x=x, y=y), size=2, 
             position=position_dodge(width=0.08), alpha=0.3) +
  theme_light()
coordplot


##Wormsss----
nephthys <- read.csv('nephtys_allyears.csv',header=T,na.strings=c(""))
scoloplos <- read.csv('scoloplos_allyears.csv',header=T,na.strings=c(""))
worms <- read.csv('worms.csv',header=T,na.strings=c(""))
worms$sibesnr <- as.factor(worms$sibesnr)
coordplot <-  ggplot(NULL) +
  geom_point(data=worms, aes
             (x=x, y=y, color=sibesnr, shape=sibesnr), size=2, 
             position=position_dodge(width=0.005), alpha=0.2) +
  theme_light()
coordplot

worms$coordid <- worms$x * worms$y
worms$coordid <- as.factor(worms$coordid)
worms$

corplot <-  ggplot(NULL) +
  geom_point(data=dat, aes
             (x=silt, y=median), size=2) +
  theme_light() +
  xlim(0,20)
corplot

sediment <- ggplot(NULL) + geom_point(data=dat, aes(x=median, y=silt))
##Autocorrelation----
#https://pages.cms.hu-berlin.de/EOL/gcg_quantitative-methods/Lab14_Kriging.html#Variogram_cloud 
dat3 <- read.csv('alldata.csv',header=T,na.strings=c(""))

geodat <- as.geodata(dat3, coords.col = 4:5, data.col = 8, data.names = NULL, 
                           covar.col = NULL, covar.names = "obj.names",
                           units.m.col = NULL, realisations = NULL,
                           na.action = c("ifany", "ifdata", "ifcovar", "none"),
                           rep.data.action, rep.covar.action, rep.units.action,
                           ...)

