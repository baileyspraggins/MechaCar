library(ggplot2) #Import dependencies 

library(dplyr) #Import dependencies 

### MPG Regression
mecha <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset

names(mecha)[names(mecha) == "vehicle.length"] <- "length" #Rename columns

names(mecha)[names(mecha) == "vehicle.weight"] <- "weight" #Rename columns

names(mecha)[names(mecha) == "spoiler.angle"] <- "s_angle" #Rename columns

names(mecha)[names(mecha) == "ground.clearance"] <- "g_clear" #Rename columns

lm(mpg ~ length + weight + s_angle + g_clear + AWD, data=mecha) # Create multiple linear regression model

summary(lm(mpg ~ length + weight + s_angle + g_clear + AWD, data=mecha)) #Generate summary statistics 

mecha <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in dataset


### Suspension Coil Summary
coil <- read.csv('Suspension_Coil.csv',stringsAsFactors = F) #read in dataset

summary(coil$PSI)

var(coil$PSI)

sd(coil$PSI)

### Suspension Coil T-Test

sample_coil <- coil %>% sample_n(30) #randomly sample 50 data points

coil_s_plt <- ggplot(sample_coil,aes(x=PSI)) #import dataset into ggplot2

coil_s_plt + geom_density() #visualize distribution using density plot

t.test(sample_coil$PSI,mu=mean(coil$PSI)) #compare sample versus population means
