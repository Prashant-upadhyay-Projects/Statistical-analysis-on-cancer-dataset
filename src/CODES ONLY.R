melanoma <- read_csv("C:/Users/princ/Desktop/STATS FOR DATA SCIENCE/TUTORIAL FOR STATS/melanoma.csv") 

View(melanoma)

data <- as_tibble(melanoma)

print(data)


print(melanoma)    


library(ggplot2)
library(ggthemes)
library(tidyverse)
library(corrplot)
library(knitr)
library(readr)
library(ggpubr)
library(ggridges)
library(plotrix)
library(pastecs)
library(psych)
library(Hmisc)


summary(melanoma)

melanoma$status <- factor(melanoma$status)
melanoma$sex <- factor(melanoma$sex)
melanoma$ulcer <- factor(melanoma$ulcer)
melanoma$year <- factor(melanoma$year)   

melanoma <- melanoma[,-1]   


melanoma <- melanoma %>%
  mutate(sex = recode_factor(sex, `0` = "female", `1` = "male")) 
                                                              
melanoma <- melanoma %>%
  mutate(status = recode_factor(status, `1` = "died", `2` = "alive", `3` = "other")) 
                                                                  
melanoma <- melanoma %>%
  mutate(ulcer = recode_factor(ulcer, `0` = "absent", `1` = "present"))


summary(melanoma)   


var(melanoma$time)      
sd(melanoma$time)       
var(melanoma$age)
sd(melanoma$age)
var(melanoma$thickness)
sd(melanoma$thickness)


par(mfrow=c(1,1))
hist(melanoma$time)
hist(melanoma$age)        
hist(melanoma$thickness)    

par(mfrow=c(2,2))
hist(melanoma$time)
hist(melanoma$age)        
hist(melanoma$thickness)


par(mfrow=c(1,1))
boxplot(melanoma$time ~ melanoma$status)
boxplot(melanoma$age ~ melanoma$status)
boxplot(melanoma$thickness ~ melanoma$status)

par(mfrow=c(2,2))
boxplot(melanoma$time ~ melanoma$status)
boxplot(melanoma$age ~ melanoma$status)
boxplot(melanoma$thickness ~ melanoma$status)


par(mfrow=c(1,1))
boxplot(melanoma$time ~ melanoma$sex)
boxplot(melanoma$age ~ melanoma$sex)
boxplot(melanoma$thickness ~ melanoma$sex)

par(mfrow=c(2,2))
boxplot(melanoma$time ~ melanoma$sex)
boxplot(melanoma$age ~ melanoma$sex)
boxplot(melanoma$thickness ~ melanoma$sex)



par(mfrow=c(1,1))
boxplot(melanoma$time ~ melanoma$ulcer)
boxplot(melanoma$age ~ melanoma$ulcer)
boxplot(melanoma$thickness ~ melanoma$ulcer)

par(mfrow=c(2,2))
boxplot(melanoma$time ~ melanoma$ulcer)
boxplot(melanoma$age ~ melanoma$ulcer)
boxplot(melanoma$thickness ~ melanoma$ulcer)


par(mfrow=c(1,1))


ggplot(melanoma, aes(time)) +
  geom_histogram(aes(fill = status, color = status), bins = 20, 
                 position = "identity", alpha = 0.5) +
  scale_fill_viridis_d() +
  scale_color_viridis_d()

ggplot(melanoma, aes(x = ulcer, y = time, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Time of Survival based on Ulcer and Gender",
       x = "ulcer", y = "time") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
  theme_minimal()


library(readxl)

plot(melanoma)  


needed_columns <- c('time', 'age', 'thickness')  

new_data <- melanoma[, needed_columns]           

cor_matrix <- cor(new_data)

print(cor_matrix)              

cr<-cor(new_data)          

corrplot(cr)              



plot(melanoma$thickness, melanoma$time)  

LinReg <- lm(melanoma$time ~ melanoma$thickness)

abline(LinReg, col = "green", lwd = 2)       


plot(melanoma$age, melanoma$time)

LinReg <- lm(melanoma$time ~ melanoma$age)

abline(LinReg, col = "green", lwd = 2)


plot(melanoma$age, melanoma$thickness)

LinReg <- lm(melanoma$thickness ~ melanoma$age)

abline(LinReg, col = "red", lwd = 5)



my_model=lm(formula = melanoma$time~melanoma$thickness)

my_model


my_model=lm(formula = melanoma$time~melanoma$age)

my_model


my_model=lm(formula = melanoma$thickness~melanoma$age)

my_model                    




qplot(x = sex, y = time,
      geom = "boxplot", data = melanoma,
      xlab = "Gender",
      ylab = "Survival time (days)",
      fill = I("pink"))                      


female <- filter(melanoma, melanoma$sex=='female')

mean(female$time)

male <- filter(melanoma, sex=='male')

mean(male$time)


melanoma %>%
  group_by(sex) %>%
  summarise(mean_time = mean(time),
            sd_time = sd(time))


melanoma %>%
  group_by(sex) %>%
  summarise(num.obs = n(),
            mean_time = round(mean(time), 0),
            sd_time = round(sd(time), 0),
            se_time = round(sd(time) / sqrt(num.obs), 0))    



time_t_test <- t.test(time ~ sex, data = melanoma)

time_t_test     




ggplot(melanoma, aes(x = time)) +
  geom_histogram()+
  facet_wrap(~sex, ncol = 1)

melanoma %>%
  group_by(sex) %>%
  summarise(var(time))

model <- aov(time ~ sex, data = melanoma)

summary(model)


TukeyHSD(model)  


qplot(x = sex, y = age,
      geom = "boxplot", data = melanoma,
      xlab = "Gender",
      ylab = "Age at operation (years)",
      fill = I("yellow"))


female <- filter(melanoma, melanoma$sex=='female')

mean(female$age)

male <- filter(melanoma, sex=='male')

mean(male$age)


melanoma %>%
  group_by(sex) %>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))

melanoma %>%
  group_by(sex) %>%
  summarise(num.obs = n(),
            mean_age = round(mean(age), 0),
            sd_age = round(sd(age), 0),
            se_age = round(sd(age) / sqrt(num.obs), 0))    

age_t_test <- t.test(age ~ sex, data = melanoma)

age_t_test         



ggplot(melanoma, aes(x = age)) +
  geom_histogram()+
  facet_wrap(~sex, ncol = 1)

melanoma %>%
  group_by(sex) %>%
  summarise(var(age))

model <- aov(age ~ sex, data = melanoma)

summary(model)


TukeyHSD(model)  





qplot(x = sex, y = thickness,
      geom = "boxplot", data = melanoma,
      xlab = "Gender",
      ylab = "Thickness of tumour (mm)",
      fill = I("orange"))


female <- filter(melanoma, melanoma$sex=='female')

mean(female$thickness)

male <- filter(melanoma, sex=='male')

mean(male$thickness)


melanoma %>%
  group_by(sex) %>%
  summarise(mean_thickness = mean(thickness),
            sd_thickness = sd(thickness))

melanoma %>%
  group_by(sex) %>%
  summarise(num.obs = n(),
            mean_thickness = round(mean(thickness), 0),
            sd_thickness = round(sd(thickness), 0),
            se_thickness = round(sd(thickness) / sqrt(num.obs), 0))  

thickness_t_test <- t.test(thickness ~ sex, data = melanoma)

thickness_t_test         




ggplot(melanoma, aes(x = thickness)) +
  geom_histogram()+
  facet_wrap(~sex, ncol = 1)

melanoma %>%
  group_by(sex) %>%
  summarise(var(thickness))

model <- aov(thickness ~ sex, data = melanoma)

summary(model)



TukeyHSD(model) 




p_time <- ggplot(data = melanoma, aes(sample = time))

p_time + stat_qq() + stat_qq_line()

p_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)



p_age <- ggplot(data = melanoma, aes(sample = age))

p_age + stat_qq() + stat_qq_line()

p_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)



p_thickness <- ggplot(data = melanoma, aes(sample = thickness))

p_thickness + stat_qq() + stat_qq_line()

p_thickness + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)


                                                                  