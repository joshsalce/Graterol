library(dplyr)
library(tidyverse)

#####---------------------------------------------------------------------------
savant = read.csv(file.choose())

hi_lev = subset(savant, pitches >= 100)
sort(unique(hi_lev$xwoba))

library(ggplot2)
library(ggrepel)

### 2022 High Leverage
graterol = subset(hi_lev, hi_lev$player_name == "Graterol, Brusdar")

ggplot(hi_lev, aes(x = xwoba, y = 1)) +  
  geom_jitter(height = 0.6) + 
  ylim(0, 2) +  
  theme(axis.title.y = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  coord_fixed(ratio = 0.05) +
  geom_point(data=graterol, 
           aes(x=xwoba,y=1), 
           color='red',
           size=3)


# 2021-2022 High Leverage
savant_21_22 = read.csv(file.choose())
hi_lev_21_22 = subset(savant_21_22, pitches >= 250)
sort(unique(hi_lev_21_22$xwoba))

graterol2 = subset(hi_lev_21_22, hi_lev_21_22$player_id == 660813)

ggplot(hi_lev_21_22, aes(x = xwoba, y = 1)) +  
  geom_jitter(height = 0.6) + 
  ylim(0, 2) +  
  theme(axis.title.y = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank()) +  
  coord_fixed(ratio = 0.05) +
  geom_point(data=graterol2, 
             aes(x=xwoba,y=1), 
             color='red',
             size=3)

#####------
rp_2019 = read.csv(file.choose())
rp_2019_highlev  = read.csv(file.choose())

hilev_names19 = rp_2019_highlev$Name
rp_2019 = subset(rp_2019, Name %in% hilev_names19)
rp_2019 = subset(rp_2019, !playerid %in% c(7411, 17292))

rp_2019$K. = gsub('%','', rp_2019$K.)
rp_2019$K. = as.numeric(rp_2019$K.)


new_rp_2019 = data.frame(Name = rp_2019$Name,
                         SeasonK. = rp_2019$K.)


hist(rp_2019$K.,
     main = "K% for 'Relief' Pitchers", 
     xlab = "K%",           
     ylab = "Frequency",
     col = 2,
     breaks = 40)

table = table(rp_2019$K.)

rp_2019_highlev = inner_join(rp_2019_highlev, new_rp_2019, by = "Name")

rp_2019_highlev$X1B = rp_2019_highlev$H - rp_2019_highlev$HR - rp_2019_highlev$X2B - rp_2019_highlev$X3B

# Calculating wOBA (.690	.719	.870	1.217	1.529	1.940)
low_K_2019 = subset(rp_2019_highlev, SeasonK. <= 20.0)
round(((.690*sum(low_K_2019$BB))+(.719*sum(low_K_2019$HBP))+
         (.870*sum(low_K_2019$X1B))+(1.217*sum(low_K_2019$X2B))+
         (1.529*sum(low_K_2019$X3B))+(1.940*sum(low_K_2019$HR))) 
      / (sum(low_K_2019$TBF)), 3)

med_K_2019 = subset(rp_2019_highlev, SeasonK. >= 20.0 & SeasonK. <= 25.0)
round(((.690*sum(med_K_2019$BB))+(.719*sum(med_K_2019$HBP))+
         (.870*sum(med_K_2019$X1B))+(1.217*sum(med_K_2019$X2B))+
         (1.529*sum(med_K_2019$X3B))+(1.940*sum(med_K_2019$HR))) 
      / (sum(med_K_2019$TBF)), 3)

high_K_2019 = subset(rp_2019_highlev, SeasonK. >= 25.0)
round(((.690*sum(high_K_2019$BB))+(.719*sum(high_K_2019$HBP))+
         (.870*sum(high_K_2019$X1B))+(1.217*sum(high_K_2019$X2B))+
         (1.529*sum(high_K_2019$X3B))+(1.940*sum(high_K_2019$HR))) 
      / (sum(high_K_2019$TBF)), 3)

#------
rp_2020 = read.csv(file.choose())
rp_2020_highlev = read.csv(file.choose())

hilev_names20 = rp_2020_highlev$Name
hilev_names20

rp_2020$Name[rp_2020$Name == 'Cameron Hill'] <- 'Cam Hill'
rp_2020$Name[rp_2020$Name == 'Kwang-hyun Kim'] <- "Kwang Hyun Kim"

rp_2020 = subset(rp_2020, Name %in% hilev_names20)

rp_2020$K. = gsub('%','', rp_2020$K.)
rp_2020$K. = as.numeric(rp_2020$K.)

new_rp_2020 = data.frame(Name = rp_2020$Name,
                         SeasonK. = rp_2020$K.)

rp_2020_highlev = inner_join(rp_2020_highlev, new_rp_2020, 
                             by = "Name")

rp_2020_highlev = rp_2020_highlev[-c(385, 50, 58, 278),] 

rp_2020_highlev$X1B = rp_2020_highlev$H - rp_2020_highlev$HR - rp_2020_highlev$X2B - rp_2020_highlev$X3B

# wOBA (.699 .728	.883	1.238	1.558	1.979)
low_K_2020 = subset(rp_2020_highlev, SeasonK. <= 20.0)
round(((.699*sum(low_K_2020$BB))+(.728*sum(low_K_2020$HBP))+
         (.883*sum(low_K_2020$X1B))+(1.238*sum(low_K_2020$X2B))+
         (1.558*sum(low_K_2020$X3B))+(1.979*sum(low_K_2020$HR))) 
      / (sum(low_K_2020$TBF)), 3)

med_K_2020 = subset(rp_2020_highlev, SeasonK. >= 20.0 & SeasonK. <= 25.0)
round(((.699*sum(med_K_2020$BB))+(.728*sum(med_K_2020$HBP))+
         (.883*sum(med_K_2020$X1B))+(1.238*sum(med_K_2020$X2B))+
         (1.558*sum(med_K_2020$X3B))+(1.979*sum(med_K_2020$HR))) 
      / (sum(med_K_2020$TBF)), 3)

high_K_2020 = subset(rp_2020_highlev, SeasonK. >= 25.0)
round(((.699*sum(high_K_2020$BB))+(.728*sum(high_K_2020$HBP))+
         (.883*sum(high_K_2020$X1B))+(1.238*sum(high_K_2020$X2B))+
         (1.558*sum(high_K_2020$X3B))+(1.979*sum(high_K_2020$HR))) 
      / (sum(high_K_2020$TBF)), 3)


#------

rp_2021 = read.csv(file.choose())
rp_2021_highlev = read.csv(file.choose())

rp_2021$Name[rp_2021$Name == 'Kwang-hyun Kim'] <- "Kwang Hyun Kim"
rp_2021$Name[rp_2021$Name == 'Ralph Garza'] <- "Ralph Garza Jr."
rp_2021$Name[rp_2021$Name == 'Sammy Long'] <- "Sam Long"

hilev_names21 = rp_2021_highlev$Name
rp_2021 = subset(rp_2021, Name %in% hilev_names21)
rp_2021 = subset(rp_2021, playerid != 23735)

rp_2021$K. = gsub('%','', rp_2021$K.)
rp_2021$K. = as.numeric(rp_2021$K.)

new_rp_2021 = data.frame(Name = rp_2021$Name,
                         SeasonK. = rp_2021$K.)

rp_2021_highlev = inner_join(rp_2021_highlev, new_rp_2021, 
                             by = "Name")
rp_2021_highlev$X1B = rp_2021_highlev$H - rp_2021_highlev$HR - rp_2021_highlev$X2B - rp_2021_highlev$X3B

# Calculating wOBA (.692	.722	.879	1.242	1.568	2.007)
low_K_2021 = subset(rp_2021_highlev, SeasonK. <= 20.0)
round(((.692*sum(low_K_2021$BB))+(.722*sum(low_K_2021$HBP))+
         (.879*sum(low_K_2021$X1B))+(1.242*sum(low_K_2021$X2B))+
         (1.568*sum(low_K_2021$X3B))+(2.007*sum(low_K_2021$HR))) 
      / (sum(low_K_2021$TBF)), 3)

med_K_2021 = subset(rp_2021_highlev, SeasonK. >= 20.0 & SeasonK. <= 25.0)
round(((.692*sum(med_K_2021$BB))+(.722*sum(med_K_2021$HBP))+
         (.879*sum(med_K_2021$X1B))+(1.242*sum(med_K_2021$X2B))+
         (1.568*sum(med_K_2021$X3B))+(2.007*sum(med_K_2021$HR))) 
      / (sum(med_K_2021$TBF)), 3)

high_K_2021 = subset(rp_2021_highlev, SeasonK. >= 25.0)
round(((.692*sum(high_K_2021$BB))+(.722*sum(high_K_2021$HBP))+
         (.879*sum(high_K_2021$X1B))+(1.242*sum(high_K_2021$X2B))+
         (1.568*sum(high_K_2021$X3B))+(2.007*sum(high_K_2021$HR))) 
      / (sum(high_K_2021$TBF)), 3)


#------