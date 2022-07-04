library(dplyr)
library(tidyverse)

csw = read.csv(file.choose())

csw_rp = subset(csw, csw$IP >= 20 & csw$IP < 50)
csw_sp = subset(csw, csw$IP >= 50)

csw_qualified = subset(csw, csw$IP >= 20)

avg_csw_fb = round(100 * ((sum(csw_qualified$C.Strikes) + 
                          sum(csw_qualified$Sw.Strikes)) / 
                  sum(csw_qualified$Num.Fastballs)), 1)
avg_csw_fb # 26.6%

median(csw_qualified$CSW.)


avg_rp_csw_fb = round(100 * ((sum(csw_rp$C.Strikes) + 
                             sum(csw_rp$Sw.Strikes)) / 
                     sum(csw_rp$Num.Fastballs)), 1)
avg_rp_csw_fb # 26.9%


avg_sp_csw_fb = round(100 * ((sum(csw_sp$C.Strikes) + 
                             sum(csw_sp$Sw.Strikes)) / 
                     sum(csw_sp$Num.Fastballs)), 1)
avg_sp_csw_fb # 26.4%