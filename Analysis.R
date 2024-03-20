library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)
library(tidyverse)
library(ggrepel)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')
Dengue <- read.csv("DengueAndWeatherDataRough2.csv")


summary(Dengue$DC[Dengue$Month==1])
summary(Dengue$Rainfall[Dengue$Month==3])

fmonthwise <- Dengue[which(Dengue$Year<='2022'), ]
sum(fmonthwise$DC)
sum(fmonthwise$DD)

mean(fmonthwise$Rainfall)

agg = aggregate(fmonthwise$Rainfall, by = list(fmonthwise$Year), FUN = sum, na.omit = T)
mean(agg$x)

mean(fmonthwise$AvgT)

describe.by(fmonthwise$Rainfall,fmonthwise$Month)
describe.by(fmonthwise$AvgT,fmonthwise$Month)

afmonthwise <- Dengue[which(Dengue$Year>'2022'), ]
sum(afmonthwise$DC)
sum(afmonthwise$DD)

sum(afmonthwise$Rainfall)

mean(afmonthwise$AvgT)

describe.by(afmonthwise$Rainfall,afmonthwise$Month)


a <- ggplot(fmonthwise, aes(x=as.factor(Month), y=log10(DC+1), colour=Group)) + 
  geom_boxplot(fill="darkgreen", alpha=0.5) + 
  geom_line(data = afmonthwise,  
            mapping = aes(x=as.factor(Month), y=log10(DC+1), group=1, colour=Group), size = 1.5) +
  ylab("Monthly Dengue Cases (log10)") + xlab("") + ggtitle("Monthly Dengue Cases, Bangladesh (2000-2023)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5) ) +
  scale_color_manual(values=c("darkgreen", "black"))
a




b <- ggplot(fmonthwise, aes(x=as.factor(Month), y=log10(DD+1), colour=Group)) + 
  geom_boxplot(fill="darkred", alpha=0.5) + 
  geom_line(data = afmonthwise,  
            mapping = aes(x=as.factor(Month), y=log10(DD+1), group=1, colour=Group), size = 1.5) +
  ylab("Monthly Dengue Deaths (log10)") + xlab("Months") + ggtitle("Monthly Dengue Deaths, Bangladesh (2000-2023)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5) ) +
  scale_color_manual(values=c("darkred", "black")) 

b


tiff("BoxDCDD.tiff", units="in", width=8, height=10, res=300)
gridExtra::grid.arrange(a,b)
dev.off()

# agg = aggregate(Dengue$Rainfall, by = list(Dengue$Year), FUN = sum, na.omit = T)
# 
# 
# x<-ggplot(data=agg, aes(x=as.factor(Group.1), y=x)) +
#   geom_bar(stat="identity", fill="steelblue") +   ylab("Yearly Total Rainfall (mm)") + xlab("") + theme_minimal()+  ggtitle("Annual Rainfall Dhaka, Bangladesh (2000-2023)") +theme(axis.text=element_text(size=12,face="bold"),
#                                                                   axis.title=element_text(size=14,face="bold"),
#                                                                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),)+
#   theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5,) ) +
#   scale_color_manual(values=c("#8196ab", "black"))
# x
# 
# 
# agg = aggregate(Dengue$AvgT, by = list(Dengue$Year), FUN = mean, na.omit = T)
# 
# 
# y <- ggplot(Dengue, aes(x=as.factor(Year), y=AvgT)) + 
#   geom_boxplot(fill="coral", alpha=0.5) +   ylab("Monthly Average Temperature (°C)") + xlab("Year") + 
#   ggtitle("Annual Temperature Dhaka, Bangladesh (2000-2023)") +theme(axis.text=element_text(size=12,face="bold"),
#                                                                    axis.title=element_text(size=14,face="bold"),
#                                                                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),)+
#   theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5,) ) +
#   scale_color_manual(values=c("red", "red"))
# 
# y

x <- ggplot(fmonthwise, aes(x=as.factor(Month), y=Rainfall, colour=Group)) + 
  geom_boxplot(fill="darkgreen", alpha=0.5) + 
  geom_line(data = afmonthwise,  
            mapping = aes(x=as.factor(Month), y=Rainfall, group=1, colour=Group), size = 1.5) +
  ylab("Monthly Total Rainfall (mm)") + xlab("") + ggtitle("Monthly Total Rainfall, Dhaka (2000-2023)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5) ) +
  scale_color_manual(values=c("darkgreen", "black"))
x




y <- ggplot(fmonthwise, aes(x=as.factor(Month), y=AvgT, colour=Group)) + 
  geom_boxplot(fill="darkred", alpha=0.5) + 
  geom_line(data = afmonthwise,  
            mapping = aes(x=as.factor(Month), y=AvgT, group=1, colour=Group), size = 1.5) +
  ylab("Monthly Average Temperature (°C)") + xlab("Months") + ggtitle("Monthly Average Temperature, Dhaka (2000-2023)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5) ) +
  scale_color_manual(values=c("darkred", "black")) 

y


tiff("BoxLine.tiff", units="in", width=8, height=10, res=300)
gridExtra::grid.arrange(x,y)
dev.off()


setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023")

Dailydata <- read.csv("DengueDGHSDataCasesDeathAge.csv", header = T)

NROW(Dailydata)

dat <- data.frame(ds = seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = 'd'),
                  y =data.frame(Dailydata$Cumulative.Case.inside.Dhaka.City[1:365],Dailydata$Cumulative.Case.outside.Dhaka.City[1:365]))

colnames(dat) <- c("Date", "Inside Dhaka City", "Outside Dhaka City")

library("tidyverse")
df <- dat %>%
  select(Date, "Inside Dhaka City", "Outside Dhaka City") %>%
  gather(key = "Characteristics", value = "value", -Date)
head(df)

# Visualization
case <- ggplot(df, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = Characteristics, linetype = Characteristics),cex=4) + 
  scale_color_manual(values = c("darkred", "blue"))+  xlab("Dates") + ylab("Cases (log10)") + 
  theme(legend.title = element_text(size=60),
        legend.text = element_text(size=80),
        legend.position = c(0.18, 0.85),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        text=element_text(size=60)) 

dates_vline <- as.Date(c("2023-06-28"))                 # Define positions of vline
dates_vline <- which(df$Date %in% dates_vline)

case <- case + geom_vline(xintercept = as.numeric(df$Date[dates_vline]),
                          col = "black", lwd = 2)

case

dat <- data.frame(ds = seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by = 'd'),
                  y =data.frame(Dailydata$Cumulative.Death.inside.Dhaka.City[1:365],Dailydata$Cumulative.Death.outside.Dhaka.City[1:365]))

colnames(dat) <- c("Date", "Inside Dhaka City", "Outside Dhaka City")

library("tidyverse")
df <- dat %>%
  select(Date, "Inside Dhaka City", "Outside Dhaka City") %>%
  gather(key = "Characteristics", value = "value", -Date)
head(df)

# Visualization
death <- ggplot(df, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = Characteristics, linetype = Characteristics),cex=4) + 
  scale_color_manual(values = c("darkred", "blue"))+ xlab("Dates")  + 
  ylab("Deaths (log10)") + theme(legend.title = element_text(size=60),
                                                legend.text = element_text(size=80),
                                                legend.position = c(0.18, 0.85),
                                                plot.title = element_text(hjust = 0.5),
                                                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
                                                text=element_text(size=60))

dates_vline <- as.Date(c("2023-06-28"))                 # Define positions of vline
dates_vline <- which(df$Date %in% dates_vline)

death <- death + geom_vline(xintercept = as.numeric(df$Date[dates_vline]),
                            col = "black", lwd = 2)
death

library(gridExtra)
tiff("LineGraph.tiff", units="in", width=35, height=40, res=300)
gridExtra::grid.arrange(case,death, ncol=1, nrow=2)
dev.off()


sldata <- read.csv("DistrictWise.csv", header = T)


shp <- readOGR(dsn = "bgd_adm_bbs_20201113_SHP", "bgd_admbnda_adm2_bbs_20201113")

head(shp@data)
shp@data$ADM2_EN

sldata <- sldata[order(sldata$ADM2_EN),]

joined_df <- merge(shp@data, sldata, by = "ADM2_EN")

q_1 <- fortify(shp, region = "ADM2_EN")


library(dplyr)

q_1 <- q_1 %>%
  mutate(prev = case_when(q_1$id=="Bagerhat" ~ sldata$Cases[1],q_1$id=="Bandarban" ~ sldata$Cases[2],q_1$id=="Barguna" ~ sldata$Cases[3],q_1$id=="Barisal" ~ sldata$Cases[4],
                          q_1$id=="Bhola" ~ sldata$Cases[5],q_1$id=="Bogra" ~ sldata$Cases[6],q_1$id=="Brahamanbaria" ~ sldata$Cases[7],q_1$id=="Chandpur" ~ sldata$Cases[8],
                          q_1$id=="Chittagong" ~ sldata$Cases[9],q_1$id=="Chuadanga" ~ sldata$Cases[10],q_1$id=="Comilla" ~ sldata$Cases[11],q_1$id=="Cox's Bazar" ~ sldata$Cases[12],
                          q_1$id=="Dhaka" ~ sldata$Cases[13],q_1$id=="Dinajpur" ~ sldata$Cases[14],q_1$id=="Faridpur" ~ sldata$Cases[15],q_1$id=="Feni" ~ sldata$Cases[16],
                          q_1$id=="Gaibandha" ~ sldata$Cases[17],q_1$id=="Gazipur" ~ sldata$Cases[18],q_1$id=="Gopalganj" ~ sldata$Cases[19],q_1$id=="Habiganj" ~ sldata$Cases[20],
                          q_1$id=="Jamalpur" ~ sldata$Cases[21],q_1$id=="Jessore" ~ sldata$Cases[22],q_1$id=="Jhalokati" ~ sldata$Cases[23],q_1$id=="Jhenaidah" ~ sldata$Cases[24],
                          q_1$id=="Joypurhat" ~ sldata$Cases[25],q_1$id=="Khagrachhari" ~ sldata$Cases[26],q_1$id=="Khulna" ~ sldata$Cases[27],q_1$id=="Kishoreganj" ~ sldata$Cases[28],
                          q_1$id=="Kurigram" ~ sldata$Cases[29],q_1$id=="Kushtia" ~ sldata$Cases[30],q_1$id=="Lakshmipur" ~ sldata$Cases[31],q_1$id=="Lalmonirhat" ~ sldata$Cases[32],
                          q_1$id=="Madaripur" ~ sldata$Cases[33],q_1$id=="Magura" ~ sldata$Cases[34],q_1$id=="Manikganj" ~ sldata$Cases[35],q_1$id=="Maulvibazar" ~ sldata$Cases[36],
                          q_1$id=="Meherpur" ~ sldata$Cases[37],q_1$id=="Munshiganj" ~ sldata$Cases[38],q_1$id=="Mymensingh" ~ sldata$Cases[39],q_1$id=="Naogaon" ~ sldata$Cases[40],
                          q_1$id=="Narail" ~ sldata$Cases[41],q_1$id=="Narayanganj" ~ sldata$Cases[42],q_1$id=="Narsingdi" ~ sldata$Cases[43],q_1$id=="Natore" ~ sldata$Cases[44],
                          q_1$id=="Nawabganj" ~ sldata$Cases[45],q_1$id=="Netrakona" ~ sldata$Cases[46],q_1$id=="Nilphamari" ~ sldata$Cases[47],q_1$id=="Noakhali" ~ sldata$Cases[48],
                          q_1$id=="Pabna" ~ sldata$Cases[49],q_1$id=="Panchagarh" ~ sldata$Cases[50],q_1$id=="Patuakhali" ~ sldata$Cases[51],q_1$id=="Pirojpur" ~ sldata$Cases[52],
                          q_1$id=="Rajbari" ~ sldata$Cases[53],q_1$id=="Rajshahi" ~ sldata$Cases[54],q_1$id=="Rangamati" ~ sldata$Cases[55],q_1$id=="Rangpur" ~ sldata$Cases[56],
                          q_1$id=="Satkhira" ~ sldata$Cases[57],q_1$id=="Shariatpur" ~ sldata$Cases[58],q_1$id=="Sherpur" ~ sldata$Cases[59],q_1$id=="Sirajganj" ~ sldata$Cases[60],
                          q_1$id=="Sunamganj" ~ sldata$Cases[61],q_1$id=="Sylhet" ~ sldata$Cases[62],q_1$id=="Tangail" ~ sldata$Cases[63],q_1$id=="Thakurgaon" ~ sldata$Cases[64],
  ))

Casesmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=log10(prev+1)),colour= "lightgrey") +
  geom_text(data=joined_df,aes(label = Cases, x = Longitude, y = Latitude),color='black',size=6,fontface = "bold")+
  scale_fill_distiller(name='Cases (log10)',palette ="Greens", direction=1)+labs(title = "Number of Dengue Cases (Districtwise)") +
  xlab(label="Longitute") + ylab(label="Latitute")+ 
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        plot.title = element_text(size = 35),
        legend.title = element_text(size=35),
        legend.text = element_text(size=35))+geom_hline(yintercept=23.8,linetype=5, color="red", size = 1.5)

Casesmap1




q_1 <- q_1 %>%
  mutate(prevD = case_when(q_1$id=="Bagerhat" ~ sldata$Deaths[1],q_1$id=="Bandarban" ~ sldata$Deaths[2],q_1$id=="Barguna" ~ sldata$Deaths[3],q_1$id=="Barisal" ~ sldata$Deaths[4],
                           q_1$id=="Bhola" ~ sldata$Deaths[5],q_1$id=="Bogra" ~ sldata$Deaths[6],q_1$id=="Brahamanbaria" ~ sldata$Deaths[7],q_1$id=="Chandpur" ~ sldata$Deaths[8],
                           q_1$id=="Chittagong" ~ sldata$Deaths[9],q_1$id=="Chuadanga" ~ sldata$Deaths[10],q_1$id=="Comilla" ~ sldata$Deaths[11],q_1$id=="Cox's Bazar" ~ sldata$Deaths[12],
                           q_1$id=="Dhaka" ~ sldata$Deaths[13],q_1$id=="Dinajpur" ~ sldata$Deaths[14],q_1$id=="Faridpur" ~ sldata$Deaths[15],q_1$id=="Feni" ~ sldata$Deaths[16],
                           q_1$id=="Gaibandha" ~ sldata$Deaths[17],q_1$id=="Gazipur" ~ sldata$Deaths[18],q_1$id=="Gopalganj" ~ sldata$Deaths[19],q_1$id=="Habiganj" ~ sldata$Deaths[20],
                           q_1$id=="Jamalpur" ~ sldata$Deaths[21],q_1$id=="Jessore" ~ sldata$Deaths[22],q_1$id=="Jhalokati" ~ sldata$Deaths[23],q_1$id=="Jhenaidah" ~ sldata$Deaths[24],
                           q_1$id=="Joypurhat" ~ sldata$Deaths[25],q_1$id=="Khagrachhari" ~ sldata$Deaths[26],q_1$id=="Khulna" ~ sldata$Deaths[27],q_1$id=="Kishoreganj" ~ sldata$Deaths[28],
                           q_1$id=="Kurigram" ~ sldata$Deaths[29],q_1$id=="Kushtia" ~ sldata$Deaths[30],q_1$id=="Lakshmipur" ~ sldata$Deaths[31],q_1$id=="Lalmonirhat" ~ sldata$Deaths[32],
                           q_1$id=="Madaripur" ~ sldata$Deaths[33],q_1$id=="Magura" ~ sldata$Deaths[34],q_1$id=="Manikganj" ~ sldata$Deaths[35],q_1$id=="Maulvibazar" ~ sldata$Deaths[36],
                           q_1$id=="Meherpur" ~ sldata$Deaths[37],q_1$id=="Munshiganj" ~ sldata$Deaths[38],q_1$id=="Mymensingh" ~ sldata$Deaths[39],q_1$id=="Naogaon" ~ sldata$Deaths[40],
                           q_1$id=="Narail" ~ sldata$Deaths[41],q_1$id=="Narayanganj" ~ sldata$Deaths[42],q_1$id=="Narsingdi" ~ sldata$Deaths[43],q_1$id=="Natore" ~ sldata$Deaths[44],
                           q_1$id=="Nawabganj" ~ sldata$Deaths[45],q_1$id=="Netrakona" ~ sldata$Deaths[46],q_1$id=="Nilphamari" ~ sldata$Deaths[47],q_1$id=="Noakhali" ~ sldata$Deaths[48],
                           q_1$id=="Pabna" ~ sldata$Deaths[49],q_1$id=="Panchagarh" ~ sldata$Deaths[50],q_1$id=="Patuakhali" ~ sldata$Deaths[51],q_1$id=="Pirojpur" ~ sldata$Deaths[52],
                           q_1$id=="Rajbari" ~ sldata$Deaths[53],q_1$id=="Rajshahi" ~ sldata$Deaths[54],q_1$id=="Rangamati" ~ sldata$Deaths[55],q_1$id=="Rangpur" ~ sldata$Deaths[56],
                           q_1$id=="Satkhira" ~ sldata$Deaths[57],q_1$id=="Shariatpur" ~ sldata$Deaths[58],q_1$id=="Sherpur" ~ sldata$Deaths[59],q_1$id=="Sirajganj" ~ sldata$Deaths[60],
                           q_1$id=="Sunamganj" ~ sldata$Deaths[61],q_1$id=="Sylhet" ~ sldata$Deaths[62],q_1$id=="Tangail" ~ sldata$Deaths[63],q_1$id=="Thakurgaon" ~ sldata$Deaths[64],
  ))

Deathsmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=log10(prevD+1)),colour= "lightgrey") +
  geom_text(data=joined_df,aes(label = Deaths, x = Longitude, y = Latitude),color='black',size=6,fontface = "bold")+
  scale_fill_distiller(name='Deaths (log10)',palette ="YlOrRd", direction=1)+labs(title = "Number of Dengue Deaths (Districtwise)") +
  xlab(label="Longitute") + ylab(label="Latitute")+ 
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        plot.title = element_text(size = 35),
        legend.title = element_text(size=35),
        legend.text = element_text(size=35))+geom_hline(yintercept=23.8,linetype=5, color="red", size = 1.5)

Deathsmap1


q_1 <- q_1 %>%
  mutate(prevI = case_when(q_1$id=="Bagerhat" ~ sldata$IR[1],q_1$id=="Bandarban" ~ sldata$IR[2],q_1$id=="Barguna" ~ sldata$IR[3],q_1$id=="Barisal" ~ sldata$IR[4],
                           q_1$id=="Bhola" ~ sldata$IR[5],q_1$id=="Bogra" ~ sldata$IR[6],q_1$id=="Brahamanbaria" ~ sldata$IR[7],q_1$id=="Chandpur" ~ sldata$IR[8],
                           q_1$id=="Chittagong" ~ sldata$IR[9],q_1$id=="Chuadanga" ~ sldata$IR[10],q_1$id=="Comilla" ~ sldata$IR[11],q_1$id=="Cox's Bazar" ~ sldata$IR[12],
                           q_1$id=="Dhaka" ~ sldata$IR[13],q_1$id=="Dinajpur" ~ sldata$IR[14],q_1$id=="Faridpur" ~ sldata$IR[15],q_1$id=="Feni" ~ sldata$IR[16],
                           q_1$id=="Gaibandha" ~ sldata$IR[17],q_1$id=="Gazipur" ~ sldata$IR[18],q_1$id=="Gopalganj" ~ sldata$IR[19],q_1$id=="Habiganj" ~ sldata$IR[20],
                           q_1$id=="Jamalpur" ~ sldata$IR[21],q_1$id=="Jessore" ~ sldata$IR[22],q_1$id=="Jhalokati" ~ sldata$IR[23],q_1$id=="Jhenaidah" ~ sldata$IR[24],
                           q_1$id=="Joypurhat" ~ sldata$IR[25],q_1$id=="Khagrachhari" ~ sldata$IR[26],q_1$id=="Khulna" ~ sldata$IR[27],q_1$id=="Kishoreganj" ~ sldata$IR[28],
                           q_1$id=="Kurigram" ~ sldata$IR[29],q_1$id=="Kushtia" ~ sldata$IR[30],q_1$id=="Lakshmipur" ~ sldata$IR[31],q_1$id=="Lalmonirhat" ~ sldata$IR[32],
                           q_1$id=="Madaripur" ~ sldata$IR[33],q_1$id=="Magura" ~ sldata$IR[34],q_1$id=="Manikganj" ~ sldata$IR[35],q_1$id=="Maulvibazar" ~ sldata$IR[36],
                           q_1$id=="Meherpur" ~ sldata$IR[37],q_1$id=="Munshiganj" ~ sldata$IR[38],q_1$id=="Mymensingh" ~ sldata$IR[39],q_1$id=="Naogaon" ~ sldata$IR[40],
                           q_1$id=="Narail" ~ sldata$IR[41],q_1$id=="Narayanganj" ~ sldata$IR[42],q_1$id=="Narsingdi" ~ sldata$IR[43],q_1$id=="Natore" ~ sldata$IR[44],
                           q_1$id=="Nawabganj" ~ sldata$IR[45],q_1$id=="Netrakona" ~ sldata$IR[46],q_1$id=="Nilphamari" ~ sldata$IR[47],q_1$id=="Noakhali" ~ sldata$IR[48],
                           q_1$id=="Pabna" ~ sldata$IR[49],q_1$id=="Panchagarh" ~ sldata$IR[50],q_1$id=="Patuakhali" ~ sldata$IR[51],q_1$id=="Pirojpur" ~ sldata$IR[52],
                           q_1$id=="Rajbari" ~ sldata$IR[53],q_1$id=="Rajshahi" ~ sldata$IR[54],q_1$id=="Rangamati" ~ sldata$IR[55],q_1$id=="Rangpur" ~ sldata$IR[56],
                           q_1$id=="Satkhira" ~ sldata$IR[57],q_1$id=="Shariatpur" ~ sldata$IR[58],q_1$id=="Sherpur" ~ sldata$IR[59],q_1$id=="Sirajganj" ~ sldata$IR[60],
                           q_1$id=="Sunamganj" ~ sldata$IR[61],q_1$id=="Sylhet" ~ sldata$IR[62],q_1$id=="Tangail" ~ sldata$IR[63],q_1$id=="Thakurgaon" ~ sldata$IR[64],
  ))

IRmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=round(prevI,2)),colour= "lightgrey") +
  geom_text(data=joined_df,aes(label = IR, x = Longitude, y = Latitude),color='black',size=10,fontface = "bold")+
  scale_fill_distiller(name='Incidence Rate',palette ="BuPu", direction=1)+labs(title = "Incidence Rate (Districtwise)") +
  xlab(label="Longitute") + ylab(label="Latitute")+ 
  theme(axis.text = element_text(size = 35),
        axis.title = element_text(size = 35),
        plot.title = element_text(size = 35),
        legend.title = element_text(size=25),
        legend.text = element_text(size=25))+geom_hline(yintercept=23.8,linetype=5, color="red", size = 1.5)

IRmap1



tiff("DistrictwiseDengue.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(Casesmap1,IRmap1, nrow=1)
dev.off()

tiff("IR.tiff", units="in", width=20, height=20, res=300)
gridExtra::grid.arrange(IRmap1, nrow=1)
dev.off()




cor.test(sldata$Cases,log10(sldata$PopSize))
cor.test(sldata$Deaths,log10(sldata$PopSize))

cor.test(sldata$Cases,log10(sldata$PD))
cor.test(sldata$Deaths,log10(sldata$PD))

cor.test(sldata$Cases,sldata$Dist)
cor.test(sldata$Deaths,sldata$Dist)

library(ggplot2)
library(ggrepel)
a <- ggplot(sldata, aes(x = log10(PopSize), y = Cases))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Population Size (log10)") + ylab("Total Dengue Cases (With Dhaka City)")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
a

dat <- sldata[-which(rownames(sldata) == "1"), ]

b <- ggplot(dat, aes(x = log10(PopSize), y = Cases))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Population Size (log10)") + ylab("Total Dengue Cases (Without Dhaka City)")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
b










library(ggplot2)
library(ggrepel)
x <- ggplot(sldata, aes(x = log10(PD), y = Cases))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Population Density (log10)") + ylab("Total Dengue Cases (With Dhaka City)")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
x

dat <- sldata[-which(rownames(sldata) == "1"), ]

cor.test(dat$Cases,log10(dat$PopSize))
cor.test(dat$Deaths,log10(dat$PopSize))

cor.test(dat$Cases,log10(dat$PD))
cor.test(dat$Deaths,log10(dat$PD))

cor.test(dat$Cases,dat$Dist)
cor.test(dat$Deaths,log10(dat$Dist))


y <- ggplot(dat, aes(x = log10(PD), y = Cases))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Population Density (log10)") + ylab("Total Dengue Cases (Without Dhaka City)")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
y





dat <- sldata[-which(rownames(sldata) == "1"), ]

m <- ggplot(dat, aes(x = Dist, y = Cases))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Distance from Dhaka City (in Km)") + ylab("Total Dengue Cases")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
m

n <- ggplot(dat, aes(x = Dist, y = Deaths))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Distance from Dhaka City (in Km)") + ylab("Total Dengue Deaths")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
n



library(gridExtra)
tiff("CorrPlot.tiff", units="in", width=25, height=30, res=300)
gridExtra::grid.arrange(a,b, x, y, m, n, nrow=3, ncol=2)
dev.off()

#######GLMM with repeated measures

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')
dendat <- read.csv("DistrictWise.csv", header=T)  

South <- subset(dendat, Region == "South")

North <- subset(dendat, Region == "North")

Dhaka <- subset(dendat, Region == "Both")

mean(South$IR)
mean(North$IR)
mean(Dhaka$IR)

IRdiff <- t.test(South$IR, North$IR, var.equal =T)
IRdiff

mean(South$CFR)
mean(North$CFR)
mean(Dhaka$CFR)

CFRdiff <- t.test(South$CFR, North$CFR, var.equal =T)
CFRdiff



setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')
dendat <- read.csv("Model_Data.csv", header=T)  

South <- subset(dendat, Location == "Chittagong" | Location == "Khulna" 
                | Location == "Barisal")

North <- subset(dendat, Location == "Mymensing" | Location == "Rajshahi" 
                | Location == "Rangpur" | Location == "Sylhet")

Dhaka <- subset(dendat, Location == "Dhaka")

sum(South$Rainfall)/3
sum(North$Rainfall)/4
sum(Dhaka$Rainfall)

sum(dendat$Rainfall)

raindiff <- t.test(South$Rainfall, North$Rainfall, var.equal =T)
raindiff

mean(South$AvgTemp)
mean(North$AvgTemp)
mean(Dhaka$AvgTemp)

mean(dendat$AvgTemp)

raindiff <- t.test(South$AvgTemp, North$AvgTemp, var.equal =T)
raindiff

mean(South$RH)
mean(North$RH)
mean(Dhaka$RH)

raindiff <- t.test(South$RH, North$RH, var.equal =T)
raindiff

library(glmmTMB)
library(DHARMa)
library(performance)

setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')
dendat <- read.csv("Model_Data.csv", header=T)  

dendat$PDnum <- dendat$PD
dendat$URRnum <- dendat$URR*100
dendat$DFDnum <- dendat$DFD


fit <- glmmTMB(Cases ~ URRnum + PDnum +  DFDnum + AvgTemp + 
                 Rainfall + RH + (1|Location) , na.action=na.omit, 
               family = nbinom1(link = "log"), data = dendat)
library(car)
summary(fit)
rmse(fit)
exp(confint(fit))

options(scipen = 999)
performance::performance(fit)






























setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')
Dengue <- read.csv("DengueAndWeatherDataRough2.csv")

fmonthwise <- Dengue[which(Dengue$Year<='2010'), ]

agg = aggregate(fmonthwise$Rainfall, by = list(fmonthwise$Month), FUN = mean, na.omit = T)
agg


afmonthwise <- Dengue[which(Dengue$Year>='2011' & Dengue$Year<='2022'), ]

agg = aggregate(afmonthwise$Rainfall, by = list(afmonthwise$Month), FUN = mean, na.omit = T)
agg



setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue 2023')

library(ggplot2)

df <- data.frame(AgeGroup=c("0-10", "11-20", "21-30","31-40", "41-50", "51-60","61-70", "71-80", "80+"),
                 Count=c(32228, 60724, 86627, 58944, 38449, 25924, 13477, 3749, 1057))
head(df)

a<-ggplot(data=df, aes(x=AgeGroup, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=10)+
  xlab("Age Group") + ylab("Number of Cases") + 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
a

# pie Age
df <- data.frame(Sex=c("Female", "Male"),
                 Count=c(40.03, 59.97))
head(df)

m <- ggplot(df, aes(x = "", y = Count, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = Count),cex=10,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("green", "lightgreen")) +
  theme_void()+ 
  xlab("") + ylab("") + ggtitle("Dengue Cases (%)")+
  theme(plot.title = element_text(size = 30,hjust=0.5),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
m




library(ggplot2)

df <- data.frame(AgeGroup=c("0-10", "11-20", "21-30","31-40", "41-50", "51-60","61-70", "71-80", "80+"),
                 Count=c(124, 144, 273, 320, 272, 256, 205, 75, 36))
head(df)

b<-ggplot(data=df, aes(x=AgeGroup, y=Count)) +
  geom_bar(stat="identity", fill="blue")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=10)+
  xlab("Age Group") + ylab("Number of Deaths") + 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
b



# pie Age
df <- data.frame(Sex=c("Female", "Male"),
                 Count=c(56.89, 43.11))


n <- ggplot(df, aes(x = "", y = Count, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = Count),cex=10,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#0000FF", "#ADD8E6")) +
  theme_void()+ 
  xlab("") + ylab("") + ggtitle("Dengue Deaths (%)")+
  theme(plot.title = element_text(size = 30,hjust=0.5),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))

n



library(gridExtra)
tiff("AgePlots.tiff", units="in", width=20, height=20, res=300)
gridExtra::grid.arrange(a, b, nrow=2, ncol=1)
dev.off()

library(gridExtra)
tiff("SexPlots.tiff", units="in", width=20, height=10, res=300)
gridExtra::grid.arrange(m, n, nrow=1, ncol=2)
dev.off()

library(ggplot2)
# Pie
slbar <- sldata %>%
  group_by(HS) %>%
  summarise(count = n()) %>%
  group_by(HS) %>% mutate(per=round(count/1705*100,2))
slbar

df <- data.frame(HS=c("0", "1", "2","3", "4", "5","6", "7 or above"),
                 Days=c(18, 997,258,124,75,56,41,136))
head(df)

z<-ggplot(data=df, aes(x=HS, y=Days)) +
  geom_bar(stat="identity", fill="darkblue")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Days), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=10)+
  xlab("Hospital Stay (Days)") + ylab("Number of Deaths")  + 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30),
        legend.title = element_text(size=30),
        legend.text = element_text(size=30))
z


library(gridExtra)
tiff("Hosp.tiff", units="in", width=20, height=12, res=300)
gridExtra::grid.arrange(z, nrow=1, ncol=1)
dev.off()




