library("ggplot2")
library("ggthemes")
library("plyr")
library("car")
library("data.table")
library("leaps")
library("olsrr")
library(usdm)
library("lars")
setwd("C:/Users/Administrator/Desktop/KYJ/CDU/R")

Lab_data = fread("CDU2_LAB_KERO10.csv", header=T, data.table=F, stringsAsFactors = T)
RTDB = fread("CDU2_RTDB.csv", header=T, data.table=F, stringsAsFactors = T)
tail(RTDB[,c(length(colnames(RTDB))-5:length(colnames(RTDB)))], 5)

subset(Lab_data, select=-CDU2KERO10)
colnames(Lab_data)[1] <- 'date'
colnames(RTDB)[1] <- 'date'

# Sorting y_ by date
Lab_data <- arrange(Lab_data, date)

y_date = Lab_data$date
y_ = Lab_data$CDU2KERO10

# Lab 데이터의 산점도 확인 - 패턴을 확인하기 위함
ggplot(Lab_data, aes(x=as.POSIXct(y_date), y=CDU2KERO10, group=1)) + 
  geom_line(colour = "orange1", size=1) + 
  #scale_x_date(date_labels = "%b ") +
  geom_point(colour = "orangered2", size=1) + theme_light()

# Lab data(CDU2KERO 10) 이상치 확인########################################################
boxplot(Lab_data$CDU2KERO10)$out
y_IQR <- IQR(Lab_data$CDU2KERO10, na.rm = T)
posIQR <- quantile(Lab_data$CDU2KERO10, probs = c(0.25, 0.75), na.rm=T)
posIQR
############################################################################################

# TA구간 확인###############################################################################
start_time <- y_date[-length(y_date)]
end_time <- y_date[-1]
time_diff <- difftime(end_time, start_time,units = "days")
plot(time_diff)
TA_df <- data.frame(start_time, end_time,time_diff)
TA <- subset(TA_df, subset=(time_diff > 7))         # TA 구간 7 이상 추출

    # TA구간의 data gap 확인
    TA_idx = as.numeric(rownames(Lab_data[Lab_data$date %in% TA$start_time,]))
    TA_interval <- data.frame()
    for(i in 1:length(TA_idx)){
      temp <- Lab_data[(TA_idx[i] - 2) : (TA_idx[i] + 3),]
      TA_interval <- rbind(TA_interval, temp)
    }
    TA_interval <- TA_interval[complete.cases(TA_interval), ]
    year <- car::recode(format(as.POSIXct(TA_interval$date), format="%Y"), 
                        "'2014'=1; '2015'=2; '2016'=3")
    ggplot(TA_interval, aes(x=as.POSIXct(TA_interval$date), y=CDU2KERO10, group=1 )) + 
      geom_line(colour = year, size=1) + 
      geom_point(colour = year, size=2) + theme_light() +
      xlab("TA interval +- 2")
    
    TA_interval
##########################################################################################
    rm(year, y_IQR, time_diff, TA_idx, start_time, posIQR, i, end_time, temp, TA_df, TA, TA_interval)
    
    
# Basic EDA #############################################################################
    
# Remove delete columns that contain ONLY NAs
EDA_RTDB <- RTDB[, colSums(!is.na(RTDB))>0]
    
# Change time interval
start_date <- as.POSIXct("2014-01-01")
end_date <- as.POSIXct("2015-12-31")
EDA_Lab_data <- subset(Lab_data, subset=((as.POSIXct(date)>start_date) & (as.POSIXct(date) < end_date)))
EDA_RTDB <- subset(EDA_RTDB, subset=((as.POSIXct(date)>start_date) & (as.POSIXct(date) < end_date)))

# Remove na row in RTDB
EDA_RTDB <- EDA_RTDB[complete.cases(EDA_RTDB), ]

# Removed constant columns in RTDB
temp_date <- subset(EDA_RTDB, select=(date))
temp <- subset(EDA_RTDB, select=c(-date))
temp <- subset(temp, select=(apply(temp, 2, var, na.rm=TRUE) != 0))
EDA_RTDB <- cbind(temp_date, temp)

# Add 30 minutes unit date 
unit_30 = format(as.POSIXct(na.omit(Lab_data[format(as.POSIXct(y_date), "%M") == "30",])$date), 
       "%Y-%m-%d %H")

unit_30 <- paste (unit_30, "00", sep = ":")
unit_30_idx <- as.numeric(rownames(EDA_RTDB[EDA_RTDB$date %in% unit_30,]))

before_30_date <- EDA_RTDB[unit_30_idx,]["date"]
before_30_data <- subset(EDA_RTDB[unit_30_idx,], select=c(-date))
after_30_date <- EDA_RTDB[unit_30_idx+1,]["date"]
after_30_data <- subset(EDA_RTDB[unit_30_idx +1,], select=c(-date))

before_30_date["date"] <- as.factor(format((as.POSIXct(before_30_date[,1])-60*30),"%Y-%m-%d %H:%M"))
mean_30 <- (after_30_data+before_30_data)/2
diff_30 <- data.frame(before_30_date, mean_30)

EDA_RTDB <- rbind(EDA_RTDB, diff_30)

rm(temp, temp_date,after_30_data, after_30_date, before_30_data, before_30_date, diff_30, mean_30, unit_30, unit_30_idx, end_date, start_date)

EDA_RTDB <- merge(EDA_Lab_data, EDA_RTDB, by="date")

# Basic EDA 후의 plot
ggplot(EDA_RTDB, aes(x=as.POSIXct(date), y=CDU2KERO10, group=1)) + 
  geom_line(colour = "orange1", size=1) +
  geom_point(colour = "orangered2", size=1) + theme_light()

boxplot(EDA_RTDB$CDU2KERO10) # 

#############################################################################################

# TA 구간의 값이 급격히 변했던 요인을 찾고 싶음.
# TA 구간동안 급격히 변했던 x 변수를 찾는다.
# 먼저 correlation으로 걸러준다.

plot(subset(EDA_RTDB[,c(1,2,20:30)], select=-c(date)))
a <- cor(subset(EDA_RTDB, select=c(CDU2KERO10)),subset(EDA_RTDB, select=-c(date, CDU2KERO10))) > 0.3

corrleation <- cor(subset(EDA_RTDB, select=c(CDU2KERO10)),subset(EDA_RTDB, select=-c(date, CDU2KERO10)))
corrleation
a <- abs(corrleation) > 0.5;a
corrleation[,colSums(is.na(a))==0]
sel_cor <- (corrleation[,a])[complete.cases(corrleation[,a])]
sel_cor <- names(sel_cor)
cor_idx = as.numeric(rownames(EDA_RTDB[names(EDA_RTDB) %in% sel_cor,]))
EDA_RTDB <- subset(EDA_RTDB, select=c(date,CDU2KERO10,cor_idx))
boxplot(EDA_RTDB$CDU2KERO10)
plot(as.POSIXct(EDA_RTDB$date), EDA_RTDB$CDU2KERO10)
boxplot(subset(EDA_RTDB, select=-c(date,CDU2KERO10, CDU2FC585_SP,CDU2FC585, CDU2FC592B)))

sel_EDA <- subset(EDA_RTDB, select=-c(date))

model <- lm(CDU2KERO10 ~.,data = sel_EDA)
k <- ols_best_subset(model)
plot(k)

vif(subset(sel_EDA, select=-c(CDU2KERO10)))
a <-lars(as.matrix(sel_EDA[, -which(names(sel_EDA) == "CDU2KERO10")]) ,sel_EDA$CDU2KERO10)
cor_b <- cor(sel_EDA);cor_b
b<-cor_b>0.5;b