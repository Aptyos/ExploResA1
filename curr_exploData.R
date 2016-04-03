##############################################################################################################
# --Code for reading in the dataset and/or processing the data                                               
# --Histogram of the total number of steps taken each day                                                    
# --Mean and median number of steps taken each day <= affichage ???                                                          
# --Time series plot of the average number of steps taken                                                    
# --The 5-minute interval that, on average, contains the maximum number of steps                             
# --Code to describe and show a strategy for imputing missing data                                           
# --Histogram of the total number of steps taken each day after missing values are imputed                   
# --Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
##############################################################################################################
library(dplyr)
library(ggplot2)
library(lattice)
library(stringr)
library(reshape2)
library(markdown)

# set local time
Sys.setlocale(category = "LC_TIME", locale = "English_United States.1252")

### # download dataset [  A DECOMMENTER ]
### v_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
### temp <- tempfile()
### download.file(v_url,temp)
### oriData <- read.csv(unz(temp, "activity.csv"))
### unlink(temp)   
oriData <- read.csv("activity.csv", stringsAsFactors =FALSE )
oriData <- data.frame(curr_dt=as.Date(oriData$date,"%Y-%m-%d"), interval=str_pad(oriData$interval,4,"left","0"), steps= as.numeric(oriData$steps), stringsAsFactors =FALSE )

## ==================================================================
## results without missing values                                     
## ==================================================================

stepsNoNas <- subset(oriData, steps >= 0  )
stepsNoNas <-
  stepsNoNas %>%
  group_by(curr_dt) %>%
  summarize(total_steps = sum(steps, na.rm =TRUE), v_mean = mean(steps, na.rm =TRUE), v_med=median(steps, na.rm =TRUE), na_steps=sum(is.na(steps)) )

noNasMean   <- round(mean(stepsNoNas$total_steps),2)
noNasMedian <- round(median(stepsNoNas$total_steps),2)

## Plotting
hist( stepsNoNas$total_steps, col="steelblue", breaks = 20,  xlab = "", main="",  ylim=c(0,20) )
abline(v=noNasMean,  col="magenta",lwd=3)
abline(v=noNasMedian, col="green", lwd=2)
legend("topright", y.intersp=1.2, lty=1, col = c("green","magenta"), cex =0.8, legend = c(paste(" Median =", noNasMedian), paste("Average =", noNasMean) ) ) 
title(main="Steps Distribution ( excluding missing values)", xlab = "Total Daily Steps")
dev.copy(png, file = "plot1.png")
dev.off()

## displaying results
#viewRes <- data.frame(stepsDaily$curr_dt, stepsDaily$total_steps, stepsDaily$v_mean, stepsDaily$v_med)
#View(viewRes)

# =====================================================================================
# Time series plot of the average number of steps taken each day
# =====================================================================================
meltSteps <- melt(oriData, id=c("interval"), measure.vars=c("steps"))
meltSteps <- meltSteps [ complete.cases(meltSteps),]
castSteps <- dcast(data = meltSteps, interval ~variable, mean)

# convert the interval column to time format ( date will be the current date)
castSteps <- data.frame(Time = strptime(castSteps$interval,format("%H%M")), steps=castSteps$steps)
# max values to be displayed on plot
maxStepsCoord <- castSteps [ grep( max(castSteps$steps), castSteps$steps),]
endIntvTime <- sub(Sys.Date(),"",maxStepsCoord[1,1])
begIntvTime <- sub(Sys.Date(),"",maxStepsCoord[1,1] - 300)
maxPlotSteps <- round(maxStepsCoord[1,2], 2)

# plotting
plot(castSteps$Time, castSteps$steps,  type="l", xlab ="Time", ylab="Average Steps", lab =c(2, 10, 7) , ylim =c(0, maxPlotSteps +20) )
abline(h= maxStepsCoord[2], col="magenta", lty =2)
abline(v= maxStepsCoord[1], col="magenta", lty =2)
legend("topright", y.intersp=1.2, cex =0.8, legend = c(paste("Max Time Interval = ", paste(begIntvTime,endIntvTime, sep = " - ")), paste("Max Average Steps = ", maxPlotSteps) ) )
dev.copy(png , width=600, height = 480, "plot2.png")
dev.off()

# =================================================================================
#  inputing missing data ==> replacing NA values with previously calculated average
# =================================================================================
# prepare the new dataset with not null steps
newData <- subset(oriData, steps >= 0  )

stepsDaily <-
  oriData %>%
  group_by(curr_dt) %>%
  summarize(total_steps = sum(steps, na.rm =TRUE), v_mean = mean(steps, na.rm =TRUE), v_med=median(steps, na.rm =TRUE), na_steps=sum(is.na(steps)) )

# targeted rows where to modify NA steps values
missedDaily <- subset( stepsDaily, na_steps > 0)

# function to get mean according to date
# -- replace NA's by the average of the current daily steps
# -- whenever no mean is provided for a particular date the average of all others days averages is returned  
getMissMean <- function ( vdate) {
  allMean <- summary( stepsDaily$v_mean)[4]
  getRow <- missedDaily [ missedDaily$curr_dt == vdate,]
  ifelse( getRow$na_steps == 288, allMean, getRow$v_mean )
}

## generating the new datatset with NAs-steps values as the mean from the initial dataset
for ( vCurrDt in missedDaily$curr_dt ) {
  missed_steps <- subset(oriData, is.na(steps) & curr_dt == vCurrDt )
  newSteps <-
    missed_steps %>%
    mutate(steps = getMissMean(vCurrDt))
  newData <- rbind(newData, newSteps)
}

stepsNewDaily <-
  newData %>%
  group_by(curr_dt) %>%
  summarize(total_steps = sum(steps, na.rm =TRUE), v_mean = mean(steps, na.rm =TRUE), v_med=median(steps, na.rm =TRUE), na_steps=100*mean(is.na(steps)) )

# add unique index column ( date and minutes concatenation) and merge
oriData <- cbind(new_dt=strptime(paste(oriData$curr_dt, oriData$interval),format("%Y-%m-%d %H%M")), oriData)
newData <- cbind(new_dt=strptime(paste(newData$curr_dt, newData$interval),format("%Y-%m-%d %H%M")), newData)
mrg_data <- merge(oriData, newData, by.x = "new_dt", by.y = "new_dt")
mrg_data <- select (mrg_data, -(curr_dt.y:interval.y)) 

# ======================================================================================
# Histogram of the total number of steps taken each day after missing values are inputed 
# ======================================================================================
newData <-                                                            
  mrg_data %>%                                                          
  group_by(curr_dt.x) %>%                                               
  summarise( old_sum = sum(steps.x, na.rm=TRUE), new_sum = sum(steps.y), v_mean = mean(steps.y), v_med=median(steps.y))

# calculated Mean and Median
corrMean   <- round(mean(newData$new_sum),2)
corrMedian <- round(median(newData$new_sum),2)

# histogram plotting
hist( newData$new_sum, col="steelblue", breaks = 20,  xlab = "", main="", ylim=c(0,20))
abline(v=corrMean,   col="magenta",lwd=3)
abline(v=corrMedian, col="green", lwd=2)
legend("topright", y.intersp=1.2,lty=1, col = c("green","magenta"),  cex =0.8, legend = c(paste(" Median =", corrMedian), paste("Average =", corrMean) ) ) 
title(main="Steps Distribution ( with correction)", xlab = "Total Daily Steps")
dev.copy(png, file = "plot3.png")
dev.off()

# =========================================================================================================
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# =========================================================================================================
mrg_data <- data.frame( new_dt=mrg_data$new_dt, interval=mrg_data$interval.x, steps=mrg_data$steps.y)

weekData <- 
   mrg_data %>%
   mutate( dayOfWeek = weekdays(new_dt) ) %>%
   mutate( weekCat = ifelse(dayOfWeek %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),"weekday","weekend")) %>%
   mutate( interval = as.numeric(interval)*5)%>%
   select( - new_dt) %>%
   group_by(weekCat, interval ) %>%
   summarize(average = mean(steps))

weekPlot <- xyplot(average ~ interval | weekCat, weekData, type="l", xlab ="Time Interval (min)", layout=c(1, 2))
png("plot4.png")
print(weekPlot)
dev.off()
