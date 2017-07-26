## cargo los datos y elimino los faltantes
unzip("activity.zip")
old <- Sys.getlocale()
Sys.setlocale("LC_TIME","English")

activity <- read.csv("activity.csv")
act_no_NA <- activity[!is.na(activity$steps),]

## agrupo por fecha y calculo total de pasos por dia
library(dplyr)
group_days <- group_by(act_no_NA, date)
sum_day <- summarize(group_days, steps = sum(steps))
## dibujo el histograma de pasos por dia. dibujo y calculo media y mediana
hist(sum_day$steps, col = "yellow", 
     main = "Histogram of total number steps taken per days",
     xlab = "Total Steps per day")

meanstep <- mean(sum_day$steps)
medianstep <- median(sum_day$steps)
abline(v=medianstep, col="green", lwd=2)
abline(v=meanstep, col="red", lty = 2)
legend("topright",lty=c(1,2), col = c("green","red"), 
       legend = c("Median", "Mean"))


## Promedio de pasos por intervalo
group_intervals <- group_by(act_no_NA, interval)
mean_interval <- summarize(group_intervals, steps = mean(steps))

with(mean_interval,
     xyplot(steps~interval, 
            main="Mean of step by interval", 
            type = "l", 
            col = "blue", 
            xlab = "Interval", 
            ylab = "Steps"
     )
)


plot(mean_interval$steps, type = "l", 
        col = "blue", xlab = "Interval", ylab = "Mean Step")

max_interval <- with(mean_interval, interval[which.max(steps)])


## Valores faltantes - completarlos
tot_NA <- sum(is.na(activity$steps))
## completo los faltantes con la media de ese intervalo para todos los dias
stepNA <- is.na(activity$steps)
activ_complete <- activity
for (i in which(stepNA)){
        activ_complete$steps[i] <- 
                mean_interval$steps[mean_interval$interval==activ_complete$interval[i]]
}


## hago el mismo histograma pero con los datos faltantes completados
group_days <- group_by(activ_complete, date)
sum_day <- summarize(group_days, steps = sum(steps))
hist(sum_day$steps, col = "yellow", 
     main = "Histogram of total number steps taken per days",
     sub = "imputing missing value",
     xlab = "Total Steps per day")
meanstepnoNA <- mean(sum_day$steps)
medianstepnoNA <- median(sum_day$steps)
abline(v=medianstepnoNA, col="green", lwd=2)
abline(v=meanstepnoNA, col="red", lty = 2)
legend("topright",lty=c(1,2), col = c("green","red"), 
       legend = c("Median", "Mean"))




## hay diferencias en los patrones de actividad entre los dias de semana y los finde?
vweek = c(rep("weekday",5),rep("weekend",2))
names(vweek) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
activ_complete$week <- as.factor(vweek[weekdays(as.Date(activ_complete$date),abbreviate=TRUE)])

library(plyr)
col_group <- c("week","interval")
mean_intw <- ddply(activ_complete, col_group, summarize, steps=mean(steps))

## con lattice
library(lattice)
with(mean_intw,
     xyplot(steps~interval|week, 
            main="Weekdays vs Weekends", 
            type = "l", 
            col = "blue", 
            xlab = "Interval", 
            ylab = "Steps",
            layout = c(1,2)
            )
     )

## con base
par(mfrow=c(1,2))
with(mean_intw,plot(steps, main="Weekdays", type = "l", col = "blue", xlab = "Interval", ylab = "Steps"))
with(mean_intw,plot(steps, main="Weekends", type = "l", col = "blue", xlab = "Interval", ylab = "Steps"))


Sys.setlocale(locale = old)




## Otra forma para hacerlo podria ser con tapply, 
## tuve el problema de que cuando hago as.factor(act_no_NA$date) 
## me trae fechas que no estan como 2012-10-01 y luego en el tapply quedan NA
## con tapply
sum_day <- tapply(act_no_NA$steps, as.factor(act_no_NA$date), sum)
## dibujo el histograma de pasos por dia. dibujo y calculo media y mediana
hist(sum_day, col = "yellow", 
     main = "Histogram of total number steps taken per days",
     xlab = "Total Steps per day")

meanstep <- mean(sum_day)
medianstep <- median(sum_day)
abline(v=medianstep, col="green", lwd=2)
abline(v=meanstep, col="red", lty = 2)
legend("topright",lty=c(1,2), col = c("green","red"), 
       legend = c("Median", "Mean"))

## con tapply
mean_interval <-  tapply(act_no_NA, as.factor(act_no_NA$interval), mean)
plot(mean_interval, type = "l", 
     col = "blue", xlab = "Interval", ylab = "Mean Step")

