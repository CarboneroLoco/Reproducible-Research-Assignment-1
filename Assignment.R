getwd()
setwd("C:\\Users\\jooo\\Desktop\\Coursera\\Coursera Data Scientist\\5. Reproducible research\\Assignment 1")

#Chequeo si en la carpeta existe ese archivo, sino existe, entonces lo descargo de la web y lo unzipeo
if(!file.exists("activity.csv")) {
        tempfile <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = tempfile)
        unzip(tempfile)
        unlink(tempfile)
}
#cargo la data
activity <- read.csv("activity.csv")

summary(activity)
str(activity)

# 1.
activity_steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)

# 2.
hist(activity_steps_day$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "wheat")

# 3.
mean_steps <- mean(activity_steps_day$steps)
median_steps <- median(activity_steps_day$steps)
#we set a normal number format to display the results
mean_steps <- format(mean_steps,digits=1)
median_steps <- format(median_steps,digits=1)

# Grafica de serie de tiempo
activity_steps_mean <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
#Plot
plot(activity_steps_mean$interval, activity_steps_mean$steps, type = "l", col = "tan3", xlab = "Intervals", ylab = "Total steps per interval", main = "Number of steps per interval (averaged) (NA removed)")                


# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps <-max(activity_steps_mean$steps)
#Que intervalos tienen el numero de pasos mas altos?
max_interval <- activity_steps_mean$interval[which(activity_steps_mean$steps == max_steps)]
max_steps <- round(max_steps, digits = 2)


#Calculando la cantidad de valores faltantes
sum(is.na(activity))


#2. Devise a strategy for filling in all of the missing values in the dataset
#Toma la muestra de la data original que incluya solo los NA
missing_values <- subset(activity, is.na(steps))
#plot repartition, by date or by intervals
par(mfrow = c(2,1), mar = c(2, 2, 1, 1))
hist(missing_values$interval, main="NAs repartition per interval")
hist(as.Date(missing_values$date), main = "NAs repartion per date", breaks = 61)

# 3. Create new dataset with the missing data filled in
# Calculo la cantidad promedio de pasos por intervalo
MeanStepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# Corta el dataset "activity" en 2 partes (con y sin NA)
activity_NAs <- activity[is.na(activity$steps),]
activity_non_NAs <- activity[!is.na(activity$steps),]
#Reemplaza los valores faltantes
activity_NAs$steps <- as.factor(activity_NAs$interval)
levels(activity_NAs$steps) <- MeanStepsPerInterval
#Vuelve al vector a su forma de numero entero 
levels(activity_NAs$steps) <- round(as.numeric(levels(activity_NAs$steps)))
activity_NAs$steps <- as.integer(as.vector(activity_NAs$steps))
#Combina los 2 datasets juntos
imputed_activity <- rbind(activity_NAs, activity_non_NAs)


# 4. Make a histogram of the total number of steps taken each day
#Grafico parametro para juntar los hist anteriores junto con este
par(mfrow = c(1,2))
#Vuelvo a graficar el histograma inicial
activity_steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
hist(activity_steps_day$steps, xlab = "Steps per Day", main = "NAs REMOVED - Total steps/day", col = "wheat")
#Creo el nuevo histograma con los NA
imp_activity_steps_day <- aggregate(steps ~ date, data = imputed_activity, FUN = sum, na.rm = TRUE)
hist(imp_activity_steps_day$steps, xlab = "Steps per Day", main = "NAs IMPUTED - Total steps/day", col = "wheat")


imp_mean_steps <- mean(imp_activity_steps_day$steps)
imp_median_steps <- median(imp_activity_steps_day$steps)
#Defino un formato normal de numero donde se muestren los resultados
imp_mean_steps <- format(imp_mean_steps,digits=1)
imp_median_steps <- format(imp_median_steps,digits=1)
#Almaceno los resultados en un dataset
results_mean_median <- data.frame(c(mean_steps, median_steps), c(imp_mean_steps, imp_median_steps))
colnames(results_mean_median) <- c("NA removed", "Imputed NA values")
rownames(results_mean_median) <- c("mean", "median")

install.packages("xtable")
library(xtable)

xt <- xtable(results_mean_median)
print(xt, type="html")



#Are there differences in activity patterns between weekdays and weekends?
#1. Crear una nueva varible factor
imputed_activity$dayType <- ifelse(weekdays(as.Date(imputed_activity$date)) == "Samstag" | weekdays(as.Date(imputed_activity$date)) == "Sonntag", "weekend", "weekday")
imputed_activity$dayType <- factor(imputed_activity$dayType)

#  2. Panel plot containing time series plot
steps_interval_dayType <- aggregate(steps ~ interval + dayType, data = imputed_activity, FUN = mean)
head(steps_interval_dayType)


#add descriptive variables
names(steps_interval_dayType) <- c("interval", "day_type", "mean_steps")
#plot with ggplot2
library(ggplot2)
plot <- ggplot(steps_interval_dayType, aes(interval, mean_steps))
plot + geom_line(color = "tan3") + facet_grid(day_type~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")