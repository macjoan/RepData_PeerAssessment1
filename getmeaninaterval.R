###Function that get the main for  the NA value in the steps for
### the dataset of activity in the project  for the week 2
### in the reprodusible researching 

##The funcytion need an interval value from the activity dataframe and the dataframe
##with the average daily activity

getmeaninaterval <- function(interval, stepsperInterval) {
  getmean <- stepsperInterval[stepsperInterval$interval==interval,]$steps
  getmean
}