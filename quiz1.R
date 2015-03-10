## Question #1
library(manipulate)
myPlot <- function(s) {
        plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
        abline(0, s)
}

manipulate(myPlot, s = slider(0, 2, step = 0.1))  #a
manipulate(myPlot(s), slider = x(0, 2, step = 0.1))  #b
manipulate(myPlot(s), s = slider(0, 2, step = 0.1))  #c <--
manipulate(myPlot(s), x.s = slider(0, 2, step = 0.1))  #d


## Question #2
library(rCharts)
data("airquality")

dTable(airquality, sPaginationType = "full_numbers")  #a  <--
d <- data.frame(airquality, stringsAsFactors = FALSE) print(d)  #b
airquality  #c
head(airquality)  #d
