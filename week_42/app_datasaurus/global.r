library(shiny)
library(ggplot2)
library(ggExtra)
library(tidyr)

datasaurus <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')
list_dataset <- unique(datasaurus$dataset)
list_dataset <- list_dataset[order(list_dataset)]
posit_param <- list("Minimum (min)" = "min", "Maximum (max)" = "max","Median (med)" = "med", "Mean" = "mean", "Mode" = "mode")
disp_param <- list("Range" = "range", "Interquartile range (IQR)" = "IQR","Standard deviation (SD)" = "SD", "Mean absolute difference (MADi)" = "MADi","Median absolute deviation (MADe)" = "MADe")


get_mode <- function(x){
	x_rounded <- data.frame(x = x,rounded = ceiling(x/5)*5)
	count_x <- aggregate(x ~ rounded,data = x_rounded, FUN = length)
	mode <- count_x$rounded[which(count_x$x == max(count_x$x))]
	return(mode[1])
}
get_rge <- function(x){
	rge <- max(x) - min(x)
	return(rge)
}
get_madi <- function(x){
	adi <- abs(x-mean(x))
	madi <- mean(adi)
	return(madi)
}
get_made <- function(x){
	ade <- abs(x-mean(x))
	made <- median(ade)
	return(made)
}


max_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "max")
max_x$type <- "max" 
min_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "min")
min_x$type <- "min"
median_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "median")
median_x$type <- "med"
mean_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "mean")
mean_x$type <- "mean"
mode_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "get_mode")
mode_x$type <- "mode"
max_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "max")
max_y$type <- "max" 
min_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "min")
min_y$type <- "min"
median_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "median")
median_y$type <- "med"
mean_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "mean")
mean_y$type <- "mean"
mode_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "get_mode")
mode_y$type <- "mode"

posit_x <- rbind(max_x,min_x,median_x,mean_x,mode_x)
colnames(posit_x) <- gsub("^x$","value",colnames(posit_x))
posit_x$param <- "x"
posit_y <- rbind(max_y,min_y,median_y,mean_y,mode_y)
colnames(posit_y) <- gsub("^y$","value",colnames(posit_y))
posit_y$param <- "y"
posi_table <- rbind(posit_x,posit_y)
posi_table$value <- round(posi_table$value, 1)

Rge_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "get_rge")
Rge_x$type <- "range"
IQR_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "IQR")
IQR_x$type <- "IQR"
SD_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "sd")
SD_x$type <- "SD"
MADi_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "get_madi")
MADi_x$type <- "MADi"
MADe_x <- aggregate(x ~ dataset, data = datasaurus, FUN = "get_made")
MADe_x$type <- "MADe"
Rge_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "get_rge")
Rge_y$type <- "range"
IQR_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "IQR")
IQR_y$type <- "IQR"
SD_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "sd")
SD_y$type <- "SD"
MADi_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "get_madi")
MADi_y$type <- "MADi"
MADe_y <- aggregate(y ~ dataset, data = datasaurus, FUN = "get_made")
MADe_y$type <- "MADe"

disp_x <- rbind(Rge_x, IQR_x, SD_x, MADi_x, MADe_x)
colnames(disp_x) <- gsub("^x$","value",colnames(disp_x))
disp_x$param <- "x"
disp_y <- rbind(Rge_y, IQR_y, SD_y, MADi_y, MADe_y)
colnames(disp_y) <- gsub("^y$","value",colnames(disp_y))
disp_y$param <- "y"
disp_table <- rbind(disp_x, disp_y)
disp_table$value <- round(disp_table$value, 1)