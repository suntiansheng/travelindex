#utf-8
rm(list = ls())
gc()
setwd("F:/spssmoneny")


library(dplyr)
travel <- read.csv(file = 'hunan.csv',na.strings = " ")
str(travel)


########num1#######

#######1.1#####
travel_1 <-  travel[travel$year<=1988,]

x_1_mean <- mean(travel_1$tohunan)
x_1_t <- travel_1$tohunan  - x_1_mean
myfun <- function(x){((x^2-x_1_mean)^2)/11}
#myfun(x_t[1])
#dim(x_t)
x_1_t <- as.matrix(x_1_t)
sqrt(sum(apply(x_1_t , 1 ,myfun)))/x_1_mean

#######1.2######

travel_2 <-  travel[travel$year>1988&travel$year<=2002,]

x_2_mean <- mean(travel_2$tohunan)
x_2_t <- travel_2$tohunan  - x_2_mean
myfun <- function(x){((x^2-x_2_mean))/14}
x_2_t <- as.matrix(x_2_t)
sqrt(sum(apply(x_2_t , 1 ,myfun)))/x_2_mean

#######1.3######

travel_3 <-  travel[travel$year>2002&travel$year<=2007,]

x_3_mean <- mean(travel_3$tohunan)
x_3_t <- travel_3$tohunan  - x_3_mean

myfun <- function(x){((x-x_3_mean)^2)/5}
x_3_t <- as.matrix(x_3_t)
sqrt(sum(apply(x_3_t , 1 ,myfun)))/x_3_mean


#######1.4######

travel_4 <-  travel[travel$year>2007,]

x_4_mean <- mean(travel_4$tohunan)
x_4_t <- travel_4$tohunan  - x_4_mean
myfun <- function(x){((x-x_4_mean)^2)/8}
x_4_t <- as.matrix(x_4_t)
sqrt(sum(apply(x_4_t , 1 ,myfun)))/x_4_mean


########num2#########

people <- read.csv(file = "people.csv")
TOTAL <- people$total[1]
myfun <- function(x){(x/TOTAL)^2}
sqrt(sum(apply(people[1,-c(1,2)],2,myfun))) *100

for(i in 1990:2003){
  vec <- people[people$year == i,]
  TOTAL <- vec$total
  rel <- sqrt(sum(apply(vec[1,-c(1,2)],2,myfun))) *100
  print(i)
  print(round(rel,2))
}

round(100*sqrt(1/ncol(vec[1,-c(1,2)])),2)


######num3######
####层次聚类
people_1 <- people[,-c(1,2)]
people_2 <- apply(people_1,2,sum)
people_standard<- scale(people_2,center = T,scale = T)
dist_p <- dist(people_standard,method = "euclidean")
plot(hclust(dist_p))
