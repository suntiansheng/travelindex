#utf-8
rm(list = ls())
gc()
setwd("F:/spssmoneny")


library(dplyr)
travel <- read.csv(file = 'hunan.csv',na.strings = " ")
str(travel)
###为了第一问的travel
travel <- read.csv(file = 'for_T_index.csv',na.strings = ' ')

########num1#######

#######1.1#####
travel_1 <-  travel[travel$年份<=2002,]

x_1_mean <- mean(travel_1$入境旅游人数)
x_1_t <- travel_1$入境旅游人数  - x_1_mean
myfun <- function(x){((x^2-x_1_mean)^2)/25}
#myfun(x_t[1])
#dim(x_t)
x_1_t <- as.matrix(x_1_t)
sqrt(sum(apply(x_1_t , 1 ,myfun)))/x_1_mean

#######1.2######

travel_2 <-  travel[travel$年份>2002&travel$入境旅游人数<=2010,]

x_2_mean <- mean(travel_2$入境旅游人数)
x_2_t <- travel_2$入境旅游人数  - x_2_mean
myfun <- function(x){((x^2-x_2_mean))/13}
x_2_t <- as.matrix(x_2_t)
sqrt(sum(apply(x_2_t , 1 ,myfun)))/x_2_mean

#######1.3######

travel_3 <-  travel[travel$年份>2010,]

x_3_mean <- mean(travel_3$入境旅游人数)
x_3_t <- travel_3$入境旅游人数  - x_3_mean

myfun <- function(x){((x-x_3_mean)^2)/5}
x_3_t <- as.matrix(x_3_t)
sqrt(sum(apply(x_3_t , 1 ,myfun)))/x_3_mean




########num2#########

people <- read.csv(file = "people.csv")
TOTAL <- people$total[1]
myfun <- function(x){(x/TOTAL)^2}
sqrt(sum(apply(people[1,-c(1,2)],2,myfun))) *100

for(i in 1990:2015){
  vec <- people[people$year == i,]
  TOTAL <- vec$total
  rel <- sqrt(sum(apply(vec[1,-c(1,2)],2,myfun))) *100
  print(i)
  print(round(rel,2))
}

round(100*sqrt(1/ncol(vec[1,-c(1,2)])),2)


######num3######
####层次聚类
#########total
people_1 <- people[,-c(1,2)]
people_2 <- apply(people_1,2,sum)
people_standard<- scale(people_2,center = T,scale = T)
dist_p <- dist(people_standard,method = "euclidean")
colnames(dist_p)
plot(hclust(dist_p))

##########<2002
people_2002 <- people[people$year<2002,-c(1,2)]
people_2002_2 <- apply(people_2002,2,sum)
people_2002_standard <- scale(people_2002_2,center = T,scale = T)
dist_2002 <- dist(people_2002_standard,method = "euclidean")
plot(hclust(dist_2002))

#######2003 - 2010
people_2002 <- people[people$year>=2002&people$year<=2010,-c(1,2)]
people_2002_2 <- apply(people_2002,2,sum)
people_2002_standard <- scale(people_2002_2,center = T,scale = T)
dist_2002 <- dist(people_2002_standard,method = "euclidean")
plot(hclust(dist_2002))
#######>=2011

people_2002 <- people[people$year>=2011,-c(1,2)]
people_2002_2 <- apply(people_2002,2,sum)
people_2002_standard <- scale(people_2002_2,center = T,scale = T)
dist_2002 <- dist(people_2002_standard,method = "euclidean")
plot(hclust(dist_2002))


######num4#######
#quanguo <- read.csv(file = 'travel.csv',na.strings = " ")
m <- travel$tohunan/travel$towhole * 100
write.csv(m , file = 'index_b.csv')


########num5######
b <- travel$tohunan/travel$towhole
a <- travel$incomehunan/travel$income_whole
Q <- a/b 
write.csv(Q , file = 'index_Q.csv')


#######num6#######

library(DMwR)
R_data <- read.csv(file = 'for_R_index.csv',na.strings = ' ')
R_data[,-1] <- knnImputation(R_data[,-1])

##province
A <- round(R_data$湖南省入境/R_data$湖南省省内 * 100 , 2)
if(file.exists('R_index_province.csv')){file.remove('R_index_province.csv')} 
write.csv(A , file = 'R_index_province.csv')
####country
B <- round(R_data$全国入境/R_data$全国国内 * 100 , 2)
if(file.exists('R_index_country.csv')){file.remove('R_index_country.csv')}
write.csv(B , file = 'R_index_country.csv')

#####R index
R_index <- round(A/B , 2)
if(file.exists('R_index.csv.csv')){file.remove('R_index.csv.csv')}
write.csv(R_index , file = 'R_index.csv')

#####绘图5.4
huitu5_4 <- read.csv(file = '绘图5_4.csv')
plot(huitu5_4[,-1])
text(huitu5_4[,-1],labels = huitu5_4$年份,pos = 4,cex = 0.7)
abline(h = 1.2, v = 0.09, col = "gray60")

###########num7##########
library(DMwR)
province <- read.csv(file = 'province.csv',na.strings = ' ')
province[,-1]  <- knnImputation(province[,-1])
B_province <- province[,-1]/province$全省 *100
write.csv(B_province , file = 'B_province.csv')

#########num8######
province <- read.csv(file = 'province.csv',na.strings = ' ')
province[,-1]  <- knnImputation(province[,-1])
provinceincome <- read.csv(file = 'provinceincome.csv' , na.strings = ' ')
provinceincome[,-1] <- knnImputation(provinceincome[,-1])
province_1 <- province[province$年份>=2005,]

A <- provinceincome[,-1] / provinceincome$全省
B <- province_1[,-1] / province_1$全省
Q <- round(A/B , 2)
Q <- Q[,-1]
write.csv(Q, file = 'each_city_Q_index.csv')


#######num9#######
