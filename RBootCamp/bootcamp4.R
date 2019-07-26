library(magrittr)
library(ggplot2)
library(dplyr)

install.packages(RColorBrewer)


Diamonds <- read.csv("data/Diamonds.csv")
Diamonds$cut <- factor(Diamonds$cut, levels=c('Fair','Good','Very Good','Premium','Ideal'), 
                       ordered=TRUE)

Diamonds$color <- factor(Diamonds$color, levels=c('J', 'I', 'H', 'G', 'F', 'E', 'D'),
                                                 ordered=TRUE)
Diamonds$clarity <- factor(Diamonds$clarity, levels=c('I1', 'SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF'),
                           ordered=TRUE)
Diamonds$color %>% table()
freq <- Diamonds$color %>% table() %>% prop.table() *100
freq
barplot(freq[order(freq, decreasing=TRUE)], main="Diamond Cut Quality - Percentage", ylab="Per cent", xlab="Color",
        col="deepskyblue")

color_cut <- table(Diamonds$color, Diamonds$cut) %>% prop.table(margin = 2) %>% round(3)
barplot(color_cut, main = "Diamond Color by Cut Quality", ylab="Proportion within Cut", xlab="Cut", 
        beside=TRUE, legend=rownames(color_cut), args.legend=c(x="top", horiz=TRUE, title="Color"),
        ylim = c(0,.30), col=brewer.pal(7, name = "RdBu"))
grid()

set.seed(4532)
samp <- sample_n(Diamonds, 30)
qplot(data = samp, x = carat, geom = "dotplot", binwidth = .1)
hist(Diamonds$z, xlab="Diamond Depth (mm)", main="Histogram of Diamond Depths (mm)", col = "dodgerblue3")
summary(Diamonds$z)
Diamonds_clean <- filter(Diamonds, z < 6 & z > 2)
hist(Diamonds_clean$z, xlab="Diamond Depth (mm)", main="Histogram of Diamong Depths (mm)", col = "dodgerblue3")

library(lattice)
Diamonds_clean %>% histogram(~ z|cut,col="dodgerblue3",
                             layout=c(1,5), data=.,xlab="Depth (mm)")

range(Diamonds$carat)
var(Diamonds$carat)
sd(Diamonds$carat)
quantile(Diamonds$carat)
IQR(Diamonds$carat)

Diamonds_clean <- filter(Diamonds, y <20 & x > 0)
plot(y~x, data = Diamonds_clean, ylab="Width (mm)", xlab="Length h (mm)", col="orangered", main = "Length by Width")

