## Code extracted from .Rmd files for slides
# some of this code may be for display purposes only (especially when you see echo=F or eval=F)


# #set scientific notation digit threshold
# options(scipen=10)

#load libraries
library(ggplot2)
library(RColorBrewer)
library(kableExtra)  # used to output nice looking tables in .Rmd documents


## ---- echo=F, warning=F, message=F---------------------------------------
anscombe2 <- with(anscombe, data.frame(
  x     = c(x1, x2, x3, x4),
  y     = c(y1, y2, y3, y4),
  set = gl(4, nrow(anscombe))
))


library(plyr)
stats <- ddply(anscombe2, .(set), summarize, 
  mean_x = mean(x),
  mean_y = mean(y), 
  sd_x = sd(x),
  sd_y = sd(y),
  corr_xy = cor(x, y), 
  intercept = lm(y ~ x)$coefficients[1], 
  x_effect = lm(y ~ x)$coefficients[2]
)
stats[,1] <- as.numeric(stats[,1])

colnames(stats) <- c("set", 
                     "mean(x)", 
                     "mean(y)", 
                     "sd(x)", 
                     "sd(y)", 
                     "cor(x,y)", 
                     "intercept", 
                     "x_effect")


library(kableExtra)

kable(round(stats, 4), caption = "Anscombe's Quartet, rounded to 4 decimals") %>%
  kable_styling(bootstrap_options = c( "striped", full_width = F))


## ---- echo=F, warning=F, message=F---------------------------------------
ggplot(anscombe2, aes(x, y)) +
  geom_point(col="hotpink", size=2) +
  facet_wrap(~ set)


## ------------------------------------------------------------------------
str(airquality)


## ------------------------------------------------------------------------
airquality$season <- factor(airquality$Month, levels=5:9,
    labels=c("Summer", "Summer", "Summer", "Fall", "Fall"))


## ------------------------------------------------------------------------
summary(airquality)


## ---- echo=F-------------------------------------------------------------
summary(airquality)


## ------------------------------------------------------------------------
apply(airquality, 2, function(x) sum(is.na(x)))


## ---- eval=F-------------------------------------------------------------
## install.packages("table1")


## ---- warning=F, message=F-----------------------------------------------
library(table1)
table1(~Ozone + Solar.R + Wind + Temp + Day | season, data=airquality)


## ---- fig.width=7, fig.height=5------------------------------------------
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])


## ---- fig.width=7, fig.height=5------------------------------------------
palette(c("hotpink", "deepskyblue"))
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], 
      pch=20, col=airquality$season,
      main="Summer (pink), Fall (blue)")


## ---- fig.width=7, fig.height=5------------------------------------------
hist(airquality$Wind)
lines(density(airquality$Wind))


## ---- fig.width=7, fig.height=5------------------------------------------
hist(airquality$Wind, breaks=20)


## ---- eval=F-------------------------------------------------------------
## hist(airquality$Wind, breaks=20, freq=FALSE)
## lines(density(airquality$Wind))


## ---- fig.width=7, fig.height=5------------------------------------------
hist(airquality$Wind, breaks=20, freq=FALSE)
lines(density(airquality$Wind))


## ---- fig.width=7, fig.height=5------------------------------------------
hist(airquality$Wind, breaks=20, freq=FALSE,
     main="", ylab="Density", xlab="Wind (mph)",
     col="lightgrey")
lines(density(airquality$Wind), 
      col="hotpink", lwd=2)


## ---- fig.width=7, fig.height=5------------------------------------------
boxplot(airquality$Solar.R, horizontal=TRUE, xlab="Solar Rad. (lang)")


## ---- fig.width=7, fig.height=5------------------------------------------
boxplot(airquality$Solar.R~airquality$season, ylab="Solar Rad. (lang)", xlab="Season")


## ---- fig.width=7, fig.height=5------------------------------------------
airquality$month_fac <- factor(airquality$Month, levels=5:9,
                  labels=c("May", "Jun", "Jul", "Aug", "Sep"))
boxplot(airquality$Solar.R~airquality$month_fac, ylab="Solar Rad. (lang)", xlab="Month")


## ---- fig.width=7, fig.height=5------------------------------------------
plot(airquality$Ozone~airquality$Wind)


## ---- fig.width=6, fig.height=4------------------------------------------
pal2 <- c("hotpink", "deepskyblue")
palette(pal2)
plot(airquality$Ozone~airquality$Wind, col=airquality$season, pch=19, 
     main="Ozone vs Wind",xlab="Wind (mph)", ylab="Ozone(ppb)")
legend("topright", bty="n", pch=19, col=pal2,
       legend=levels(airquality$season))


