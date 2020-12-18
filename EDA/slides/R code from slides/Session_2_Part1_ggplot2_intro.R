## Code extracted from .Rmd files for slides
# some of this code may be for display purposes only 
# (especially when you see echo=F / eval=F in the headers, or calls to kable)

#load libraries
library(knitr)
library(ggplot2)
library(RColorBrewer)


## ---- echo=F-------------------------------------------------------------
knitr::kable(head(diamonds), format = 'html')


## ---- eval=FALSE, fig.width=7, fig.height=5------------------------------
## ggplot(data=diamonds, aes(x=carat, y=price)) +
##   geom_point()


## ---- echo=FALSE, fig.width=7, fig.height=5------------------------------
ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_point()


## ---- eval=FALSE, fig.width=7, fig.height=5------------------------------
## ggplot(data=diamonds, aes(x=carat, y=price, color=color)) +
##   geom_point()


## ---- echo=FALSE, fig.width=7, fig.height=5------------------------------
ggplot(data=diamonds, aes(x=carat, y=price, color=color)) + 
  geom_point()


## ---- eval=FALSE, fig.width=7, fig.height=5------------------------------
## ggplot(data=diamonds, aes(x=carat, y=price, color=color)) +
##   geom_point(alpha=.25) +
##   geom_smooth()


## ---- echo=FALSE, fig.width=7, fig.height=5------------------------------
ggplot(data=diamonds, aes(x=carat, y=price, color=color)) + 
  geom_point(alpha=.25) + 
  geom_smooth()


## ---- eval=F-------------------------------------------------------------
## ggplot(data=diamonds, aes(x=carat, y=price, color=color)) +
##   geom_point(alpha=.25) +
##   geom_smooth()


## ---- fig.width=7, fig.height=5, message=FALSE---------------------------
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point(alpha=.25) + 
  geom_smooth()


## ---- eval=F-------------------------------------------------------------
## ggplot(data=diamonds, aes(x=carat, y=price)) +
##   geom_point(aes(color=cut), alpha=.25) +
##   geom_smooth()


## ---- echo=F, fig.width=7, fig.height=5, message=F-----------------------
ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_point(aes(color=cut), alpha=.25) + 
  geom_smooth()


## ---- eval=F, fig.width=7, fig.height=5----------------------------------
## ggplot(data=diamonds, aes(x=price)) +
##   geom_histogram(aes(color=cut), alpha=.25)


## ---- echo=F, fig.width=7, fig.height=5----------------------------------
ggplot(data=diamonds, aes(x=price)) + 
  geom_histogram(aes(color=cut), alpha=.25)


## ---- eval=F, fig.width=7, fig.height=5, message=F-----------------------
## ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) +
##   geom_histogram(alpha=.25)


## ---- echo=F, fig.width=7, fig.height=5, message=F-----------------------
ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) + 
  geom_histogram(alpha=.25)


## ---- eval=F, fig.width=7, fig.height=5, message=F-----------------------
## ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) +
##   geom_histogram(alpha=.25, position="identity")


## ---- echo=F, fig.width=7, fig.height=5, message=F-----------------------
ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) + 
  geom_histogram(alpha=.25, position="identity")


## ---- eval=F, fig.width=7, fig.height=5, message=F-----------------------
## ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) +
##   geom_histogram(alpha=.25) +
##   facet_wrap(~cut)


## ---- echo=F, fig.width=7, fig.height=5, message=F-----------------------
ggplot(data=diamonds, aes(x=price, color=cut, fill=cut)) + 
  geom_histogram(alpha=.25) + 
  facet_wrap(~cut)


## ---- eval=F, fig.width=7, fig.height=5, message=F-----------------------
## ggplot(data=diamonds, aes(x=price, y=..density..,
##                           color=cut, fill=cut)) +
##   geom_histogram(alpha=.25) +
##   facet_wrap(~cut)


## ---- echo=F, fig.width=7, fig.height=5, message=F-----------------------
ggplot(data=diamonds, aes(x=price, y=..density.., 
                          color=cut, fill=cut)) + 
  geom_histogram(alpha=.25, bins=30)+ 
  facet_wrap(~cut)


## ---- fig.width=7, fig.height=5, message=F-------------------------------
ggplot(data=diamonds, aes(x=carat, y=..density..,
                          color=cut, fill=cut)) +
  geom_histogram(alpha=.25, bins=30) +
  facet_wrap(~cut)


## ---- fig.width=7, fig.height=5, message=F-------------------------------
ggplot(data=diamonds, aes(x=carat, color=cut, fill=cut)) +
  geom_density(alpha=.5)


## ------------------------------------------------------------------------
# discretizing carats into quartiles
carat_levs <- c(0, quantile(diamonds$carat))
diamonds$carat_fac <- factor(cut(diamonds$carat, carat_levs), ordered=TRUE)

ggplot(data=diamonds, aes(x=carat_fac, y=price, color=cut)) + 
  geom_boxplot() 


## ---- eval=F-------------------------------------------------------------
## bp <- ggplot(data=diamonds, aes(x=carat_fac, y=price, color=cut)) +
##   geom_boxplot()
## 
## bp +
##   labs(title="price by carat quartile",
##        subtitle="broken down by cut quality",
##        x="carat quartiles")


## ---- echo=F-------------------------------------------------------------
bp <- ggplot(data=diamonds, aes(x=carat_fac, y=price, color=cut)) + 
  geom_boxplot() 

bp +
  labs(title="price by carat quartile", 
       subtitle="broken down by cut quality",
       x="carat quartiles")


## ------------------------------------------------------------------------
# sample subset + build loess model
dia_sub <- diamonds[sample(1:nrow(diamonds), 500), ]
mod <- loess(price~carat, data=dia_sub)

# generate predictions
grid <- data.frame(carat = seq(min(diamonds$carat), max(diamonds$carat), length = 100))
grid$price <- predict(mod, newdata = grid)

ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_point(alpha=.01) + 
  geom_smooth(method='loess', se=FALSE) +
  geom_line(data=grid, aes(x=carat, y=price), color="hotpink")



