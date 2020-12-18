## Code extracted from .Rmd files for slides
# some of this code may be for display purposes only 
# (especially when you see echo=F / eval=F in the headers, or calls to kable)

# #set scientific notation digit threshold
# options(scipen=10)

#load libraries
library(ggplot2)
library(RColorBrewer)


## ----Vectors-------------------------------------------------------------
numVec <- c(2,3,4)  # <- is the assigning operator
numVec
length(numVec)  # gives the number of elements in the vector

x <- 2
is.vector(x)  # single numbers are stored as vectors of length 1


## ----Character_and_Logical_Vector_Examples-------------------------------
logVec <- c(TRUE, FALSE, FALSE, T, F)
logVec


## ------------------------------------------------------------------------
charVec <- c("red", "green", "blue")
charVec


## ------------------------------------------------------------------------
logVec2 <- charVec=="red"
logVec2


## ------------------------------------------------------------------------
charVec2 <- c("green", "red", "blue")
charVec2 == charVec


## ------------------------------------------------------------------------
logVec2 <- charVec=="red"
logVec2


## ------------------------------------------------------------------------
x <- 1:10
y <- 10:1
x > y


## ------------------------------------------------------------------------
logVec
!logVec


## ----data_type_coercion1-------------------------------------------------
numCharVec <- c(3.14, pi, "pi")
numCharVec                 


## ----data_type_coercion2-------------------------------------------------
numVec <- 1:10
numToChar <- as(numVec, "character")
numToChar


## ------------------------------------------------------------------------
numToChar + 1


## ----factors1------------------------------------------------------------
sex <- rep(c(0,1), times = 4)
sex


## ----factors2------------------------------------------------------------
sex2 <- factor(sex, levels=c(0,1),  # original values
              labels=c("Male", "Female")) # new values
sex2


## ------------------------------------------------------------------------
sex <- factor(c("Male", "Female", "M", "F", "Dude", "F", "Male", "Dudette"))
sex


## ------------------------------------------------------------------------
sex <- factor(sex, levels=c("Dude", "Dudette", "F", "Female", "M", "Male"),
              labels=c("Male", "Female", "Female", "Female", "Male", "Male"))
sex


## ----factors_level_order1------------------------------------------------
sizes <- factor(c("small", "med", "large"))
sizes      # levels ordered alphabetically


## ----factors_level_order2------------------------------------------------
sizes_ord <- factor(sizes, levels=c("small", "med", "large"))
sizes_ord # same values, but levels are in the specified order


## ----factors_level_order1rep---------------------------------------------
sizes <- factor(c("small", "med", "large"))
sizes


## ------------------------------------------------------------------------
as.numeric(sizes)  # levels ordered alphabetically (default)


## ------------------------------------------------------------------------
as.numeric(sizes_ord)  # levels ordered by small to large sizes


## ----factors_level_order1rep2--------------------------------------------
sizes <- factor(c("small", "med", "large"))
sizes


## ------------------------------------------------------------------------
as.numeric(sizes)==as.numeric(sizes_ord)
sizes==sizes_ord


## ----Matrices------------------------------------------------------------
myMat <- matrix(NA, nrow = 2, ncol = 4)
myMat


## ------------------------------------------------------------------------
length(myMat)  # gives total # of elements (nrow times ncol)
dim(myMat)  # gives a length 2 vector (nrow, ncol)



## ----fill_Matrix---------------------------------------------------------
evens <- seq(2, 16, 2)
evens


## ------------------------------------------------------------------------
myMat <- matrix(evens, nrow = 2, ncol = 4)
myMat 



## ----fill_Matrix2--------------------------------------------------------
evens <- seq(2, 16, 2)
evens


## ------------------------------------------------------------------------
myMat <- matrix(evens, nrow = 2, ncol = 4, byrow=TRUE)
myMat


## ----echo=TRUE-----------------------------------------------------------
vec1 <- 1:4
vec2 <- 5:8
vec3 <- 9:12


## ------------------------------------------------------------------------
colMat <- cbind(vec1, vec2, vec3)
colMat


## ----echo=TRUE-----------------------------------------------------------
vec1 <- 1:4
vec2 <- 5:8
vec3 <- 9:12


## ----echo=TRUE-----------------------------------------------------------
rowMat <- rbind(vec1, vec2, vec3)
rowMat


## ------------------------------------------------------------------------
myArr <- array(1:5, dim=c(3, 4, 5))  # something fishy here?
myArr[, , 1]


## ------------------------------------------------------------------------
myList <- list("sex"=sex, 
               "rowMat"=rowMat)
length(myList)
myList


## ----echo=TRUE-----------------------------------------------------------
a <- Inf
b <- 0
rslt <- c(b/a, a/a, 1/b)
rslt


## ----echo=TRUE-----------------------------------------------------------
a <- c(1,2)
a[3]

b <- 0/0
b


## ----echo=TRUE-----------------------------------------------------------
vec <- c(1, NA, 3, NaN, NA, 5, NaN)
is.na(vec)  # look at elements 4 and 5
is.nan(vec) # compare


## ------------------------------------------------------------------------
female <- rep(c(0,1), times = 4)
height <- sample(62:70, length(female))
df <- data.frame(female, sex, height)
df


## ----load_C19_data-------------------------------------------------------
ct <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


## ------------------------------------------------------------------------
head(ct)


## ------------------------------------------------------------------------
tail(ct, 5)  # specify just the last 5 rows


## ------------------------------------------------------------------------
tail(ct, 5)  # specify just the last 5 rows


## ---- eval=FALSE---------------------------------------------------------
## vec <- 1:10    # integers from 1 to 10
## vec <- vec*10  # multiply each element in vec by 10
## vec[3]         # predict output


## ---- echo=FALSE---------------------------------------------------------
vec <- 1:10
vec <- vec*10
vec[3] 


## ---- eval=FALSE---------------------------------------------------------
## vec[1:3]        # predict output


## ---- echo=FALSE---------------------------------------------------------
vec[1:3]


## ---- eval=FALSE---------------------------------------------------------
## vec[c(2,4,6)]   # predict output


## ---- echo=FALSE---------------------------------------------------------
vec[c(2,4,6)]


## ------------------------------------------------------------------------
ct[1:2, ]  # get the first 2 rows


## ------------------------------------------------------------------------
ct[1, 2]   # get the element in the first row, second column


## ------------------------------------------------------------------------
head(ct$fips)  # select the first 6 values of the second column


## ------------------------------------------------------------------------
ct$fips[1:6]


## ------------------------------------------------------------------------
ct[1:6, 4]


## ---- eval=F-------------------------------------------------------------
## vec[c(1, 1, 1)]  # predict output


## ---- echo=F-------------------------------------------------------------
vec[c(1, 1, 1)]


## ------------------------------------------------------------------------
geq10k <- ct$cases >= 10000  # T/F returned 
length(geq10k)


## ------------------------------------------------------------------------
tail(geq10k)
tail(ct$cases)


## ------------------------------------------------------------------------
sum(geq10k) # Treats TRUE as 1 and FALSE as 0


## ------------------------------------------------------------------------
unique(ct$county[geq10k])


## ------------------------------------------------------------------------
CA_geq5k <- which(ct$state=="California" 
                 & ct$date=="2020-06-01"
                 & ct$cases > 5000)
CA_geq5k


## ------------------------------------------------------------------------
ct[CA_geq5k, ]


## ------------------------------------------------------------------------
ct[1:5, c("county", "cases")]


## ------------------------------------------------------------------------
ct[1:5, c(2,5)]


## ------------------------------------------------------------------------
june1_inds <- which(ct$date=="2020-06-01")
sort_inds <- order(ct$cases[june1_inds], decreasing=FALSE)


## ------------------------------------------------------------------------
ct[sort_inds[1:5],]


## ------------------------------------------------------------------------
x <- c(4, 2, 1, 3, 5)


## ------------------------------------------------------------------------
order(x)


## ------------------------------------------------------------------------
june1_inds <- which(ct$date=="2020-06-01")
sort_inds <- order(ct$cases[june1_inds], decreasing=FALSE)


## ----eval=F--------------------------------------------------------------
## ct[sort_inds[1:5],]


## ------------------------------------------------------------------------
length(sort_inds)
nrow(ct)


## ---- eval=F-------------------------------------------------------------
## june1_inds <- which(ct$date=="2020-06-01")
## ct_june1 <- ct[june1_inds, ]  # create new subsetted df
## sort_inds <- order(ct_june1$cases)
## ct_june1[sort_inds[1:5],]
## 


## ---- echo=F-------------------------------------------------------------
june1_inds <- which(ct$date=="2020-06-01")
ct_june1 <- ct[june1_inds, ]  # create new subsetted df
sort_inds <- order(ct_june1$cases)
ct_june1[sort_inds[1:5],]



## ---- eval=F-------------------------------------------------------------
## subset(ct, ct$state=="California"
##            & ct$date=="2020-06-01"
##            & ct$cases > 5000)


## ---- eval=F-------------------------------------------------------------
## library(dplyr)
## ct %>%
##   filter(date=="2020-06-01") %>%
##   arrange(cases, state, county) %>%
##   slice(1:5)


## ------------------------------------------------------------------------
contents <- c("ct", "ct_june1")
save(contents, ct, ct_june1, file="data/session1.Rdata")


## ---- eval=F-------------------------------------------------------------
## load("data/session1.Rdata")


## ------------------------------------------------------------------------
contents


## ---- eval=F-------------------------------------------------------------
## write.table(ct_june1,
##             file = "data/ct_june1.csv",
##             sep = ",",
##             col.names = TRUE,
##             row.names = FALSE)


## ---- eval=F-------------------------------------------------------------
## read.csv("data/ct_june1.csv")


## ----eval=FALSE----------------------------------------------------------
## install.packages("RColorBrewer")  # put it in quotes!
## library(RColorBrewer)  # load it into your R workspace


## ---- eval=FALSE---------------------------------------------------------
## ?quantile


## ----if_syntax_do_not_run, eval = F--------------------------------------
## if (condition){
##   # do something
## } else if (condition2) {
##   # do something else
## } else {
##   # do this other catchall thing
## }


## ------------------------------------------------------------------------
x <- sample(1:5, size=1)
if (x<3){
  print("less than 3")
} else {
  print("greater than / equal to 3")
}


## ----ifelse_syntax_do_not_run, eval=F------------------------------------
## ifelse(TF_vector, do_if_true, do_if_false)


## ------------------------------------------------------------------------
x <- 1:5
ifelse(x<3, "small", "large")


## ----while_syntax_do_not_run, eval=FALSE---------------------------------
## while (condition){
##   # do this over and over
##   # modify condition!
## }


## ------------------------------------------------------------------------
x <- 1
while (x < 5){
  print(x)
  x <- x+1
}


## ----for_syntax_do_not_run, eval=FALSE-----------------------------------
## for (el in set){
##   #do something depending on el
## }


## ------------------------------------------------------------------------
x <- c("a", "b", "c", "d")
for (let in x){
  print(let)
}


## ---- echo=FALSE---------------------------------------------------------
letter_mat <- matrix(sample(letters, 25, replace=TRUE),
            nrow=5)
letter_mat


## ------------------------------------------------------------------------
y <- rep(NA, 4)  # pre-populate vector y
for (i in 1:4){
    y[i] <- letter_mat[i, i+1]
}
print(y)


## ----apply_syntax_donoteval, eval=F--------------------------------------
## apply(mat, MARGIN, function(x) some_func(x, other_args))


## ------------------------------------------------------------------------
mat <- rbind(rnorm(10), rnorm(10, 5), rnorm(10, -5, 2))
dimnames(mat) <- list(paste0("row", 1:3), 
                      paste0("col", 1:10))
round(mat,1) # for display on slide


## ---- eval=FALSE---------------------------------------------------------
## mat <- rbind(rnorm(10), rnorm(10, 5), rnorm(10, -5, 2))
## dimnames(mat) <- list(paste0("row", 1:3),
##                       paste0("col", 1:10))
## round(mat,1) # for display on slide


## ---- eval=F-------------------------------------------------------------
## apply(mat, 1, mean)


## ---- echo=F-------------------------------------------------------------
apply(mat, 1, mean)


## ---- results='hide'-----------------------------------------------------
mat <- rbind(rnorm(1000), rnorm(1000, 5), rnorm(1000, -5, 2))
dimnames(mat) <- list(paste0("row", 1:3), 
                      paste0("col", 1:1000))


## ---- eval=F-------------------------------------------------------------
## apply(mat, 1, function(x) quantile(x, probs=c(.025, .5, .975)))


## ---- echo=F-------------------------------------------------------------
apply(mat, 1, function(x) quantile(x, probs=c(.025, .5, .975)))


## ------------------------------------------------------------------------
my_func <- function(arg1, arg2=1:4){  # declare function my_func
  result <- sum(arg1) / sum(arg2)     # do stuff
  return(result) 
}


## ------------------------------------------------------------------------
my_func(arg1=1:100)


## ------------------------------------------------------------------------
result # does not exist outside of function


## ------------------------------------------------------------------------
arg1   # does not exist outside of function


## ------------------------------------------------------------------------
my_func <- function(arg1, arg2=1:4){  # declare function my_func
  result <- sum(arg1) / sum(arg2)     # do stuff
  return(result) 
}


## ------------------------------------------------------------------------
my_func <- function(arg1, arg2=1:4){  # declare function my_func
  result <- sum(arg1) / sum(arg2)     # do stuff
  return(result) 
}


## ------------------------------------------------------------------------
my_func(arg1=1:100)
my_func(arg1=1)
my_func(arg1=100:200, arg2=5:15)


## ------------------------------------------------------------------------
ct$date <- as.Date(ct$date)


## ------------------------------------------------------------------------
dat <- subset(ct, fips==6059)
dat[1:3,]


## ------------------------------------------------------------------------
diff(c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))


## ---- eval=F-------------------------------------------------------------
## diff(dat$date)


## ---- echo=F-------------------------------------------------------------
diff(dat$date)


## ------------------------------------------------------------------------
dat <- dat[c(1:5, 10:15, 20:25), ]


## ------------------------------------------------------------------------
diff(dat$date)



## ------------------------------------------------------------------------
daydiffs <- diff(dat$date)
daydiffs


## ------------------------------------------------------------------------
n_jumps <- sum(daydiffs != 1)
n_jumps


## ------------------------------------------------------------------------
total_missed <- sum(daydiffs-1) 
total_missed


## ------------------------------------------------------------------------
days_missed <- function(date_vec){
  daydiffs <- diff(date_vec) #changed dat$date to date_vec
  n_jumps <- sum(daydiffs != 1)
  total_missed <- sum(daydiffs-1)
  
  # format the output
  res <- c(n_jumps, total_missed)
  names(res) <- c("jumps", "total")
  
  return(res)
}


## ------------------------------------------------------------------------
days_missed(dat$date)

