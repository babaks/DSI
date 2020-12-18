## Code extracted from .Rmd files for slides
# some of this code may be for display purposes only 
# (especially when you see echo=F / eval=F in the headers, or calls to kable/knitr)

#load libraries
library(knitr)
library(RColorBrewer)


## ------------------------------------------------------------------------
library(ggplot2)
table(diamonds$cut)


## ------------------------------------------------------------------------
table(diamonds$cut, diamonds$color)


## ---- eval=F-------------------------------------------------------------
## table(ct$fips)[1:60] # just the first 60


## ---- echo=F-------------------------------------------------------------
load("data/ct.Rdata")
table(ct$fips)[1:60]


## ------------------------------------------------------------------------
length(table(ct$fips))


## ---- eval=F-------------------------------------------------------------
## table(table(ct$fips))


## ---- echo=F-------------------------------------------------------------
table(table(ct$fips))


## ---- fig.height=5-------------------------------------------------------
plot(table(table(ct$fips)))


## ------------------------------------------------------------------------
ggplot(data=ct, aes(x=date, y=factor(fips))) +
  geom_raster(aes(fill=cases))


## ---- eval=F-------------------------------------------------------------
## ggplot(data=subset(ct, state=="California"), aes(x=date, y=county)) +
##   geom_raster(aes(fill=cut(cases, quantile(cases, probs=seq(0, 1, .1), na.rm=T))))
## 


## ---- echo=F, fig.width=10, fig.height=6---------------------------------
ggplot(data=subset(ct, state=="California"), aes(x=date, y=county)) +
  geom_raster(aes(fill=cut(cases, quantile(cases, probs=seq(0, 1, .1), na.rm=T))))



## ------------------------------------------------------------------------
CAdate_plt <- ggplot(data=subset(ct, state=="California"), aes(x=date, y=county)) +
  geom_raster(aes(fill=cut(cases, quantile(cases, probs=seq(0, 1, .1), na.rm=T))))


## ---- fig.width=6, fig.height=4.5----------------------------------------
ggplot(data=subset(ct, state=="California"), aes(x=date, y=county)) +
  geom_raster(aes(fill=cases)) +
  scale_fill_continuous(trans="log10",    # transformation
                        breaks=10^(1:6),  # define legend breaks 
                        labels=10^(1:6))  # define legend labels


## ---- message=F----------------------------------------------------------
CAdate_plt +
  scale_fill_viridis_d() +
  theme(legend.position = "none")


## ---- message=F----------------------------------------------------------
CAdate_plt +
  scale_fill_viridis_d(option="plasma") +
  theme(legend.position = "none")


## ---- message=F----------------------------------------------------------
CAdate_plt +
  scale_fill_viridis_d(option="magma") +
  theme(legend.position = "none")


## ---- message=F----------------------------------------------------------
CAdate_plt +
  scale_fill_brewer(palette="Spectral") +
  theme(legend.position = "none")



## ---- message=F----------------------------------------------------------
CAdate_plt +
  scale_fill_brewer(palette="Spectral", direction=-1) +
  theme(legend.position = "none")



## ---- eval=F, fig.width=10, fig.height=6, message=F----------------------
## CAdate_plt +
##   scale_fill_viridis_d(option="plasma", direction=-1) +
##   labs(title="COVID-19 cumulative cases (deciles) by date",
##        subtitle="counties in California",
##        x="",
##        y="",
##        fill="case counts \n (by decile)") +
##   theme_classic()+
##   theme(axis.text.y=element_text(size=7))


## ---- echo=F, fig.width=10, fig.height=8---------------------------------
CAdate_plt +
  scale_fill_viridis_d(option="plasma", direction=-1) +
  labs(title="COVID-19 cumulative cases (deciles) by date",
       subtitle="counties in California",
       x="",
       y="",
       fill="case counts \n (by decile)") +
  theme_classic()+
  theme(axis.text.y=element_text(size=7))


## ---- echo=F-------------------------------------------------------------
Indometh$Subject <- factor(Indometh$Subject, levels=1:6, ordered=FALSE)


## ---- fig.width=5, fig.height=3------------------------------------------
ggplot(Indometh, aes(x=time, y=conc)) + 
  geom_line(aes(color=Subject)) 


## ---- fig.width=5, fig.height=3------------------------------------------
ggplot(Indometh, aes(x=time, y=log(conc))) + 
  geom_line(aes(color=Subject)) 


## ---- figure.width=10, figure.height=5-----------------------------------
ggplot(ct, aes(x=date, y=cases, color=county)) +
  geom_line() +
  theme(legend.position="none")


## ------------------------------------------------------------------------
unique(subset(ct, county=="Adair")$state)


## ------------------------------------------------------------------------
# make a test vector of state names  
st_test <- sample(c(state.name), 10, replace=T)


## ------------------------------------------------------------------------
# match them to the abbreviations 
abbs_test <- state.abb[match(st_test, state.name)]


## ------------------------------------------------------------------------
# check  
cbind(st_test, abbs_test)


## ---- eval=F-------------------------------------------------------------
## paste0(1:5, "+", 1:5, "=", 2*1:5)


## ---- echo=F-------------------------------------------------------------
paste0(1:5, "+", 1:5, "=", 2*1:5)


## ------------------------------------------------------------------------
ct$countyst <- paste0(ct$county, 
                      " ",         # add a space
                      state.abb[match(ct$state, state.name)])


## ------------------------------------------------------------------------
# check
ct[sample(1:nrow(ct), 5), c("county", "state", "countyst")]


## ---- fig.width=10, fig.height=5.5---------------------------------------
ggplot(ct, aes(x=date, y=cases, color=countyst)) +
  geom_line() +
  theme(legend.position="none")


## ---- fig.width=10, fig.height=5-----------------------------------------
ggplot(ct, aes(x=date, y=cases, color=countyst)) +
  geom_line() +
  geom_text(data=subset(ct, date==max(date) & cases>45000),
            aes(x=date-10, y=cases, label=countyst),
            color="grey30") +
  theme(legend.position="none")


## ---- fig.width=10, fig.height=5.5---------------------------------------
ggplot(subset(ct, cases>0), aes(x=date, y=cases, color=countyst)) +
  geom_line() +
  theme(legend.position="none") + 
  scale_y_log10()


## ---- fig.width=10, fig.height=5.5---------------------------------------
ggplot(subset(ct, cases>0 & county !="Unknown"), 
       aes(x=date, y=cases, color=countyst)) +
  geom_line() +
  theme(legend.position="none") + 
  scale_y_log10()


## ---- fig.width=10, fig.height=5.5---------------------------------------
ggplot(subset(ct, state=="California"), 
       aes(x=date, y=cases, color=countyst)) +
  geom_line() 


## ---- fig.width=10, fig.height=5.5---------------------------------------
ggplot(subset(ct, state=="California" & cases>0), 
       aes(x=date, y=cases, color=countyst)) +
  geom_line() + 
  scale_y_log10(breaks=c(10, 50, 100, 500, 1000, 5000, 10000))


## ---- eval=F-------------------------------------------------------------
## ct$case_diffs <- unlist(lapply(split(ct, ct$countyst), function(x) c(NA, diff(x$cases)))))


## ------------------------------------------------------------------------
library(dplyr)
ct <- ct %>%
  group_by(countyst) %>%  # group your data by countyst
  arrange(date) %>%       # order (and implement the order) within the groups
  mutate(case_diffs = c(NA, diff(cases)))  # add a new variable


## ------------------------------------------------------------------------
# max observed cases for county (used to subset)
ct <- ct %>%
  group_by(countyst) %>%
  mutate(maxcases = max(cases))

# add column of day-to-day differences
ct <- ct %>%
  group_by(countyst) %>%
  arrange(date) %>%
  mutate(diffs_cases = c(NA, diff(cases)))

# create a sliding window average
ct <- ct %>%
  group_by(countyst) %>%
  arrange(date) %>%
  mutate(diffs_roll = data.table::frollmean(diffs_cases, n=7, na.rm=T))


## ------------------------------------------------------------------------
# order factor levels by most recent # cases
# use ct_last to define order
ct_last <- ct %>%
  filter(date==max(date)) %>%
  arrange(cases, state, county) %>%
  select(county, state, countyst, cases)

#re-order the factor levels
ct$countyst <- factor(ct$countyst,
                      levels=rev(unique(ct_last$countyst)),
                      ordered=TRUE)


## ---- fig.width=10, fig.height=8-----------------------------------------
ggplot(subset(ct, state=="California" & maxcases>500), 
       aes(x=date, y=cases, color=countyst)) +
  geom_line() + 
  scale_y_log10(breaks=c(10, 50, 100, 500, 1000, 5000, 10000)) +
  labs(title="cumulative case counts, log-scaled",
       subtitle="California counties with > 500 cases",
       color="county name \n (ordered by # cases)")



## ------------------------------------------------------------------------
cens <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
dim(cens)


## ------------------------------------------------------------------------
cens <- data.frame("pop2019" = cens$POPESTIMATE2019,
                   "county" = cens$CTYNAME,
                   "state" = cens$STNAME,
                   "fips_st" = cens$STATE,
                   "fips_ct" = cens$COUNTY)


## ---- echo=F-------------------------------------------------------------
knitr::kable(head(cens), 'html')


## ------------------------------------------------------------------------
library(stringr)
ct_words <- str_split(cens$county, pattern=" ")
last_words <- unlist(lapply(ct_words, function(x) x[ifelse(length(x)>1, length(x), 0)]))
table(last_words)


## ------------------------------------------------------------------------
#fips codes: 2-digit state + 3-digit county
combine_fips <- function(f_st, f_ct){
  # f_ct=0 indicates a state total
  f_ct <- ifelse(f_ct==0, NA, f_ct) 
  
  # count # digits in county FIPS code
  # & how many 0s needed between codes
  num_dig <- 1+floor(log(f_ct, 10)) 
  num_0s <- 3-num_dig 
  
  # stick num0s 0s between the state and county codes
  if(is.na(num_0s)){
    return(NA)
  } else if (num_0s==0){
    return(as.numeric(paste0(f_st,f_ct)))
  } else if (num_0s==1){
    return(as.numeric(paste0(f_st, "0", f_ct)))
  } else if (num_0s==2){
    return(as.numeric(paste0(f_st, "00", f_ct)))
  }
}


## ------------------------------------------------------------------------
cens$fips <- apply(cens, 1, function(x) combine_fips(f_st=as.numeric(x["fips_st"]), 
                                                     f_ct=as.numeric(x["fips_ct"])))


## ---- warning=T, message=T, error=T--------------------------------------
ct_test <- left_join(ct, cens, by="fips")


## ------------------------------------------------------------------------
# checking each fips appears only 1x in cens
# otherwise left_join() will duplicate unnecessarily
sum(table(cens$fips))==length(unique(cens$fips)) 


## ------------------------------------------------------------------------
cens <- cens[!is.na(cens$fips), ] # may be more than one NA here
sum(table(cens$fips))==length(unique(cens$fips))


## ---- message=T----------------------------------------------------------
cens$fips <- factor(cens$fips)
ct_test <- left_join(ct, cens, by="fips")


## ------------------------------------------------------------------------
names(ct_test)
ct_test[, c("county.x", "fips", "county.y")]


## ------------------------------------------------------------------------
ct_test <- ct_test[, 1:13]
colnames(ct_test)[c(2, 3, 13)] <- c("county", "state", "pop")


## ------------------------------------------------------------------------
ct <- ct_test
rm(ct_test) # delete


## ---- echo=F-------------------------------------------------------------
library(knitr)
library(kableExtra)
kable(head(ct), 'html') %>%
  kable_styling(bootstrap_options = c( "striped", "scale_down", "hover"), font_size=11)

