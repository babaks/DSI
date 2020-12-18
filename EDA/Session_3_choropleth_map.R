##################################################
## Project:   DSI workshop: making choropleth map
## Date:      Jun 22, 2020
## Author:    Arnie Seong
##################################################




# SETUP/LOAD --------------------------------------------------------------

library(dplyr)
library(pryr)

library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# setup plotting
dev.off() # like a reboot for plotting
pardefault <- par(no.readonly=T) # save default base R plotting parameters.  
#To restore defaults, call par(pardefault)
palramp <- colorRampPalette(c("deepskyblue", "purple", "orangered")) # a palette scheme that I like
transp_pal <- function(pal, alpha=75){
  # adds transparency to existing color palette
  # alpha scale in base R goes from 0 to 255
  return( rgb(t(col2rgb(pal)), alpha = alpha, maxColorValue = 255) )
} 






# Review ------------------------------------------------------------------

# Div1 <- c(Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, Vermont)
# Div2 <- c(New Jersey, New York, Pennsylvania)
# Div3 <- c(Illinois, Indiana, Michigan, Ohio, Wisconsin)
# Div4 <- c(Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota, South Dakota)
# Div5 <- c(Delaware, Florida, Georgia, Maryland, North Carolina, South Carolina, Virginia, District of Columbia, West Virginia)
# Div6 <- c(Alabama, Kentucky, Mississippi, Tennessee)
# Div7 <- c(Arkansas, Louisiana, Oklahoma, Texas)
# Div8 <- c(Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah, Wyoming)
# Div9 <- c(Alaska, California, Hawaii, Oregon, Washington)





# LOAD DATA ---------------------------------------------------------------
load(file="data/Ex2_done_alldata.Rdata")
contents
names(ct)



# VISUAL CHECK: ESRI STAY HOME ORDERS -------------------------------------
esri


ct <- ct %>%
  group_by(state, date) %>%
  mutate(state_cases=sum(cases))


ggplot(ct, aes(x=date, y=state, 
               alpha=stayhome_active, fill=state_cases)) + 
  geom_raster() + 
  labs(title="cumulative cases and state-mandated stay-at-home orders",
       subtitle="source: ESRI geographic information systems & NYTimes",
       fill=" ",
       alpha=" ",
       x=" ", 
       y=" ") +
  theme_classic() +
  scale_fill_viridis_c(option="plasma", trans="log10") + 
  scale_alpha(range=c(.2, 1), 
              breaks=c(0, 1), 
              labels=c("inactive", "active"),
              guide="none") + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(5, "line"), 
        legend.key.height = unit(.5, "line"))






ct <- ct %>%
  group_by(fips) %>%
  arrange(date) %>%
  mutate(state_casediffs=c(NA, diff(state_cases)))

ct <- subset(ct, !is.na(fips) & county!="Unknown")

ggplot(ct, aes(x=date, y=state, 
               alpha=stayhome_active, fill=state_casediffs)) + 
  geom_raster() + 
  labs(title="day-to-day new cases and state-mandated stay-at-home orders",
       subtitle="source: ESRI geographic information systems & NYTimes \n - gray indicates no change or downwards adjustment",
       fill=" ",
       alpha=" ",
       x=" ", 
       y=" ") +
  theme_classic() +
  scale_fill_viridis_c(option="plasma", trans="log10") + 
  scale_alpha(range=c(.25, 1),
              guide="none") + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(5, "line"), 
        legend.key.height = unit(.5, "line"))




# CASERATE and DEATHRATE BY PARTY ---------------------------------------------------
# Before we get into choropleth maps...
# Visualizing: political affiliation, caserate, population, counties by state

maxdate <- max(ct$date)

# ggplot(subset(ct, date==max(date)), 
#        aes(x=gop_winby, 
#            y=state, 
#            size=cases/pop, 
#            color=log(pop, 10))) + 
#   geom_point(aes(alpha=cases/pop))


Cases_byparty <- ggplot(subset(ct, date==max(date)), 
                        aes(x=gop_winby, 
                            y=state, 
                            size=cases/pop, 
                            color=log(pop, 10))) + 
  geom_point(aes(alpha=cases/pop)) + 
  # add line down middle
  geom_segment(x=0, y=0, xend=0, yend=100, col="grey50", size=.5) +
  # redo plot labels
  labs(title="total cases per capita (size) by 2016 county election results",
       subtitle=paste0("New York Times COVID-19 data for ", maxdate),
       y="",
       x="percent party won by",
       color="population") + 
  # relabel x axis
  scale_alpha_continuous() +
  scale_x_continuous(breaks=c(-.5, 0, .5), 
                     labels=c("Dem by 50%", "Tie", "GOP by 50%"))

Cases_byparty


# better color scale and labels
Cases_byparty + theme_classic() +
  scale_color_viridis_c(option="plasma", 
                        breaks=3:7, 
                        labels=c("1k", "10k", "100k", "1m", "10m"))


# Similar, but deathrate (deaths/cases)
DperC_byparty <- ggplot(subset(ct, date==max(date)), 
                        aes(x=gop_winby, y=state, size=deaths/cases, color=log(pop, 10))) + 
  geom_point(aes(alpha=deaths/cases)) + 
  geom_segment(x=0, y=0, xend=0, yend=100, col="grey50", size=.5) +
  labs(title="total deaths / total cases by county 2016 election results",
       subtitle=paste0("New York Times COVID-19 data for ", maxdate),
       y="",
       x="percent party won by",
       color="population") + 
  scale_x_continuous(breaks=c(-.5, 0, .5), 
                     labels=c("Dem by 50%", "Tie", "GOP by 50%"))

DperC_byparty + theme_classic() +
  scale_color_viridis_c(option="plasma", 
                        breaks=3:7, 
                        labels=c("1k", "10k", "100k", "1m", "10m"))













# FORMATTING DATA: ELEC DATA + CHOROPLETH BASICS -----------------------------
# To make a choropleth map, need some sort of shapefile / file to draw polygons from.  
# The maps package has the ones we need
library(maps)

# grab the county-level data from maps package
counties <- map_data("county") 
# the long(itude), lat(itude), and order columns define the outline of each county polygon
# the group is also important, as we'll see

# the maps package also contains a county-fips key
data(county.fips)
counties[1:5, ] 
ct[1:5, ]
# here I'm formatting these variables so that I can join counties with elec.
# I need fips in counties, so I'll take it from county.fips
# but I need to join these first
counties$polyname <- paste0(counties$region,",", counties$subregion)
counties <- left_join(counties, county.fips, by="polyname")

elec_choro <- left_join(counties, elec, by="fips")
# dangit, that dumb choice of having fips as a factor in ct is haunting us.


elec$fips <- as.numeric(as.character(elec$fips))
elec_choro <- left_join(counties, elec, by="fips")

#just to check size of the files
pryr::object_size(elec)
pryr::object_size(counties)
pryr::object_size(elec_choro)

elec_choro[1:5, ]


# ELEC CHORO MAP ----------------------------------------------------------

# basic map, using the gop_winby variable to fill
ggplot() +
  geom_polygon(data=counties, aes(x=long, y=lat, group=group, fill=group))

# what happens when you forget group
ggplot() +
  geom_polygon(data=counties, aes(x=long, y=lat))


# in general I like a few options in general on choropleth maps
elec_chplt <- ggplot() +
  geom_polygon(data=elec_choro, 
               aes(x=long, y=lat, group=group, fill=gop_winby), 
               colour="white") +   # white outlines
  labs(x="", 
       y="") +
  scale_y_continuous(breaks=c()) + # no axis ticks
  scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank()) + # border
  coord_fixed(1.3) # fix aspect ratio.  For some reason 1.3 looks better to me than 1

elec_chplt #OK, a good base.



# let's try out a different color gradient.
elec_chplt + 
  scale_fill_gradient2(low="dodgerblue", mid="white", high="red", na.value="black")

# Ugh.  Let's try out a viridis palette.  I usually like "plasma"
elec_chplt + 
  scale_fill_viridis_c(option="plasma")

# Not bad.  Let's try "magma"  
elec_chplt + 
  scale_fill_viridis_c(option="magma")

# that seems closer to the usual color scheme
# and contrast is OK
# let's make the legend better and give a title
elec_chplt + 
  labs(title="county-level election results",
       fill="") +
  scale_fill_viridis_c(option="magma",
                       breaks=c(-.5, 0, .5), 
                       labels=c("dem win by 50%", "even", "gop win by 50%"))








# CHOROMAPS COVID ---------------------------------------------------------

# # Already did this above....
library(maps)
counties <- map_data("county")
states <- map_data("state")
data(county.fips)

counties$polyname <- paste0(counties$region,",", counties$subregion)
counties <- left_join(counties, county.fips, by="polyname")

# Here I'm joining in the reverse order because I want to eliminate some of the Unknowns and NAs in ct.  So we'll only plot what we have the polygons for.
ct$fips <- as.integer(as.character(ct$fips))
choro_df_large <- left_join(counties, ct, by="fips")

# lots of duplication required b/c each county gets so many rows in the df counties
pryr::object_size(choro_df_large)  # 1.16 GB  yikes.


# let's just subset to the last date we have
maxdate <- max(ct$date)
choro_df <- subset(choro_df_large, date==maxdate)
rm(choro_df_large)
pryr::object_size(choro_df)        # 14.6 MB  - MUCH easire / faster / more manageable.


# color scaling terribly uninformative
ggplot() +
  geom_polygon(data=choro_df, 
               aes(x=long, y=lat, group=group, fill=cases), 
               colour="white")


base <- ggplot() +
  geom_polygon(data=counties, 
               aes(x=long, y=lat, group=group), colour="white", fill="grey80") +
    geom_polygon(data=choro_df, 
               aes(x=long, y=lat, group=group, fill=cases), 
               colour="white") +
  labs(title=paste0("cumulative case count by county, ", maxdate),
       subtitle="source: NYTimes county-level data \n - gray indicates no data for date",
       fill="cumulative cases \n June 19, 2020", 
       x="", y="") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + 
  theme(panel.border = element_blank(),
        panel.background = element_blank()) + coord_fixed(1.3)

base +
  scale_fill_viridis_c(option = "plasma", 
                       trans="log10") +
  theme(legend.position = "bottom", 
        legend.key.width = unit(5, "line"), 
        legend.key.height = unit(.5, "line"))



unique(ct$cens_reg)
# same code; let's look at regions
ggplot() +
  geom_polygon(data=subset(choro_df, cens_reg=="Northeast"), 
               aes(x=long, y=lat, group=group, fill=cases), 
               colour="white") +
  labs(fill="cumulative cases", 
       title=paste0("cumulative case count by county, ", maxdate),
       x="", y="") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank()) + coord_fixed(1.3) +
  scale_fill_viridis_c(option = "plasma", 
                     trans="log10") 









# More complex ------------------------------------------------------------

# cut into quantiles and make vectors for legends
disp_probs <- c(0, .25, .5, .75, .9, .95, .99, 1)
ptiles <- paste0("(", disp_probs[1:(length(disp_probs)-1)]*100, 
                 "% to ", disp_probs[2:length(disp_probs)]*100, "%]")



# uh oh, this was bad.  I should have taken the quantiles BEFORE all the values 
# were multiplied by the number of points it takes to draw the counties (different for each county!)
# in the join() operation.
# We're ok regarding time, though, since this is just from 1 date.
choro_df$qtile <- cut(choro_df$cases,
                      quantile(choro_df$cases, probs=disp_probs, na.rm=T),
                      dig.lab=5)


qtiles <- unique(choro_df$qtile)[order(unique(choro_df$qtile))]


qtile_labs <- paste0(qtiles, " \n", ptiles)
qtile_labs[length(qtile_labs)] <- "NA"




ggplot() +
  geom_polygon(data=choro_df, 
               aes(x=long, y=lat, group=group, fill=qtile), 
               colour="white") +
  # geom_polygon(data=states,
  #              aes(x=long, y=lat, group=group), fill=NA,
  #              colour="black", size=1) +
  scale_fill_viridis_d(option = "plasma",
                       labels = qtile_labs) +
  labs(fill="cumulative cases \n(quantile range)", 
       title=paste0("cumulative cases by county, ", maxdate),
       subtitle="percentile/quantile ranges in legend",
       x="", y="") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank()) + coord_fixed(1.3) + 
  guides(fill=guide_legend( #getting 2 lines on the legend for each color
    keywidth=0.2, #making color blocks in legend bigger
    keyheight=0.3,
    default.unit="inch")
  )










