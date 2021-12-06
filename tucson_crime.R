# Alex Carney
# 2020 Tucson Crime Data Cluster Analysis

######PRE-PROCESS AND CLEAN UP######
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.of.packages <- c("ggplot2","dplyr","cluster","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
library(dplyr)
library(cluster)
library(tidyverse)
data <- read.csv("tucson_police_incidents_2020.csv")
zip_pop <- read.csv("population.csv")
data <- data[c('X','Y','city','state','zip','LOC_STATUS','MONTH_REPT','YEAR_REPT','TIME_REPT','STATUTDESC','DIVISION')]

# Clean up white space problems and string column inconsistencies 
data$city <- trimws(data$city,which= c("both"))
data$state <- trimws(data$state,which= c("both"))
data$STATUTDESC <- trimws(data$STATUTDESC,which= c("both"))
data$DIVISION <- trimws(data$DIVISION,which= c("both"))
data$LOC_STATUS <- trimws(data$LOC_STATUS,which= c("both"))

# Rename columns for clarity, in particular city to AREA, 
# since areas such as Oro Valley are considered "part" of Tucson but aren't "cities"
names(data) <- c('X','Y','AREA','STATE','ZIP','LOC_STATUS','MONTH_NUM','YEAR','TIME_REPT','DESC','OPS_DIVISION')

# Only consider rows with valid geo location data
data <- data[(data$LOC_STATUS == 'GEOCODED') & (data$STATE == 'AZ'),]

# 'TUC' is supposed to be 'TUCSON', make a string MONTH column
data$AREA[data$AREA == 'TUC'] <- "TUCSON"
month_abbrs <- c("Jan","Feb","Mar",
                "Apr","May","Jun",
                "Jul","Aug","Sep",
                "Oct","Nov","Dec")
data$MONTH <- month_abbrs[data$MONTH_NUM]

# low data points for areas other than main 'TUCSON', only consider main 'TUCSON' area
table(data$AREA)
data <-data[data$AREA == 'TUCSON',]

# More cleanup, importantly the classification.csv replaces any , with _ , need for lookup
data <- data[!is.na(data$ZIP),]
data <- data[data$YEAR == 2020,]

# Consider only operational districts with a significant count of incidents
table(data$OPS_DIVISION) #ops div '' and ops div 8
data <- data[(data$OPS_DIVISION != '') & (data$OPS_DIVISION != '8'),]

# Fix an erroneous zip code,process zip code df, append population by zip code
# unique(data$ZIP)
data$ZIP[data$ZIP==86713] <- 85713
data <-merge(data,zip_pop,by="ZIP")
data <- data[!is.na(data$POPULATION),]
# Don't consider zip codes with a population less than 1000, a reasonable value
data <- data[data$POPULATION >=1000,]

# Snag the parent crime type for our cluster analysis
data$OFFENSE <- sapply(strsplit(data$DESC,"/"), `[`, 1)

# Consider types a reasonable person would denote as a notable, serious, unambiguous criminal event
ignore_types <- c("MENTAL CASES","PUBLIC ASSIST","MISCELLANEOUS","RUNAWAY JUVENILE","SICK CARED FOR","FOUND","OTHER OFFENSES",
                  "CIVIL MATTER","FOUND ANIMAL","TRAFFIC ACCIDENT","TRAFFIC & MOTOR VEHC LAWS","OTHER VEHICLE ACCIDENTS","PERSONAL INJURY ACCIDENTS",
                  "DISASTER","RED TAG ISSUED","FALSE ALARM","UNFOUNDED","SUD","LOST","DEATH","FIRE","SUBSTANCE USE DEFLECTION","VAGRANCY","NON-TRAFFIC ACCIDENT",
                  "SUSPICIOUS ACTIVITY","PUBLIC HAZARD", "ANIMAL BITES")
data <- subset(data, !(data$OFFENSE %in% ignore_types))

# cleanup some inconsistent labeling, merge some labeling
# unique(data$OFFENSE)
data$OFFENSE[data$OFFENSE == "OFFENSES AGNST FAMILY & CHLDRN"] <- "OFFENSES AGAINST FAMILY & CHILDREN"
data$OFFENSE[data$OFFENSE == "ASSAULT, AGGRAVATED"] <- "ASSAULT"

data_proc <- data
data_proc$ZIP <-as.factor(data_proc$ZIP)



######EXPLORATION######

#Lets see which crimes are most prevalent across all of Tucson
offense_grp <-group_by(data_proc,OFFENSE)
offense_cnts <- summarise(offense_grp,COUNT=n())
offense_cnts <- offense_cnts[order(offense_cnts$COUNT,decreasing = TRUE),]
off_cnts_plot <-ggplot(data=offense_cnts,aes(x=OFFENSE,y=COUNT)) + geom_bar(stat="identity",fill="red",color="black") + coord_flip() +
                ggtitle('Count of All Notable Offense Class Occurences in Tucson,AZ (2020)') +
                xlab('Offense Class') + ylab('Count') + 
                theme(plot.title = element_text(size = 18),
                axis.title = element_text(size = 15))
                
off_cnts_plot
# There are 8 noticable crime types with high occurence across all of Tucson.
# Clustering will be over-complicated if all crimes are considered, so consider just these types
off_to_consider <- offense_cnts[1:8,]

top_off_cnts_plot <-ggplot(data=off_to_consider,aes(x=OFFENSE,y=COUNT)) + geom_bar(stat="identity",fill="red",color="black") + coord_flip() +
  ggtitle('Count of Top 8 Notable Offense Class Occurences in Tucson,AZ (2020)') +
  xlab('Offense Class') + ylab('Count') + 
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 15))

top_off_cnts_plot

#Plot shares of top 8 crime types responded to by operational district
top_8_nms = off_to_consider$OFFENSE
data_top_8 <- data_proc[data_proc$OFFENSE %in% top_8_nms,]
by_ops_group <- group_by(data_top_8,OPS_DIVISION)
by_ops <- summarise(by_ops_group,COUNT=n())
num_crimes <- sum(by_ops$COUNT)
by_ops$PERC <- (by_ops$COUNT/num_crimes)*100

#calculate label pos for pie chart
by_ops <- data.frame(by_ops)
by_ops <- by_ops %>%
  arrange(desc(OPS_DIVISION)) %>%
  mutate(lab_ypos = cumsum(PERC) - 0.5*PERC)

#plot
by_ops$PERC <- round(by_ops$PERC,digit=2)
by_ops_plot <- ggplot(by_ops, aes(x = 2, y = PERC, fill = OPS_DIVISION)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab_ypos, label = PERC), color = "white")+
  theme_void()+
  xlim(0.5, 2.5) +
  ggtitle("Percentage of Top 8 Offense Classes Encountered by TPD Operations Division (2020)")+
  guides(fill=guide_legend(title="TPD Operations Division"))+ 
  labs(caption = "Top 8 Classes: Larceny,Disorderly Conduct,Assault,Criminal Damage,Narcotic Drug Laws","FRAUD","GTA","BURGLARY")
by_ops_plot

# Plot shares of top 8 crime classes by ZIP code
by_zip_group <- group_by(data_top_8,ZIP)
by_zip <- summarise(by_zip_group,COUNT=n())

#plot
zip_cnt_plot <-ggplot(data=by_zip,aes(x=ZIP,y=COUNT)) + geom_bar(stat="identity",fill="blue",color="black") + coord_flip() +
  ggtitle('Count of Top 8 Notable Offense Classes by Zip Code (2020)') +
  xlab('ZIP Code') + ylab('Count') + 
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 15))+ 
  labs(caption = "Top 8 Classes: Larceny,Disorderly Conduct,Assault,Criminal Damage,Narcotic Drug Laws","FRAUD","GTA","BURGLARY",)
zip_cnt_plot

# Plot count and type of crimes vs month crime was committed across ALL of Tucson,
# in a geom_tile plot
by_mnth_off_grp <- group_by(data_top_8,MONTH,OFFENSE)
by_mnth_off <- summarise(by_mnth_off_grp,COUNT=n())
by_mnth_off$MONTH <- ordered(by_mnth_off$MONTH,levels=month_abbrs)
by_mnth_plot <- ggplot(by_mnth_off, aes(MONTH, OFFENSE, fill = COUNT)) +
  geom_tile(color = "black")+
  scale_fill_distiller(palette = 3, direction = 1) +
  geom_text(aes(label=COUNT)) +
  ggtitle('Count of Top 8 Notable Offense Classes by Month (2020)') +
  xlab('Month') + ylab('Offense Class') +
  theme(plot.title = element_text(size = 18), 
        axis.title = element_text(size = 15))
by_mnth_plot

# Cluster Analysis
cls_grp <- group_by(data_top_8,ZIP,OFFENSE)
sum_grp <- summarise(cls_grp,COUNT=n())
# widen 
sum_grp <- spread(sum_grp,key=OFFENSE,value = COUNT)
# replace NA with count of 0, no incidence
sum_grp[is.na(sum_grp)] <- 0
drop <- c("ZIP")
sum_grp_num <- sum_grp[, !names(sum_grp) %in% drop]
# Do the scaling
sum_grp_num_scl = as.matrix(scale(sum_grp_num))

# Elbow method to get a sense for clusters
set.seed(123)
k.max <- 15
wss <- sapply(1:k.max,function(k){kmeans(sum_grp_num_scl, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#investigating 2 clusters, per elbow recommendation
model <- kmeans(sum_grp_num_scl,2)
df <- data.frame(sum_grp_num_scl, model$cluster)
clus_2_plot <- clusplot(df, model$cluster, color=TRUE, shade=F, labels=0, lines=0, main='2 Clusters')
clus_2_plot

# Using 4 clusters to address those 'outlier' clusters for lack of a better term
model <- kmeans(sum_grp_num_scl,4)
df <- data.frame(sum_grp_num_scl, model$cluster)
clus_4_plot <- clusplot(df, model$cluster, color=TRUE, shade=F, labels=0, lines=0, main='4 Cluster')
clus_4_plot

# Assign each zip its cluster :), finally
final <- data.frame(sum_grp_num_scl)
final$GROUP <- as.factor(model$cluster)
final$ZIP <- as.factor(sum_grp$ZIP)

agg = aggregate(final[,1:8],list(final$GROUP),mean)

# Clusters 2 and 4 have negative indicators across the board. These Could be
# considered low crime areas.Cluster 3 is showing low variablity across crime class.
# Cluster 3 is showing the highest crime class indicators, with cluster 4 showing the least.
# Clusters 1/3 are the 'high crime' clusters. Cluster 2/4 are the 'low crime' clusters.
# 4 2 1 3 is the order from least to highest crime indicators across the board.

# Post mortem pairplot correlation quicklook
pairs_df <-sum_grp
pairs_df$ZIP <-NULL
pairs_plot <- pairs(pairs_df)
pairs_plot