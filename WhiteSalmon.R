#Load Libraries
#install.packages('dataRetrieval');install.packages ('ggplot2'); install.packages('EGRET'); install.packages('dplyr'); install.packages('magrittr'); install.packages('lubridate'); install.packages('trend')
library(dataRetrieval);library (ggplot2); library(EGRET); library(dplyr); library(magrittr); library(lubridate); library(trend)
#Definte the USGS gage Site to be downloaded
siteNo<-'14123500'
pcode = '00060'
scode="00003"
start.date = "1965-10-01"
end.date="2018-09-30"
#Load Data
ws<- readNWISdv(siteNumbers = siteNo, parameterCd = pcode, statCd = scode, startDate=start.date, endDate=end.date)
#Rename columns
ws <- renameNWISColumns(ws);colnames(ws)

#Separate Date into Years/Months
ws$Year <- year(ws$Date); ws$Month <- month(ws$Date)
ws$YFS <- (ws$Year-min(ws$Year))
#Create WaterYear
ws$WaterYear <- ifelse(ws$Month>=10,ws$Year+1,ws$Year)
ws$WaterYFS <- (ws$WaterYear-min(ws$WaterYear))
###SOMETHING IN THIS GROUPING IS CAUSING PROBLEMS##
month.flow <- ws %>% 
  group_by(WaterYFS, Month) %>% 
  summarise(Total_cfs = sum(Flow, na.rm=T), n=n()) %>%  round(3)
month.flow <- as.data.frame(month.flow); 
annual.flow <- ws %>%
  group_by(WaterYFS) %>%
  summarise(Total_cfs = sum(Flow, na.rm=T), n=n()) %>%  round(3)
annual.flow <- as.data.frame(annual.flow);  
#remove those missing more than 10%
annual.flow <- subset(annual.flow, n>=330)


##descriptive stats
#install.packages("pastecs")
library(pastecs)
stat.desc(month.flow)

#Serial correlation check
#Annual
par(mfrow=c(2,2))
acf(annual.flow$Total_cfs, main = "Annual Autocorrelation Function")
pacf(annual.flow$Total_cfs, main = "Annual Parial Correlation Function")
#Monthly
acf(month.flow$Total_cfs, main = "Monthly Autocorrelation Function")
pacf(month.flow$Total_cfs, main = "Monthly Partial Autocorrelation Function")

################################################################################################################
################################################################################################################
##compare standards regression to MK regression 


#Linear Regression
linear = lm(Total_cfs ~ WaterYFS , data = annual.flow);  summary(linear)
x.est <- as.data.frame(seq(0,52,1)); colnames(x.est)<-"WaterYFS"
y.est <- as.data.frame(predict(linear,x.est, interval="confidence"))
###############
#MK
##############
temp.ts<-ts(annual.flow$Total_cfs, start=min(annual.flow$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#_#_#_#_#_#_#_#
# It looks like the code below is deprecated: b.sen and intercept no longer exist
# So, when it tries to call them to calculate the estimate and confidence limits, it gets null values.

# I think, looking at the documentation, that:
# sen$b.sen = sen$estimates (since this gives you the estimate of Sen's slope, which is b)
# b.sen.up and b.sen.lo I think are similar to sen$conf.int -- NB: this assumes a conf. level of 0.95!
# sen.intercept: looking online, it looks like you need to use the zyp package to get this

#sen$b.sen*sen$nobs;
#sen$b.sen.up*sen$nobs;  sen$b.sen.lo*sen$nobs
#sen$intercept

sen$estimates*sen$parameter;
sen$conf.int[1]*sen$parameter; sen$conf.int[2]*sen$parameter;

##### Several other packages to calculate the intercept
##### Both mblm and zyp are interchangeable, and give the same result when I tested them both
##### It looks like zyp is easier in some ways, so I stuck with that

#install.packages('mblm')
#library(mblm)
#install.packages('zyp')
library(zyp)
zsen <- zyp.sen(linear,annual.flow)
#zsen$intercepts  #give list of ALL intercepts
#median(zsen$slopes) #gives same value as sen$estimates
zsen$coefficients # gives both Intercept and slope estimate (as WaterYFS)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


# create lines to plot on graph
# y-axis. We are using the equation y=a+b*x, where a is the intercept and b is the slope

estimate<-round((senint+senb*seq(0,dim(annual.flow)[1]-1,1)),1)
#upper confidence interval
upperc1<-round((senint+senhi*seq(0,dim(annual.flow)[1]-1,1)),1)
#lower confidence interval
lowerc1<-round((senint+senlo*seq(0,dim(annual.flow)[1]-1,1)),1)

#Plot Annual Flow over time
par(mar = c(5,5,3,5)) #set plot margins
plot(annual.flow$WaterYFS, annual.flow$Total_cfs, type='n', yaxt="n", 
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
box()
#add data to the plot
points(annual.flow$WaterYFS, annual.flow$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(annual.flow$WaterYFS, annual.flow$Total_cfs, col=rgb(0,0,0,0.8))  

#Add linear regression to plot
lines(x.est$WaterYFS, y.est$fit, col="red", lty=2);
lines(lowess(annual.flow$WaterYFS, annual.flow$Total_cfs), col="blue", lty=2)

#draw a polygon between the lower and upper confidence interval  
polygon(c(rev(annual.flow$WaterYFS), (annual.flow$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(annual.flow$WaterYFS, estimate, lty=2, col=rgb(0.5,1,0.5,1))
legend("bottomleft",c("Annual Flow", "LOWESS", "Linear Regression", "MK regression", "MK confidence interval"), col=c("black","blue","red",rgb(0.5,1,0.5,1),rgb(0,0,0.2)), pch=c(19,NA,NA,NA,22), pt.cex=c(0.8,1,1,1,2), lty=c(1,2,2,2,NA), pt.bg=c(NA,NA,NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)
title("Various Trendlines 1966-2018 Water Years")


##################################################################################
#PRE AND POST DAM REMOVAL ALL DATA
par(mar = c(5,5,3,5)) #set plot margins
plot(annual.flow$WaterYFS, annual.flow$Total_cfs, type='n', yaxt="n", ylim=c(0,max(annual.flow$Total_cfs, na.rm=TRUE)*1.5),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal")
legend("bottomleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)


#add the data
points(annual.flow$WaterYFS, annual.flow$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(annual.flow$WaterYFS, annual.flow$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,0,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(annual.flow, WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(linear,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(annual.flow, WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(linear,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))


##################################################################################################
###### MONTHLY COMPARISONS (no pre and post)
month.stats <- as.data.frame(matrix(nrow=0,ncol=5)); 
#create meaningful column names
colnames(month.stats) <- c("Month", "Pval","Intercept","Slope","Nobs");

#set up plots frame
par(mfrow=c(3,4))     #3 rows and 4 columns
par(mar = c(2,4,2,1)) #set plot margins (how much white space around each plot) (bottom, left, top right)

#loop through each month  
for (i in 1:12) {
  zt <- subset(month.flow, Month==i)
  
  #create time series
  temp.ts<-ts(zt$Total_cfs, start=min(zt$WaterYFS, freq=1));  
  #mk 
  mk <- mk.test(temp.ts); mk
  #sens  
  sen <- sens.slope(temp.ts); sen                        
  
  #Rewrote the formula for zyp, since the column names changed...
  monthLinear <- lm(formula = Total_cfs ~ WaterYFS, data = zt)
  zsen <- zyp.sen(monthLinear,zt)
  senint <- zsen$coefficients[1]
  senb <- zsen$coefficients[2]
  senhi <- sen$conf.int[2]
  senlo <- sen$conf.int[1]
  
  
  #fill dataframe so months can be directly compared
  month.stats[i,1] <- i;  month.stats[i,2]<-mk$pvalg;   month.stats[i,3]<-senint
  month.stats[i,4] <- senb;     month.stats[i,5]<-sen$parameter;
  
  # create lines to plot on graph
  estimate<-round((senint+senb*seq(0,dim(zt)[1]-1,1)),1)
  upperc1<-round((senint+senhi*seq(0,dim(zt)[1]-1,1)),1)
  lowerc1<-round((senint+senlo*seq(0,dim(zt)[1]-1,1)),1)
  
  #plot each month:
  plot(zt$WaterYFS, zt$Total_cfs, type='n', yaxt="n", main=i, ylim=c(0,125000), ylab="", xlab = '', cex.lab=0.9)
  axis(2, las=2, cex.axis=0.8)
  points(zt$WaterYFS, zt$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
  lines(zt$WaterYFS, zt$Total_cfs, col=rgb(0,0,0,0.2))  
  polygon(c(rev(zt$WaterYFS), (zt$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
  lines(zt$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))
}

#####################################################################################################
#####################################################################################################
############################                                #########################################
############################    MONTHLY PRE/POST TESTS      #########################################
############################        and Sample Plots        #########################################
############################                                #########################################
#####################################################################################################

#############OCTOBER
#create the plot
par(mar = c(5,5,3,5)) #set plot margins
oct<-subset(month.flow, Month==10)
plot(oct$WaterYFS, oct$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal OCTOBER")
legend("bottomleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(oct$WaterYFS, oct$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(oct$WaterYFS, oct$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,0,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==10 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==10 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   November
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==11 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==11 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   December
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==12 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==12 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen


#############################january

#create the plot
par(mar = c(5,5,3,5)) #set plot margins
jan<-subset(month.flow, Month==1)
plot(jan$WaterYFS, jan$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal JANUARY")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(jan$WaterYFS, jan$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(jan$WaterYFS, jan$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,0,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==1 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==1 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   February
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==2 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==2 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   March
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==3 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==3 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen



#####APRIL
#create the plot
par(mar = c(5,5,3,5)) #set plot margins
apr<-subset(month.flow, Month==4)
plot(apr$WaterYFS, apr$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal APRIL")
legend("bottomleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(apr$WaterYFS, apr$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(apr$WaterYFS, apr$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,0,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==4 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==4 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   May
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==5 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==5 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   June
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==6 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==6 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   July

#create the plot
par(mar = c(5,5,3,5)) #set plot margins
jul<-subset(month.flow, Month==7)
plot(jul$WaterYFS, jul$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal JULY")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(jul$WaterYFS, jul$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(jul$WaterYFS, jul$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,0,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==7 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==7 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################  August
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==8 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==8 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################    September
par(mar = c(5,5,3,5))
sep<-subset(month.flow, Month==9)
plot(sep$WaterYFS, sep$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 1966 46 = 2012 52 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression Pre and Post Dam Removal SEPTEMBER")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(sep$WaterYFS, sep$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(sep$WaterYFS, sep$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(45,-10000,46,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==9 & WaterYFS<46)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==9 & WaterYFS>=46)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-45)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+45), (post.DR$WaterYFS+45)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+45, estimate, lty=2, col=rgb(0,0,.2,1))

######################################################################################################
######################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FLOOD RETURN INTERVAL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################################################################################################
peak.flow <- ws %>%
  group_by(WaterYFS) %>%
  summarise(Peak_cfs = max(Flow, na.rm=T), n=n()) %>%  round(3)
peak.flow <- as.data.frame(peak.flow); peak.flow  
#remove rows missing more than 10% of data
peak.flow <- subset(peak.flow, n>=(365-365*.1))
#rank flows
peak.flow <- arrange(peak.flow, desc(Peak_cfs)); peak.flow[1:5,]
peak.flow$Rank <- rank(-peak.flow$Peak_cfs); peak.flow[1:5,] 
n.years <- dim(peak.flow)[1]; n.years
peak.flow$ReturnInterval <- (n.years+1)/peak.flow$Rank; peak.flow[1:5,]
peak.flow$AnnualProb <- round(1/peak.flow$ReturnInterval*100,3);  peak.flow[1:5,]
#Plot the return interval with the peak flow
par(mfrow=c(1,1))     #one graph per plot
par(mar = c(5,5,3,5)) #set plot margins
plot(peak.flow$ReturnInterval, peak.flow$Peak_cfs, log="x", type='n', yaxt="n", xlim=c(1,1000), ylim=c(0,20000),
     ylab="Peak Streamflow (cfs)", xlab = 'Return Interval (Years)')
axis(2, las=2, cex.axis=0.9)
#create minor tick marks
minor.ticks <- c(2,3,4,6,7,8,9,20,30,40,60,70,80,90,200,300,400,600,700,800,900)    
#add minor tick marks to x-ais
axis(1,at=minor.ticks,labels=FALSE, col="darkgray")                                
box() #draw a box around the plot
#add points to the plot
points(peak.flow$ReturnInterval, peak.flow$Peak_cfs, col=rgb(0,0,0,0.6), cex=1.2, pch=19)  #add points to plot

#determine whether a linear or log regression has teh best fit...
RI.linear = lm(Peak_cfs ~ ReturnInterval , data = peak.flow); RI.linear
summary(RI.linear)
RI.log = lm(Peak_cfs ~ log(ReturnInterval), data=peak.flow)
summary(RI.log)
#Log regression had the best fit
#Estimate the streamflow at the following return intervals using the log regression
x.est <- as.data.frame(c(100,200,500,1000)); colnames(x.est)<-"ReturnInterval"
y.est <- predict(RI.log,x.est, interval="confidence")
y.est <- as.data.frame(y.est)
y100 = cbind(x.est, y.est);  y100 <- subset(y100, x.est==100)$fit
y100

#Add to plot
points(x.est$ReturnInterval, y.est$fit, col="red", pch=2, lwd=2);
legend("bottomright", c("Observed","Log Estimated"), pch=c(19,2), col=c("darkgray","red"))

#################create the function###############

#create the flood_int function that reads in the data parameter
flood_int = function(data){ 
  #Calculate the maximum annual flow by water year
  peak.flow <- data %>%
    group_by(WaterYFS) %>%
    summarise(Peak_cfs = max(Flow, na.rm=T), n=n()) %>%  round(3)
  peak.flow <- as.data.frame(peak.flow); 
  #remove rows missing more than 10% of data
  peak.flow <- subset(peak.flow, n>=(365-365*.1))
  
  #rank flows
  peak.flow <- arrange(peak.flow, desc(Peak_cfs)); peak.flow[1:5,]
  peak.flow$Rank <- rank(-peak.flow$Peak_cfs); peak.flow[1:5,]
  
  #calculate teh return interval
  n.years <- dim(peak.flow)[1]; n.years
  peak.flow$ReturnInterval <- (n.years+1)/peak.flow$Rank; peak.flow[1:5,]
  peak.flow$AnnualProb <- round(1/peak.flow$ReturnInterval*100,3);  peak.flow[1:5,]
  
  #return the data frame so it can be used outside the function
  return (peak.flow)
}
########### Call for the early period################
ws.early <- subset(ws, Date>="1965-10-01" & Date<="2011-10-25");         # summary(ws.early)
peak.flow.early <- flood_int(ws.early)   ;
summary(peak.flow.early)  

#Basic plot
par(mar = c(5,5,3,5)) #set plot margins
plot(peak.flow.early$ReturnInterval, peak.flow.early$Peak_cfs, log="x", type='n', yaxt="n", xlim=c(1,1000), ylim=c(0,20000),
     ylab="Peak Streamflow (cfs)", xlab = 'Return Interval (Years)')
axis(2, las=2, cex.axis=0.9)
axis(1,at=minor.ticks,labels=FALSE, col="darkgray")                                 
box()

#plot original data 
points(peak.flow$ReturnInterval, peak.flow$Peak_cfs, col=rgb(0,0,1,0.5), cex=0.8, pch=19)    
#plot early period data
points(peak.flow.early$ReturnInterval, peak.flow.early$Peak_cfs, col="red", cex=1.0, pch=19)  

#linear regression
RI.linear.early = lm(Peak_cfs ~ ReturnInterval , data = peak.flow.early);
summary(RI.linear.early)
RI.log.early = lm(Peak_cfs ~ log(ReturnInterval), data=peak.flow.early)
summary(RI.log.early) 

#plot log line
x.est <- as.data.frame(c(100,200,500,1000)); colnames(x.est)<-"ReturnInterval"
y.est.pre <- predict(RI.log.early,x.est, interval="confidence")
y.est.pre <- as.data.frame(y.est.pre)

#Add regression ponits
points(x.est$ReturnInterval, y.est$fit, col="black", pch=5, lwd=2);  #original POR
points(x.est$ReturnInterval, y.est.pre$fit, col="blue", pch=2, lwd=2);  #early 
points(x.est$ReturnInterval, y.est.post$fit, col="goldenrod3", pch=12, lwd=2); #late

#add straight lines at points of interest
abline(h=y.est.pre$fit[1], col="black", lty=3); abline(v=100, col="red", lty=3)
abline(h=y100, col="blue", lty=3)

#add a legend
legend("bottomright", c("Period of Record","1965-2011 data", "Est. Flow POR", "Est.Flow 1965-2011", "Est.Flow 2011-2018"), 
       col=c("blue","red","blue","red", "goldenrod3"), pch=c(19,19,2,5,12))
############LATE PERIOD############
ws.late <- subset(ws, Date>="2011-10-26");         # summary(ws.late)
peak.flow.late <- flood_int(ws.late)   ;
summary(peak.flow.late)  

#Basic plot
par(mar = c(5,5,3,5)) #set plot margins
plot(peak.flow.late$ReturnInterval, peak.flow.late$Peak_cfs, log="x", type='n', yaxt="n", xlim=c(1,1000), ylim=c(0,20000),
     ylab="Peak Streamflow (cfs)", xlab = 'Return Interval (Years)')
axis(2, las=2, cex.axis=0.9)
axis(1,at=minor.ticks,labels=FALSE, col="darkgray")                                 
box()

#plot original data
points(peak.flow$ReturnInterval, peak.flow$Peak_cfs, col=rgb(0,0,1,0.5), cex=0.8, pch=19)  
#plot late data
points(peak.flow.late$ReturnInterval, peak.flow.late$Peak_cfs, col=rgb(0.7,0.4,0,0.6), cex=1.2, pch=19)  

#linear regression on post 1984
RI.linear.late = lm(Peak_cfs ~ ReturnInterval , data = peak.flow.late);
summary(RI.linear.late)
RI.log.late = lm(Peak_cfs ~ log(ReturnInterval), data=peak.flow.late)
summary(RI.log.late) 

#plot regression log line
y.est.post <- predict(RI.log.late,x.est, interval="confidence")
y.est.post <- as.data.frame(y.est.post)
points(x.est$ReturnInterval, y.est.post$fit, col="goldenrod3", pch=12, lwd=2);
#plot original regression
points(x.est$ReturnInterval, y.est$fit, col="blue", pch=2, lwd=2);
#plot early return interval
points(x.est$ReturnInterval, y.est.pre$fit, col="red", pch=5, lwd=2);

#draw ablines
abline(h=c(y100,y.est.pre$fit[1],y.est.post$fit[1]), col=c("blue","red","goldenrod3"), lty=3);
abline(v=100, col="black", lty=3)

legend("bottomright", c("Period of Record","2011-2018 data", "Est. Flow POR", "Est.Flow 1965-2011", "Est.Flow 2011-2018"), 
       col=c("blue","goldenrod3","blue","red", "goldenrod3"), pch=c(19,19,2,5,12))

########### Flood BACI Plot
par(mar = c(5,5,3,5)) #set plot margins
plot(peak.flow.early$ReturnInterval, peak.flow.early$Peak_cfs, log="x", type='n', yaxt="n", xlim=c(1,1000), ylim=c(0,20000),
     ylab="Peak Streamflow (cfs)", xlab = 'Return Interval (Years)')
axis(2, las=2, cex.axis=0.9)
axis(1,at=minor.ticks,labels=FALSE, col="darkgray")                                 
box()

#plot late data 
points(peak.flow.late$ReturnInterval, peak.flow.late$Peak_cfs, col=rgb(0.7,0.4,0,0.6), cex=1.2, pch=19)   
#plot early period data
points(peak.flow.early$ReturnInterval, peak.flow.early$Peak_cfs, col="red", cex=1.0, pch=19)  


#Add regression ponits
points(x.est$ReturnInterval, y.est$fit, col="blue", pch=5, lwd=2);  #original POR
points(x.est$ReturnInterval, y.est.pre$fit, col="red", pch=2, lwd=2);  #early 
points(x.est$ReturnInterval, y.est.post$fit, col="goldenrod3", pch=12, lwd=2); #late


#add straight lines at points of interest
abline(h=c(y100,y.est.pre$fit[1],y.est.post$fit[1]), col=c("blue","red","goldenrod3"), lty=3);
abline(v=100, col="black", lty=3)

#add a legend
legend("bottomright", c("1965-2011 data", "2011-2018 data", "Est.Flow 1965-2011", "Est.Flow 2011-2018", "Est POR"), 
       col=c("red","goldenrod3","red", "goldenrod3","blue"), pch=c(19,19,2,5,12))

##########~~~~~~~~~~~~~RI TABLE~~~~~~~~~~~~~~~~##############

#create data frame
RI.table <- as.data.frame(matrix(nrow=3, ncol=6));    
#give column names
colnames(RI.table) <- c("Date.Range", "RI_100yr","RI_500yr","RI_1000yr","Nyears","AdjustedR2")

#fill in columns
RI.table$Date.Range <- c("1965-2018","1965-2011","2011-2018")
RI.table$RI_100yr <- c(y.est$fit[1],y.est.pre$fit[1],y.est.post$fit[1])
RI.table$RI_500yr <- c(y.est$fit[3],y.est.pre$fit[3],y.est.post$fit[3])
RI.table$RI_1000yr <- c(y.est$fit[4],y.est.pre$fit[4],y.est.post$fit[4])
RI.table$Nyears <- c(dim(peak.flow)[1], dim(peak.flow.early)[1], dim(peak.flow.late)[1])
RI.table$AdjustedR2 <- c(summary(RI.log)$adj.r.squared, summary(RI.log.early)$adj.r.squared, 
                         summary(RI.log.late)$adj.r.squared)
#view table
RI.table

#######~~~~~~~~~~~~PROBABILITY OF 30 YR Return~~~~~~~~~~~~~~##############
#What's the probability of these events occurring in a 30 year mortgage??
Rperiod = c(100,500,1000)
n.years = 30
for (i in 1:3){
  print(paste0("Percent likelihood over ", n.years, " years for a ", Rperiod[i]," year flood: ", round((1-(1-(1/Rperiod[i]))^n.years)*100,2), "%"))
}
########################################################################################################
########################################################################################################
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~7q10~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################
########################################################################################################

##install.packages("TTR")
library(TTR)
##Low Flow Return Interval
ws$Q7 <- SMA(ws$Flow,7) #the first 7 observations are not included
summary(ws)  

#include year and month variables
ws$Year <- year(ws$Date);  ws$Month <- month(ws$Date)

#Calculate the minimum 7 day average in each year
low.flow <- ws %>%
  group_by(Year) %>%
  summarise(MinQ7 = min(Q7, na.rm=T), n=n()) %>%  round(3)
low.flow <- as.data.frame(low.flow);  
#remove rows missing more than 10% of data
low.flow <- subset(low.flow, n>=(365-365*.1))

#rank flows - notice the rank is now in ascending order
low.flow <- arrange(low.flow, (MinQ7)); low.flow[1:5,]
low.flow$Rank <- rank(low.flow$MinQ7); low.flow[1:5,]

#calculate return interval
n.years.por <- n.years <- dim(low.flow)[1]; #n.years
low.flow$ReturnInterval <- (n.years+1)/low.flow$Rank; low.flow[1:5,]
low.flow$AnnualProb <- round(1/low.flow$ReturnInterval*100,3);  low.flow[1:5,]

#plot the data
par(mar = c(5,5,3,5)) #set plot margins
plot(low.flow$AnnualProb, low.flow$MinQ7, type='n', yaxt="n", xlim=c(1,100),
     ylab="Min Q7 Streamflow (cfs)", xlab = 'Probability of smaller flows')
axis(2, las=2, cex.axis=0.9)
points(low.flow$AnnualProb, low.flow$MinQ7, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
abline(v=10, lty=4, col="black")

#linear regression
linear = lm(MinQ7 ~ AnnualProb , data = low.flow);
summary(linear)
#exponential regression
exp.lm = lm(log(MinQ7) ~ (AnnualProb), data=low.flow)
summary(exp.lm) 

#linear is pretty close
x.est <- as.data.frame(seq(0,100,10)); colnames(x.est)<-"AnnualProb"
y.est <- predict(linear,x.est, interval="confidence")
y.est <- as.data.frame(y.est)
y.est.exp <- as.data.frame(exp(predict(exp.lm,x.est, interval="confidence")))

#Add to plot
lines(x.est$AnnualProb, y.est$fit, col="red", lty=3, lwd=2);
lines(x.est$AnnualProb, y.est.exp$fit, col="darkgreen", lty=5, lwd=2)

#What is the 7Q10 low flow value?
low.7Q10 <- predict(linear,filter(x.est,AnnualProb==10),interval="confidence"); low.7Q10
abline(h=low.7Q10[1], col="black", lty=2, lwd=2)

#####    WHEN WERE THE LOW FLOW DAYS

low.days <- subset(ws, Flow <= low.7Q10[1]); dim(low.days)
n.years <- length(unique(low.days$Year))
print(paste0("Probability of occurrence: ", round(n.years/length(unique(ws$Year))*100,2)))

#plot low flow days
plot(ws$Date, ws$Flow, type='n', yaxt="n", ylim=c(300,1000),
     ylab="Streamflow (cfs)", xlab = '')
axis(2, las=2, cex.axis=0.9)
lines(ws$Date, ws$Flow, lwd=1, col=rgb(0,0,1,0.3))

points(low.days$Date, low.days$Flow, col=rgb(1,0,0,0.8), pch=19)  
abline(v=c(as.Date("2011-10-26"), as.Date("2011-11-26")), lty=2, col="black", lwd=3)
abline(h=low.7Q10, col="red", lty=4)

###########~~~~~~~~~~~Use Function to Calculate Low Flow RI

Fun_7q10 = function(data, ndays){   
  data$Q7 <- SMA(data$Flow, ndays) #the first 7 observations are not included
  
  #For each year, calculate the minimum Q7
  data$Year <- year(data$Date);  data$Month <- month(data$Date)
  
  #Maximum Annual Flow
  low.flow <- data %>%
    group_by(Year) %>%
    summarise(MinQ7 = min(Q7, na.rm=T), n=n()) %>%  round(3)
  
  low.flow <- as.data.frame(low.flow);  
  #remove rows missing more than 10% of data
  low.flow <- subset(low.flow, n>=(365-365*.1))
  
  #rank flows
  low.flow <- arrange(low.flow, (MinQ7)); low.flow[1:5,]
  low.flow$Rank <- rank(low.flow$MinQ7); low.flow[1:5,]
  
  n.years <- dim(low.flow)[1]; n.years
  low.flow$ReturnInterval <- (n.years+1)/low.flow$Rank; low.flow[1:5,]
  low.flow$AnnualProb <- round(1/low.flow$ReturnInterval*100,3);  low.flow[1:5,]
  
  return (low.flow)
} 
############Post Dam Removal
#subset data
post.dam <- subset(ws, Date>="2011-10-26")
#call function
low.flow.post <- Fun_7q10(post.dam, 7)

#Plot data
par(mar = c(5,5,3,5)) #set plot margins
plot(low.flow.post$AnnualProb, low.flow.post$MinQ7, type='n', yaxt="n", xlim=c(1,100), ylim=c(300,1000),
     ylab="Min Q7 Streamflow (cfs)", xlab = 'Annual Probability of Exceedance')
axis(2, las=2, cex.axis=0.9)
points(low.flow.post$AnnualProb, low.flow.post$MinQ7, col=rgb(0.7,0.5,0,0.8), cex=1, pch=19)  
abline(v=10, lty=4, col="black")

#linear regression
linear.post = lm(MinQ7 ~ AnnualProb , data = low.flow.post);
summary(linear.post)
#exponential regression
exp.lm.post = lm(log(MinQ7) ~ (AnnualProb), data=low.flow.post)
summary(exp.lm.post) 

#r2 is similar - you can choose either one
y.est.post <- predict(linear.post,x.est, interval="confidence")
y.est.post <- as.data.frame(y.est.post)
y.est.exp.post <- as.data.frame(exp(predict(exp.lm.post,x.est, interval="confidence")))

#Add to plot
#lines(x.est$AnnualProb, y.est.post$fit, col="red", lty=3,lwd=2);
#lines(x.est$AnnualProb, y.est.exp.post$fit, col="darkgreen", pch=4)

#What is the 7Q10 low flow value?
low.7Q10.post <- predict(linear.post,filter(x.est,AnnualProb==10),interval="confidence"); low.7Q10.post
abline(h=low.7Q10.post[1], col=rgb(0.7,0.5,0), lty=2, lwd=2)
#abline(h=low.7Q10.post, col="black", lty=4)

#add original low flow value
abline(h=low.7Q10[1], col="red", lty=4)
points(low.flow$AnnualProb, low.flow$MinQ7, col=rgb(1,0,0,0.8), cex=0.6, pch=19)  

legend("top", c("Post Dam Removal Annual Low Flow", "Post Dam Removal 7Q10", "POR Annual Low Flow", "POR 7Q10"), col=c("goldenrod3","goldenrod3","red","red"),
       pch=c(19,NA,19,NA), lty=c(0,2,0,4))

print(paste0("Min Flow increased by ", round((low.7Q10.post[1]-low.7Q10[1])/low.7Q10[1]*100,2), "%"))


###

#plot low flow days
plot(ws$Date, ws$Flow, type='n', yaxt="n", ylim=c(300,1000),
     ylab="Streamflow (cfs)", xlab = '')
axis(2, las=2, cex.axis=0.9)
lines(ws$Date, ws$Flow, lwd=1, col=rgb(0,0,1,0.3))

#subset data to only include low flow exceedances
low.days.post <- subset(ws, Flow <= low.7Q10.post[1]); dim(low.days.post)
#plot points and ablines
points(low.days.post$Date, low.days.post$Flow, col=rgb(1,0,0,0.6), pch=19, cex=0.8)  
abline(v=c(as.Date("2011-10-26"), as.Date("2011-11-26")), lty=2, col="black", lwd=3)
abline(h=low.7Q10.pre[1], col="red", lty=4)
points(low.days$Date, low.days$Flow, col=rgb(0.7,0,0,0.6), pch=19)  
abline(h=low.7Q10[1], col="darkred", lty=4)

####7Q10 RI TABLE

n.years.pre <- length(unique(low.days.pre$Year))
#create table
RI.table <- as.data.frame(matrix(nrow=2, ncol=5));    
#provide column names
colnames(RI.table) <- c("Date.Range", "7Q10_cfs","No_Years","Annual_Prob","AdjustedR2")
#fill columns with relevant data
RI.table$Date.Range <- c("1965-2018","2011-2018")
RI.table$`7Q10_cfs` <- c(round(low.7Q10[1],3), round(low.7Q10.pre[1],3))
RI.table$No_Years <- c(n.years.por, n.years.pre)
RI.table$Annual_Prob <- c(round(n.years.por/length(unique(ws$Year))*100,2), round(n.years.pre/length(unique(ws$Year))*100,2))
RI.table$AdjustedR2 <- c(summary(exp.lm)$adj.r.squared, summary(exp.lm.pre)$adj.r.squared)
#Look at the table
RI.table

#############PRE DAM REMOVAL
#subset data
pre.dam <- subset(ws, Date<"2011-10-26")
#call function
low.flow.pre <- Fun_7q10(pre.dam, 7)

#Plot data
par(mar = c(5,5,3,5)) #set plot margins
plot(low.flow.pre$AnnualProb, low.flow.pre$MinQ7, type='n', yaxt="n", xlim=c(1,100), ylim=c(300,1000),
     ylab="Min Q7 Streamflow (cfs)", xlab = 'Annual Probability of Exceedance')
axis(2, las=2, cex.axis=0.9)
points(low.flow.post$AnnualProb, low.flow.post$MinQ7, col=rgb(0.7,0.5,0,0.8), cex=1, pch=19)  
abline(v=10, lty=4, col="black")

#linear regression
linear.pre = lm(MinQ7 ~ AnnualProb , data = low.flow.pre);
summary(linear.pre)
#exponential regression
exp.lm.pre = lm(log(MinQ7) ~ (AnnualProb), data=low.flow.pre)
summary(exp.lm.pre) 

#r2 is similar - you can choose either one
y.est.pre <- predict(linear.pre,x.est, interval="confidence")
y.est.pre <- as.data.frame(y.est.pre)
y.est.exp.pre <- as.data.frame(exp(predict(exp.lm.pre,x.est, interval="confidence")))

#Add to plot
#lines(x.est$AnnualProb, y.est.pre$fit, col="red", lty=3,lwd=2);
#lines(x.est$AnnualProb, y.est.exp.pre$fit, col="darkgreen", pch=4)

#What is the 7Q10 low flow value?
low.7Q10.pre <- predict(linear.pre,filter(x.est,AnnualProb==10),interval="confidence"); low.7Q10.post
abline(h=low.7Q10.post[1], col=rgb(0.7,0.5,0), lty=2, lwd=2)
#abline(h=low.7Q10.pre, col="black", lty=4)

#add original low flow value
abline(h=low.7Q10.pre[1], col="red", lty=4)
points(low.flow.pre$AnnualProb, low.flow.pre$MinQ7, col=rgb(1,0,0,0.8), cex=0.6, pch=19)  

legend("top", c("Post Dam Removal Annual Low Flow", "Post Dam Removal 7Q10", "Pre Dam Removal Annual Low Flow", "Pre Dam Removal 7Q10"), col=c("goldenrod3","goldenrod3","red","red"),
       pch=c(19,NA,19,NA), lty=c(0,2,0,4))

print(paste0("Min Flow increased by ", round((low.7Q10.post[1]-low.7Q10.pre[1])/low.7Q10.pre[1]*100,2), "%"))


###

#plot low flow days
plot(ws$Date, ws$Flow, type='n', yaxt="n", ylim=c(300,1000),
     ylab="Streamflow (cfs)", xlab = '')
axis(2, las=2, cex.axis=0.9)
lines(ws$Date, ws$Flow, lwd=1, col=rgb(0,0,1,0.3))

#subset data to only include low flow exceedances
low.days.pre <- subset(ws, Flow <= low.7Q10.pre[1]); dim(low.days.pre)
#plot points and ablines
points(low.days.pre$Date, low.days.pre$Flow, col=rgb(1,0,0,0.6), pch=19, cex=0.8)  
abline(v=c(as.Date("2011-10-26"), as.Date("2011-11-26")), lty=2, col="black", lwd=3)
abline(h=low.7Q10.pre[1], col="red", lty=4)
points(low.days.post$Date, low.days.post$Flow, col=rgb(0.7,0,0,0.6), pch=19)  
abline(h=low.7Q10.post[1], col="darkred", lty=4)

####7Q10 RI TABLE

n.years.pre <- length(unique(low.days.pre$Year))
#create table
RI.table <- as.data.frame(matrix(nrow=2, ncol=5));    
#provide column names
colnames(RI.table) <- c("Date.Range", "7Q10_cfs","No_Years","Annual_Prob","AdjustedR2")
#fill columns with relevant data
RI.table$Date.Range <- c("1965-2018","2011-2018")
RI.table$`7Q10_cfs` <- c(round(low.7Q10[1],3), round(low.7Q10.pre[1],3))
RI.table$No_Years <- c(n.years.por, n.years.pre)
RI.table$Annual_Prob <- c(round(n.years.por/length(unique(ws$Year))*100,2), round(n.years.pre/length(unique(ws$Year))*100,2))
RI.table$AdjustedR2 <- c(summary(exp.lm)$adj.r.squared, summary(exp.lm.pre)$adj.r.squared)
#Look at the table
RI.table




#####################################################################################################
#####################################################################################################
############################           7 YEARS PRE          #########################################
############################    MONTHLY PRE/POST TESTS      #########################################
############################        and Sample Plots        #########################################
############################                                #########################################
#####################################################################################################
#Define the USGS gage Site to be downloaded
siteNo<-'14123500'
pcode = '00060'
scode="00003"
start.date = "2004-10-01"
end.date="2018-09-30"
#Load Data
ws<- readNWISdv(siteNumbers = siteNo, parameterCd = pcode, statCd = scode, startDate=start.date, endDate=end.date)
#Rename columns
ws <- renameNWISColumns(ws);colnames(ws)

#Separate Date into Years/Months
ws$Year <- year(ws$Date); ws$Month <- month(ws$Date)
ws$YFS <- (ws$Year-min(ws$Year))
#Create WaterYear
ws$WaterYear <- ifelse(ws$Month>=10,ws$Year+1,ws$Year)
ws$WaterYFS <- (ws$WaterYear-min(ws$WaterYear))
###SOMETHING IN THIS GROUPING IS CAUSING PROBLEMS##
month.flow <- ws %>% 
  group_by(WaterYFS, Month) %>% 
  summarise(Total_cfs = sum(Flow, na.rm=T), n=n()) %>%  round(3)
month.flow <- as.data.frame(month.flow); 
annual.flow <- ws %>%
  group_by(WaterYFS) %>%
  summarise(Total_cfs = sum(Flow, na.rm=T), n=n()) %>%  round(3)
annual.flow <- as.data.frame(annual.flow);  
#remove those missing more than 10%
annual.flow <- subset(annual.flow, n>=330)





#############OCTOBER


#create the plot
par(mar = c(5,5,3,5)) #set plot margins
oct<-subset(month.flow, Month==10)
plot(oct$WaterYFS, oct$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 2004 7 = 2012 13 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression 7 Years Pre and Post Dam Removal OCTOBER")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(oct$WaterYFS, oct$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(oct$WaterYFS, oct$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(6,0,7,1000000, col=rgb(0.7,0,0,0.2), border=NA)




#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==10 & WaterYFS<7)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==10 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+6), (post.DR$WaterYFS+6)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+6, estimate, lty=2, col=rgb(0,0,.2,1))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############################November
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==11 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==11 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################december
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==12 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==12 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#############################   january

#create the plot
par(mar = c(5,5,3,5)) #set plot margins
jan<-subset(month.flow, Month==1)
plot(jan$WaterYFS, jan$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 2004 7 = 2012 13 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression 7 Years Pre and Post Dam Removal JANUARY")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(jan$WaterYFS, jan$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(jan$WaterYFS, jan$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(6,-10000,7,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==1 & WaterYFS<7)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==1 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+6), (post.DR$WaterYFS+6)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+6, estimate, lty=2, col=rgb(0,0,.2,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##########################    February
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==2 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==2 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##################################    March
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==3 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==3 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
####################    APRIL
#create the plot
par(mar = c(5,5,3,5)) #set plot margins
apr<-subset(month.flow, Month==4)
plot(apr$WaterYFS, apr$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 2004 7 = 2012 13 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression 7 Years Pre and Post Dam Removal APRIL")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(apr$WaterYFS, apr$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(apr$WaterYFS, apr$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(6,0,7,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==4 & WaterYFS<7)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==4 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+6), (post.DR$WaterYFS+6)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+6, estimate, lty=2, col=rgb(0,0,.2,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################    May
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==5 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==5 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################    June
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==6 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==6 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   July

#create the plot
par(mar = c(5,5,3,5)) #set plot margins
jul<-subset(month.flow, Month==7)
plot(jul$WaterYFS, jul$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 2004 7 = 2012 13 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression 7 Years Pre and Post Dam Removal JULY")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(jul$WaterYFS, jul$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(jul$WaterYFS, jul$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(6,-10000,7,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==7 & WaterYFS<7)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==7 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+6), (post.DR$WaterYFS+6)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+6, estimate, lty=2, col=rgb(0,0,.2,1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   August
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==8 & WaterYFS<7)
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==8 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################   September
#create the plot
par(mar = c(5,5,3,5)) #set plot margins
sep<-subset(month.flow, Month==9)
plot(sep$WaterYFS, sep$Total_cfs, type='n', yaxt="n", ylim=c(0,125000),
     ylab="Total Streamflow (cfs)", xlab = 'Water Years Count (0 = 2004 7 = 2012 13 = 2018)', cex.lab=0.9)
axis(2, las=2, cex.axis=0.8)
title("Mann-Kendall Regression 7 Years Pre and Post Dam Removal SEPTEMBER")
legend("topleft",c("Annual Flow","MK regression", "MK confidence interval"), col=c("black","black",rgb(0,0,0.2)), pch=c(19,NA,22), pt.cex=c(0.8,1,2), lty=c(1,2,NA), pt.bg=c(NA,NA,rgb(0,0,0.2,0.2)), cex=0.8)

points(sep$WaterYFS, sep$Total_cfs, col=rgb(0,0,0,0.8), cex=0.8, pch=19)  
lines(sep$WaterYFS, sep$Total_cfs, col=rgb(0,0,0,0.8))  
#draw a rectangle around the period Dam Came Down(x1,y1,x2,y2)
rect(6,-10000,7,1000000, col=rgb(0.7,0,0,0.2), border=NA)

#Subset and run the MK test
######################################
# Pre Dam Removal
#######################################
pre.DR <- subset(month.flow, Month==9 & WaterYFS<7)
#create time series
temp.ts<-ts(pre.DR$Total_cfs, start=min(pre.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk

#Sen's Slope
sen <- sens.slope(temp.ts); sen

##### Sen's intercepts
zsen <- zyp.sen(Total_cfs~WaterYFS,pre.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]


#create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(pre.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(pre.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(pre.DR)[1]-1,1)),1)

#plot the polygon and line
polygon(c(rev(pre.DR$WaterYFS), (pre.DR$WaterYFS)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(pre.DR$WaterYFS, estimate, lty=2, col=rgb(0,0,.2,1))

######################################
# Post Dam Removal
#######################################
post.DR <- subset(month.flow, Month==9 & WaterYFS>=7)
post.DR['WaterYFS'] <- (post.DR['WaterYFS']-6)
#create time series
temp.ts<-ts(post.DR$Total_cfs, start=min(post.DR$WaterYFS, freq=1));  
mk <- mk.test(temp.ts); mk
sen <- sens.slope(temp.ts); sen

zsen <- zyp.sen(Total_cfs~WaterYFS,post.DR)
senint <- zsen$coefficients[1]
senb <- zsen$coefficients[2]
senhi <- sen$conf.int[2]
senlo <- sen$conf.int[1]

# create lines to plot on graph
estimate<-round((senint+senb*seq(0,dim(post.DR)[1]-1,1)),1)
upperc1<-round((senint+senhi*seq(0,dim(post.DR)[1]-1,1)),1)
lowerc1<-round((senint+senlo*seq(0,dim(post.DR)[1]-1,1)),1)

#plot the polygon and line on the graph:
polygon(c(rev(post.DR$WaterYFS+6), (post.DR$WaterYFS+6)), c(rev(lowerc1), (upperc1)),  col=rgb(0,0,0.2,0.2), border=NA)
lines(post.DR$WaterYFS+6, estimate, lty=2, col=rgb(0,0,.2,1))

