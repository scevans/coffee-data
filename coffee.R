
### How does the espresso machine vary in the volume of
### coffee it dispenses for a single "large" cup?

setwd("/home/sam/Dropbox/github/coffee-data")
#setwd("wherever you put this file after downloading it")

library(ggplot2)

df <- read.csv("coffee-data.csv")

# The ruler I use to measure height (and therefore calculate volume)
# has a 3 mm length between the zero mark and its end. So, we add 3 
# to the height...
df$height <- df$height + 3

# ... and then calculate volume and rate, 
# so we have more accurate data!
df$volume <- df$height*(3.1415*37^2)/1000
df$rate <- df$volume/df$duration

################################
### Volume vs. duration (duration of coffee dispensation)
{
# sample size
n <- length(df$duration)
# label to print sample size in subsequent plot
n.text <- paste("n == ", n)
# data.frame of sample size label and coordinates
# at which to print it on the plot
n.df <- data.frame(n.text, x=min(df$duration), y=0.87*max(df$volume))
  
# calculate R-squared value
rsq <- signif(summary(lm(df$volume ~ df$duration))$r.squared, digits=3)
# make a label to print R-squared value in subsequent plot
rsq.text <- paste("R^2 == ",rsq)
# make a data.frame of R-squared value label and coordinates
# at which to print it on the plot
rsq.df <- data.frame(rsq.text,x=min(df$duration),y=0.9*max(df$volume))

# function to extract regression equation
lm_eqn <- function(df)
{
  m <- lm(df$volume ~ df$duration)
  eq <- substitute(y == a + b*x, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq))                 
}

# generate plot w/ simple linear regression overlayed
plot <- ggplot(df, aes(duration, volume)) +
  geom_point() +
  stat_smooth(method="lm") +
  xlab("duration of coffee dispensation (seconds)") +
  ylab("volume of one large cup (ml)") +
  theme(axis.text=element_text(size=4), axis.title=element_text(size=5)) +
  theme_bw()

# add regression equation to plot
plot <- plot + 
  geom_text(data=n.df, aes(x,y,label=n.text),parse=TRUE,hjust=0,vjust=0,size=5) +
  geom_text(data=rsq.df, aes(x,y,label=rsq.text),parse=TRUE,hjust=0,vjust=0,size=5) +
  geom_text(aes(x=min(df$duration),y=0.94*max(df$volume),label=lm_eqn(df)),parse=TRUE,hjust=0,vjust=0,size=5)

# add a layer that highlights the last data point recorded
#plot <- plot +
#  geom_point(data=df[length(df$duration),],color="red",size=2.5)

# add a layer that highlights data points where espresso machine 
# had to "warm up" before dispensing coffee
plot <- plot +
  geom_point(data=df[df$warmup%in%"yes",], color="red", size=2.5) +
  geom_text(aes(x=mean(df$duration)-5,y=min(df$volume),label="(machine warmup before dispensed)"),
           hjust=0,vjust=0.5,color="red",size=4)

# print plot in R window
plot

# save plot directly to file as a .jpeg
jpeg("coffee-volume-vs-duration-all.jpeg", width=7, height=5, units="in", res=300)
print(plot)
dev.off()
}



################################
### Distributions of volume dispensed: weekdays vs. weekends
{
df$workday <- factor(df$workday)

# median volumes for weekday and weekend categories
med.week <- median(df$volume[df$workday%in%"Mon-Fri"])
med.wkend <- median(df$volume[df$workday%in%"Sat-Sun"])

# generate violin-and-boxplot plot with points overlayed and jittered to better reflect sample size
vol.workday <- ggplot(df, aes(workday, volume)) +
  geom_violin(color="white", fill="gray", alpha=0.85)

# add layer highlighting points where espresso machine needed to
# "warm up" before dispensing coffee
vol.workday <- vol.workday +
  geom_point(data=df[df$warmup%in%"yes",], color="red", size=2.5) +
  geom_text(aes(x="Sat-Sun", y=max(df$volume)+10, label="(machine warmup before dispensed)"),
            hjust=1,vjust=1,size=4,color="red") +
  geom_text(aes(x="Sat-Sun", y=max(df$volume)+4, label="(median)"),
            hjust=1,vjust=1,size=4,color="blue") +
  #geom_point(position=position_jitter(width=0.1,height=0)) +
  # plot points representing median volumes for weekdays and weekends
  geom_point(aes(x="Mon-Fri",y=med.week),color="blue",size=2.5) +
  geom_point(aes(x="Sat-Sun",y=med.wkend),color="blue",size=2.5) +
  xlab("time of the week") +
  ylab("volume of one large cup (ml)") +
  theme(axis.text=element_text(size=4), axis.title=element_text(size=5)) +
  coord_cartesian(ylim=c(min(df$volume)-5,max(df$volume)+30)) +
  theme_bw()

# add sample sizes for weekdays and weekends
vol.workday <- vol.workday +
  geom_text(aes(x="Mon-Fri",y=max(df$volume)+15,
                label=paste("n == ", length(df$workday[which(df$workday%in%"Mon-Fri")]))),
            parse=TRUE,hjust=0.5,vjust=0,size=5) +
  geom_text(aes(x="Sat-Sun",y=max(df$volume)+15,
                label=paste("n == ", length(df$workday[which(df$workday%in%"Sat-Sun")]))),
            parse=TRUE,hjust=0.5,vjust=0,size=5)

# print plot in R window
vol.workday

# save plot to file as .jpeg
jpeg("coffee-volume-by-workday.jpeg", width=7, height=5, units="in", res=300)
print(vol.workday)
dev.off()
}


################################
### Distributions of volume dispensed by day
{
  df$day <- factor(df$day,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
    
    
  median.by.day <- data.frame(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                              c(0,0,0,0,0,0,0),
                              c(0,0,0,0,0,0,0))
  
  names(median.by.day) <- c("day", "median.volume", "n")
  
  for (i in 1:7)
  {
    median.by.day$median.volume[i] <- median(df$volume[df$day%in%median.by.day$day[i]])
    median.by.day$n[i] <- length(df$volume[df$day%in%median.by.day$day[i]])
  }
  
  # generate violin-and-boxplot plot with points overlayed and jittered to better reflect sample size
  vol.day <- ggplot(df, aes(day, volume)) +
    geom_violin(color="white", fill="gray", alpha=0.85) +
    geom_point()
  
  # add layer highlighting points where espresso machine needed to
  # "warm up" before dispensing coffee
  vol.day <- vol.day +
    geom_point(data=df[df$warmup%in%"yes",], color="red", size=2.5) +
    geom_text(aes(x="Sun", y=max(df$volume)+16, label="(machine warmup before dispensed)"),
              hjust=1,vjust=1,size=4,color="red") +
    geom_text(aes(x="Thu", y=max(df$volume)+16, label="(median)"),
              hjust=1,vjust=1,size=4,color="blue") +
    geom_text(aes(x="Tue", y=max(df$volume)+16, label=paste("n = ", length(df$volume), sep="")),
              hjust=0,vjust=1,size=4) +
    #geom_point(position=position_jitter(width=0.1,height=0)) +
    # plot points representing median volumes for weekdays and weekends
    geom_point(data=median.by.day, aes(day,median.volume),color="blue",size=2.5) +
    xlab("day of the week") +
    ylab("volume of one large cup (ml)") +
    theme(axis.text=element_text(size=4), axis.title=element_text(size=5)) +
    coord_cartesian(ylim=c(min(df$volume)-5,max(df$volume)+20)) +
    theme_bw()
  
  # add sample sizes for weekdays and weekends
  vol.day <- vol.day +
    geom_text(data=median.by.day, aes(day,y=max(df$volume)+4),
                  label=paste("(", median.by.day$n, ")", sep=""),
              parse=TRUE,hjust=0.5,vjust=0,size=4)
  
  # print plot in R window
  vol.day
  
  # save plot to file as .jpeg
  jpeg("coffee-volume-by-day.jpeg", width=8, height=5, units="in", res=300)
  print(vol.day)
  dev.off()
}


################################
### Rate vs. date
{
  # add a numeric variable of days of the year (for x-axis)
  date.to.num <- read.csv("date-to-numeric.csv")
  df$day.yr <- 0  
  for (i in 1:length(df$date))
  {
    df$day.yr[i] <- date.to.num$day.yr[which(date.to.num$date%in%df$date[i])]
  }
  
  rate.date <- ggplot(df, aes(day.yr, rate)) +
    geom_point() +
    geom_hline(yintercept=mean(df$rate), linetype="dashed") +
    stat_smooth(method="loess") +
    xlab("day of the year") +
    ylab("rate of coffee dispensation (ml/second)") +
    scale_x_continuous(breaks=c(seq(from=min(df$day.yr), to=max(df$day.yr), by=7))) +
    theme(axis.text=element_text(size=4), axis.title=element_text(size=5)) +
    theme_bw()
  
  rate.date <- rate.date +
    geom_point(data=df[df$warmup%in%"yes",], color="red", size=2.5) +
    geom_text(aes(x=64,y=2.7,label="(machine warmup before dispensed)"),
              hjust=0,vjust=0.5,color="red",size=4) +
    geom_text(aes(x=64,y=2.8,label="(dashed line indicates mean rate)"),
              hjust=0,vjust=0.5,size=4) +
    geom_text(aes(x=64,y=2.9,label=paste("n = ", length(df$volume), sep="")),
              hjust=0,vjust=0.5,size=4)
  
  rate.date
  
  jpeg("coffee-rate-by-date.jpeg", width=8, height=5, units="in", res=300)
  print(rate.date)
  dev.off()
}


################################
### Volume vs. duration (duration of coffee dispensation)
### EXCLUDING instances where machine had to warm up
{
  # subset df by removing machine warmup rows
  df.no <- df[df$warmup%in%"no",]
  
  # recalculate sample size
  n <- length(df.no$duration)
  # label to print sample size in subsequent plot
  n.text <- paste("n == ", n)
  # data.frame of sample size label and coordinates
  # at which to print it on the plot
  n.df <- data.frame(n.text, x=min(df.no$duration), y=0.87*max(df.no$volume))
  
  # recalculate R-squared value
  rsq <- signif(summary(lm(df.no$volume ~ df.no$duration))$r.squared, digits=3)
  # make a label to print R-squared value in subsequent plot
  rsq.text <- paste("R^2 == ",rsq)
  # make a data.frame of R-squared value label and coordinates
  # at which to print it on the plot
  rsq.df <- data.frame(rsq.text,x=min(df.no$duration),y=0.9*max(df.no$volume))
  
  # function to extract regression equation
  lm_eqn <- function(df.no)
  {
    m <- lm(df.no$volume ~ df.no$duration)
    eq <- substitute(y == a + b*x, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2)))
    as.character(as.expression(eq))                 
  }
  
  # generate plot w/ simple linear regression overlayed
  plot.no.warmup <- ggplot(df.no, aes(duration, volume)) +
    geom_point() +
    stat_smooth(method="lm") +
    xlab("duration of coffee dispensation (seconds)") +
    ylab("volume of one large cup (ml)") +
    theme(axis.text=element_text(size=4), axis.title=element_text(size=5)) +
    theme_bw()
  
  # add regression equation to plot
  plot.no.warmup <- plot.no.warmup + 
    geom_text(data=n.df, aes(x,y,label=n.text),parse=TRUE,hjust=0,vjust=0,size=5,color="blue") +
    geom_text(data=rsq.df, aes(x,y,label=rsq.text),parse=TRUE,hjust=0,vjust=0,size=5) +
    geom_text(aes(x=min(df.no$duration),y=0.94*max(df.no$volume),label=lm_eqn(df.no)),parse=TRUE,hjust=0,vjust=0,size=5)
    
  # add a layer that highlights data points where espresso machine 
  # had to "warm up" before dispensing coffee
  plot.no.warmup <- plot.no.warmup +
    geom_text(aes(x=mean(df.no$duration)-5,y=min(df.no$volume),label="(machine warmup data points removed)"),
              hjust=0,vjust=0.5,color="blue",size=4)
  
  # print plot in R window
  plot.no.warmup
  
  # save plot directly to file as a .jpeg
  jpeg("coffee-volume-vs-duration-excluding-warmup-data.jpeg", width=7, height=5, units="in", res=300)
  print(plot.no.warmup)
  dev.off()
}