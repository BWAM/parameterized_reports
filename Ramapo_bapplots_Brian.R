setwd("C:/NYBackup/RStats/Projects/RIBS/Ramapo")
##Reading data files from the working directory that was established inthe data import script
rama.data<-read.table("RAMAPO_2018_RAS_METRICS.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#ordering data from US to DS
rama.data$LOCATION.STATION<-factor(rama.data$LOCATION.STATION, 
                                   levels= c("RAMA-18.6",
                                                                         "RAMA_T25-3-0.2",
                                                                         "RAMA-16.8",
                                                                         "RAMA-16.5",
                                                                         "RAMA-16.1",
                                                                         "RAMA-13.3",
                                                                         "RAMA-11.8",
                                                                         "RAMA-4.8",
                                                                         "RAMA-1.1"))


##Standard Error Plots using spread of data for this project

#All code from this point down was adapted from my Honnedaga Inverts graphing script

#Helper function from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#to produce summary stats (including SEs) for the dataset
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


##plot below can run ci-95% confidence intervals or se-std error of the mean
library(ggplot2)
library(Rmisc)
Sum_bap <- summarySE(rama.data, measurevar="bap", groupvars=c("LOCATION.STATION", "PWLID"))

labels.df<-data.frame(
  label=c("non","slight","moderate","severe"),
  x=1,
  y=c(8.75,6.25,3.0,1.0),
  stringsAsFactors = FALSE
)

ggplot(data=Sum_bap, aes(x=LOCATION.STATION, y=bap, shape=PWLID, fill=PWLID)) + 
  geom_errorbar(data=Sum_bap,aes(ymin=bap-ci, ymax=bap+ci), width=.15, size=0.25) +
  geom_point(data=Sum_bap,size=3) +
  geom_hline(data=Sum_bap, yintercept=2.5, linetype="dashed", color = "gray") + #adds horizontal lines for impact categories
  geom_hline(data=Sum_bap, yintercept=5, linetype="dashed", color = "gray") +
  geom_hline(data=Sum_bap, yintercept=7.5, linetype="dashed", color = "gray") +
  labs(family="serif", size=14)+
  theme(legend.text=element_text(size=12,family="serif"))+
  ylab("Biological Assessment Profile Score") +
  scale_y_continuous(limits=c(0, 10)) +
  scale_shape_manual("PWL ID", values = c(16, 21, 25)) +
  scale_fill_manual("PWL ID",  values = c ("black", "white", "gray")) +
   theme_bw() + theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), #remove gridlines
                     axis.title.y=element_text(size=12,family="serif",face="bold"),
                     axis.title.x=element_blank(), 
                     axis.text.x = element_text(angle = 45, 
                     hjust = 1, size=12,family="serif",face="bold"), #rotate text angle
                     plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), 
                     legend.position=c(.11,.81)+ #reduces white space around plot edges
                       geom_label(aes(x=.5,y=.5),label="test"))
  
  #geom_label(data=labels.df,aes(x,y, label=label))


#below text should be added to graph but not working because it was from base R
#geom_text(.9,.9,"severe",cex=0.75,srt=90)
#text(7.7,3.7,"moderate",cex=0.75,srt=90)
#text(7.7,6.2,"slight",cex=0.75,srt=90)
#text(7.7,8.7,"non",cex=0.75,srt=90)


summary(rama.data$bap)


#load the psych package, downloaded from CRAN, now residing in local packages
library(psych)
##basic descriptive stats for bap broken into grouped variables ie Location and SiteType\
## output is a matrix mat=TRUE, and have excluded the skew stats skew and kurtosis
describeBy(rama.data[,"bap"],list(rama.data$LOCATION.STATION),skew=FALSE, ranges=TRUE, mat=TRUE)

##write results
##sink() creates the txt file, having the window wide will create a single row of data as opposed
## to a broken row that carries over below. It will write the same way its displayed
sink("Ramapo.Summary.Stats.csv")
##perform statistical summary from above that will now go into the open sink limited to 3 decimal placts (digits=3)
describeBy(rama.data[,"bap"],list(rama.data$LOCATION.STATION),skew=FALSE, digits=2,ranges=TRUE, mat=TRUE)
##closes the sink after the output has been written to the "RochEmbay.Stats.txt"
sink()


##########BOXPLOTS for same dataset#############

#rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
#    col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),...)
##see color literature for adjustments - lots
##see ?rect for details on setting rectangle 
##this works to produce boxplots with first 3 box highlighted gray##
##must replot a second time to put it on top of gray box
##setting axis limits  - xlim=c(xmin, xmax), ylim=c(ymin, ymax))
##yaxs="i" removes the 4% added to data range specified in axis limits 
##yaxt="n" suppresses y axis default to add specified axis ticks
#text adds text to x,y coordinates of plot in quotes

#plots boxplot, main title =XGENS, cex.axis adjusts axis font size, xlab adds x axis label,
# ylab adds y label, cex.lab adjusts label sizes, 
# all font size adjustment are multiplied by default size of 1

#this script is for boxplots and has manual naming  based on the order field - not necessary given the reorder above
boxplot(bap~order,rama.data, names=c("RAMA-18.6","RAMA_T25-3-0.2","RAMA-16.8","RAMA-16.5","RAMA-16.1","RAMA-13.3","RAMA-11.8","RAMA-4.8","RAMA-1.1"),
        cex.axis=0.75,xlab="Sampling Location", ylab="Biological Assessment Profile",
        cex.lab=1.01,ylim=c(0, 10),yaxs="i",yaxt="n")
axis(2,at=seq(0, 10,by=2.5),cex.axis=0.75)

#adds rectangle to plot, removing the border
rect(5.5,0,7.5,10,col="gray95", border=NA)
#reference lines
abline(h=c(2.5,5, 7.5),col="gray")
#plot a second time to write over gray box that covers plot
boxplot(bap~order,rama.data, names=c("RAMA-18.6","RAMA_T25-3-0.2","RAMA-16.8","RAMA-16.5","RAMA-16.1","RAMA-13.3","RAMA-11.8","RAMA-4.8","RAMA-1.1"),
        col="white",cex.axis=0.75, xlab="Sampling Location", 
        ylab="Biological Assessment Profile",cex.lab=1.01,ylim=c(0, 10),yaxs="i", yaxt="n",add=TRUE)
axis(2,at=seq(0, 10,by=2.5),cex.axis=0.75)


#text adds text to x,y coordinates of plot in quotes
text(3,8.5,"Reference")
text(6.5,8.5,"AOC")
text(8.5,8.5,"XXX")
text(7.7,1.2,"severe",cex=0.75,srt=90)
text(7.7,3.7,"moderate",cex=0.75,srt=90)
text(7.7,6.2,"slight",cex=0.75,srt=90)
text(7.7,8.7,"non",cex=0.75,srt=90)


