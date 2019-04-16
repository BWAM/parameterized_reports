library(WQReview)
library(ggplot2)
library(stringr)
library(reshape2)
library(RColorBrewer)
library(scales)

#library(plyr)
#library(gridExtra)
setwd('X:\\LakeErieWQ\\Data\\R\\Plots')


#Read site and parameter info
ErieSites <- read.csv('X:\\LakeErieWQ\\Data\\R\\ErieTribSiteInfo.csv', colClasses=c('STAID'='character'))
ErieSites$SITE_NO <- ErieSites$STAID
ErieSites$y <- 0.1
STAIDS <- ErieSites$SITE_NO
plotParms <- read.csv('X:\\LakeErieWQ\\Data\\R\\LoadPlotParms.csv', colClasses=c('PCODE'='character'))
plotParms$PNAME <- as.character(plotParms$PNAME)

#pull data
begin.date = '2017-10-01'
end.date = '2019-12-31'
qw.data <- readNWISodbc(DSN='nwisny',env.db='01',qa.db='01',STAIDS, dl.parms='All', parm.group.check=TRUE,begin.date,end.date)

#put shortnames into qw.data, remove unused columns, remove duplicate samples
b <- merge(qw.data$PlotTable,ErieSites, by='SITE_NO')
b$long_Station_NM <- b$STATION_NM
b$STATION_NM <- b$ShortName
keepVars <- names(b) %in% c("RECORD_NO", "PARM_CD", "RESULT_VA", "REMARK_CD","ShortName","SITE_NO","DSOrder", "SAMPLE_START_DT","MEDIUM_CD","PROJECT_CD","DrainageArea","MainStem")
b <- unique(b[keepVars])
b <- b[ which(b$PROJECT_CD=='ERIETRIB '| b$PROJECT_CD=='ErieTrib '| b$PROJECT_CD=='erietrib '), ]     #remove non-RIBS samples
b <- b[ which(b$MEDIUM_CD=='WS '), ]     #keep only regular samples - no QA

#set medium code shapes
qual.shapes <- c(19, 6, 2, 5, 4, 3, 6, 7, 8, 9, 11)
names(qual.shapes) <- c("Sample", "<", ">", "E", "A", "M", "N", "R", "S", "U", "V")

#set colors for mainstem and tribs
mainStem.colors <- c('indianred4','steelblue')
mainStem.colors <- brewer.pal(6, "Set1")[2:3]
names(mainStem.colors) <- c('Y','N')



logTicks <- function(n = 5, base = 10){
  # Divisors of the logarithm base. E.g. for base 10: 1, 2, 5, 10.
  divisors <- which((base / seq_len(base)) %% 1 == 0)
  mkTcks <- function(min, max, base, divisor){
    f <- seq(divisor, base, by = divisor)
    return(unique(c(base^min, as.vector(outer(f, base^(min:max), `*`)))))
  }
  
  function(x) {
    rng <- range(x, na.rm = TRUE)
    lrng <- log(rng, base = base)
    min <- floor(lrng[1])
    max <- ceiling(lrng[2])
    
    tck <- function(divisor){
      t <- mkTcks(min, max, base, divisor)
      t[t >= rng[1] & t <= rng[2]]
    }
    # For all possible divisors, produce a set of ticks and count how many ticks
    # result
    tcks <- lapply(divisors, function(d) tck(d))
    l <- vapply(tcks, length, numeric(1))
    
    # Take the set of ticks which is nearest to the desired number of ticks
    i <- which.min(abs(n - l))
    if(l[i] < 2){
      # The data range is too small to show more than 1 logarithm tick, fall
      # back to linear interpolation
      ticks <- pretty(x, n = n, min.n = 2)
    }else{
      ticks <- tcks[[i]]
    }
    return(ticks)
  }
}




#plot loop
for(i in 1:length(plotParms$PCODE)){
  varData <- unique(b[which(b$PARM_CD==plotParms$PCODE[i]), ])
  QData <-   unique(b[which(b$PARM_CD=='00061'), ])
  QData$Q <- QData$RESULT_VA
  keepVars <- names(QData) %in% c("RECORD_NO", "Q")
  QData <- QData[keepVars]
  instLoad <- merge(varData,QData, by='RECORD_NO',all.x=TRUE)
  instLoad$Q[is.na(instLoad$Q) ] <- 1   #replace missing Q with 1
  
  instLoad$instLoad <- instLoad$RESULT_VA*instLoad$Q*28.3168*60*60*24/1000000 #convert to kg/day
  instLoad$yield <- instLoad$instLoad/instLoad$DrainageArea/2.59 #kg/day/sq km
  
  #determine y for labels and melt data frame
  ErieSites$RESULT_VA <- 10^ceiling(log10(min(instLoad$RESULT_VA)))
  ErieSites$instLoad <- 10^ceiling(log10(min(instLoad$instLoad)))
  ErieSites$yield <- 10^ceiling(log10(min(instLoad$yield)))
  ErieSites <- melt(ErieSites,id.vars=c('ShortName','DSOrder'),measure.vars=c('RESULT_VA','instLoad','yield'))
  #pull select variables and melt data frame
  plotData <- melt(instLoad, id.vars=c("DSOrder","REMARK_CD","MainStem"),measure.vars=c("RESULT_VA","instLoad","yield"))
  
  #make action level data frame
  xRange <- c(.5,30.5,0,0)
  ActionLevel <- rep(plotParms$ActionLevel[i],4)
  variable <- c('RESULT_VA','RESULT_VA','instLoad','instLoad')
  AL <- data.frame(xRange,ActionLevel,variable)
  
  #set panel label info
  panelLabelx <- c(1,1,1)
  panelLabely <- c(10^ceiling(log10(max(instLoad$RESULT_VA))),10^ceiling(log10(max(instLoad$instLoad))),10^ceiling(log10(max(instLoad$yield))))
  variable <- c('RESULT_VA','instLoad','yield')
  temp <- plotParms$PNAME[i]
  panelLabelText <- c(plotParms$PNAME[i],'Instantaneous load, kg/day','Yield, kg/day/square km')
  panelInfo <- data.frame(panelLabelx, panelLabely, variable, panelLabelText)
  
  ggplot() +
    geom_text(data=ErieSites, aes(x=DSOrder,y=value,label=ShortName, angle=90), color='grey80', hjust=0,size=4)+ #write site labels
    geom_text(data=panelInfo, aes(x=panelLabelx,y=panelLabely,label=panelLabelText), hjust=0, fontface='bold' ) + #label panels
    geom_line(data=AL, aes(x=xRange, y=ActionLevel),color='firebrick') + #plot action level
    geom_point(data=plotData,aes(x=DSOrder,y=value, color=MainStem, shape=REMARK_CD) ,  size=3) + #plot data
    facet_wrap(~ variable, scales = "free_y", ncol = 1) +
    scale_shape_manual("", values = qual.shapes, labels=c('< (Non-detect)','Detect')) +
    scale_color_manual("",values = mainStem.colors, labels=c('','Cattaraugus')) +
    scale_y_log10(labels = comma, breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40)) + #labels = trans_format("log10", math_format(10^.x))) + 
    scale_x_continuous(limits=c(.5, 19.5)) +
    ylab('') +
    #theme_bw() +
    theme(strip.background = element_blank(),strip.text.x = element_blank(),
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  #print(p2)
  #dev.copy(pdf,paste('C:\\Nystrom',plotParms$PCODE[i], ".pdf",sep=''), width=11, height=8.5)
  #dev.copy(jpg,paste('X:\\LakeErieWQ\\Data\\R\\Plots\\Load_',plotParms$PCODE[i], ".pdf",sep=''), width=11, height=8.5)
  #dev.off()
  ggsave(paste('X:\\LakeErieWQ\\Data\\R\\Plots\\Load_',plotParms$PCODE[i], ".jpg",sep=''), width=20, height=12)
  
  
  rm(panelLabelx,  panelLabely,variable,temp,panelLabelText,panelInfo,varData,QData,keepVars,instLoad )
  
}








