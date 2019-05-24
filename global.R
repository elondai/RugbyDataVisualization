library(ggplot2)
library(gridExtra)
library(corrplot)

########## Data Preparation ##############
# Profile
df.playerprofiles = read.table("./datasets/PlayerProfiles.csv", skip = 2, sep = ",",
                                quot="", header = T, stringsAsFactors = F)

colnames(df.playerprofiles) = gsub("\\.", "", colnames(df.playerprofiles))
df.playerprofiles[df.playerprofiles==0] = NA

df.profile = aggregate(df.playerprofiles[, 5:31],
          by = list(df.playerprofiles$Player, df.playerprofiles$PositionGroup, df.playerprofiles$Position),
          FUN = mean)

colnames(df.profile)[1:3] = c("Player", "PositionGroup", "Position")

# Defence
df.defence = read.table("./datasets/Defence.csv", skip = 2, sep = ",",
                        quot="", header = T, stringsAsFactors = F)  

colnames(df.defence) = gsub("\\.", "", colnames(df.defence))
df.defence$TackleCompletition = as.numeric(gsub("%", "", df.defence$TackleCompletition)) 
df.def.overall = df.defence[which(is.na(df.defence$MatchID1)), ]
df.def.detail = df.defence[-which(is.na(df.defence$MatchID1)), ]
df.def.combine = merge(df.def.detail, df.profile, by = "Player")
df.def.cor = merge(df.def.overall, df.profile, by = "Player")

# Carries
df.carries = read.table("./datasets/Carries.csv", skip = 2, sep = ",",
                        quot="", header = T, stringsAsFactors = F)

colnames(df.carries) = gsub("\\.", "", colnames(df.carries))
df.carries$Linebreak = as.numeric(gsub("%", "", df.carries$Linebreak)) 
df.carries$DefendersBeatenPCT = as.numeric(gsub("%", "", df.carries$DefendersBeatenPCT)) 
df.carries$LQB = as.numeric(gsub("%", "", df.carries$LQB)) 
df.carries$Gainline = as.numeric(gsub("%", "", df.carries$Gainline))

df.car.overall = df.carries[which(is.na(df.carries$MatchID1)), ]
df.car.overall = df.car.overall[-1, ]
df.car.detail = df.carries[-which(is.na(df.carries$MatchID1)), ]
df.car.combine = merge(df.car.detail, df.profile, by = "Player")
df.car.cor = merge(df.car.overall, df.profile, by = "Player")

# Breakdown
df.breakdown = read.table("./datasets/Breakdown.csv", skip = 2, sep = ",",
                          quot="", header = T, stringsAsFactors = F)

colnames(df.breakdown) = gsub("\\.", "", colnames(df.breakdown))
df.breakdown$OOAAEffectiveness = as.numeric(gsub("%", "", df.breakdown$OOAAEffectiveness)) 
df.breakdown$OOADEffectiveness = as.numeric(gsub("%", "", df.breakdown$OOADEffectiveness))
df.brk.overall = df.breakdown[which(is.na(df.breakdown$MatchID1)), ]
df.brk.detail = df.breakdown[-which(is.na(df.breakdown$MatchID1)), ]
df.brk.combine = merge(df.brk.detail, df.profile, by = "Player")
df.brk.cor = merge(df.brk.overall, df.profile, by = "Player")

########## Utility Functions ##############
# Get X,Y for the polygon in gauge
get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
  th.start <- pi*(1-a/100)
  th.end   <- pi*(1-b/100)
  th       <- seq(th.start,th.end,length=100)
  x        <- c(r1*cos(th),rev(r2*cos(th)))
  y        <- c(r1*sin(th),rev(r2*sin(th)))
  return(data.frame(x,y))
}

# Get X,Y for the pointer in gauge
get.poly4pointer <- function(a, r1 = 0.5, r2 = 1.0) {
  th.start <- pi*((a-0.3)/100)
  th.end   <- pi*((a+0.3)/100)
  th       <- seq(th.start,th.end,length=100)
  
  th2.start <- pi*((a-8)/100)
  th2.end   <- pi*((a+8)/100)
  th2       <- seq(th2.start,th2.end,length=100)
  
  x        <- c(r1*cos(th2),rev(r2*cos(th)))
  y        <- c(r1*sin(th2),rev(r2*sin(th)))
  return(data.frame(x,y))
}

# Gauge Plot Function
gg.gauge <- function(pos=0, value=0, min=0, max=100, title="", Date="", opposite=0, breaks=c(0,30,70,100)) {
  if (is.list(pos))
  {
    pos = unlist(pos)
  }
  if (is.list(value))
  {
    value = unlist(value)
  }
  if (is.list(min))
  {
    min = unlist(min)
  }
  if (is.list(max))
  {
    max = unlist(max)
  }
  
  if (all(is.na(pos)))
  {
    if (opposite)
    {
      # for speed, the lower figure is the better
      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Orange")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        annotate("text",x=0,y=0,label="NA",vjust=0,size=5,fontface="italic")+
        geom_text(data=as.data.frame(breaks), size=4, fontface="bold", vjust=-0.5, col="white",
                  aes(x=0.75*cos(pi*(1-breaks/100)),y=0.75*sin(pi*(1-breaks/100)),
                      label=round(max - breaks / 100 * (max - min), 2)))+ 
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title = element_text(hjust = 0.5, size=15, face="bold.italic"))+
        labs(title=title)      
    }
    else
    {
      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Orange")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        annotate("text",x=0,y=0,label="NA",vjust=0,size=5,fontface="italic")+
        geom_text(data=as.data.frame(breaks), size=4, fontface="bold", vjust=-0.5, col="white",
                  aes(x=0.75*cos(pi*(1-breaks/100)),y=0.75*sin(pi*(1-breaks/100)),
                      label=round(max - breaks / 100 * (max - min), 2)))+ 
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              plot.title = element_text(hjust = 0.5, size=15, face="bold.italic"))+
        labs(title=title)      
    }
  }
  else
  {
    if(opposite)
    {
      d.arrow = data.frame()
      for (i in 1:length(pos)){
        if (!is.na(pos[i]))
        {
          temp = get.poly4pointer(pos[i], 0.2, 0.85)
          d.arrow = rbind(d.arrow, cbind(temp, Date[i]))          
        }
      }
      colnames(d.arrow)[3] = "Date"
      
      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Orange")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        geom_polygon(data=d.arrow, aes(x,y,group=Date,fill=Date), col="grey30")+
        geom_text(data=as.data.frame(breaks), size=4, fontface="bold", vjust=-0.5, col="white",
                  aes(x=0.75*cos(pi*(1-breaks/100)),y=0.95*sin(pi*(1-breaks/100)),
                      label=round(max - breaks / 100 * (max - min), 2)))+ 
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size=15, face="bold.italic"))+
        labs(title=title)         
    }
    else
    {
      d.arrow = data.frame()
      for (i in 1:length(pos)){
        if (!is.na(pos[i]))
        {
          temp = get.poly4pointer(pos[i], 0.2, 0.85)
          d.arrow = rbind(d.arrow, cbind(temp, Date[i]))          
        }
      }
      colnames(d.arrow)[3] = "Date"
      
      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Orange")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        geom_text(data=as.data.frame(breaks), size=4, fontface="bold", vjust=-0.5, col="white",
                  aes(x=0.75*cos(pi*(1-breaks/100)),y=0.9*sin(pi*(1-breaks/100)),
                      label=round(min + breaks / 100 * (max - min), 2)))+
        geom_polygon(data=d.arrow, aes(x,y,group=Date,fill=Date), col="grey30")+
        #annotate("text",x=0,y=0,label=value,vjust=0,size=4,fontface="bold",col="red")+        
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank(),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size=15, face="bold.italic"))+
        labs(title=title)         
    }
  }
  
}

# Get min, max, value and position for plotting gauge.
get.value4gauge <- function(x, y) {
  v.min = min(x, na.rm = T)
  v.max = max(x, na.rm = T)
  v.value = y
  
  if (v.max == v.min)
  {
    v.percent = 100
  }
  else
  {
    v.percent = ((v.value - v.min) / (v.max - v.min)) * 100
  }
  
  return(list(v.min, v.max, v.value, v.percent))
}
