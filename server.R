library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)
library(corrplot)

get.poly <- function(a,b,r1=0.5,r2=1.0) {
  th.start <- pi*(1-a/100)
  th.end   <- pi*(1-b/100)
  th       <- seq(th.start,th.end,length=100)
  x        <- c(r1*cos(th),rev(r2*cos(th)))
  y        <- c(r1*sin(th),rev(r2*sin(th)))
  return(data.frame(x,y))
}

get.poly2 <- function(a, r1=0.5,r2=1.0) {
  th.start <- pi*(1-(a-0.3)/100)
  th.end   <- pi*(1-(a+0.3)/100)
  th       <- seq(th.start,th.end,length=100)
  
  th2.start <- pi*(1-(a-8)/100)
  th2.end   <- pi*(1-(a+8)/100)
  th2       <- seq(th2.start,th2.end,length=100)
  
  x        <- c(r1*cos(th2),rev(r2*cos(th)))
  y        <- c(r1*sin(th2),rev(r2*sin(th)))
  return(data.frame(x,y))
}

get.poly3 <- function(a, r1=0.5,r2=1.0) {
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
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Gold1")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        annotate("text",x=0,y=0,label="NA",vjust=0,size=5,fontface="italic")+
        annotate("text",x=0.75*cos(pi),y=0,label=max,vjust=-0.5,size=4,fontface="bold",col="white")+
        annotate("text",x=-0.75*cos(pi),y=0,label=min,vjust=-0.5,size=4,fontface="bold",col="white")+
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
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Gold1")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        annotate("text",x=0,y=0,label="NA",vjust=0,size=5,fontface="italic")+
        annotate("text",x=0.75*cos(pi),y=0,label=min,vjust=-0.5,size=4,fontface="bold",col="white")+
        annotate("text",x=-0.75*cos(pi),y=0,label=max,vjust=-0.5,size=4,fontface="bold",col="white")+
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
          temp = get.poly3(pos[i], 0.2, 0.85)
          d.arrow = rbind(d.arrow, cbind(temp, Date[i]))          
        }
      }
      colnames(d.arrow)[3] = "Date"
      
      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Gold1")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        geom_polygon(data=d.arrow, aes(x,y,group=Date,fill=Date), col="grey30")+
        #annotate("text",x=0,y=0,label=value,vjust=0,size=4,fontface="bold",col="red")+
        annotate("text",x=0.75*cos(pi),y=0,label=max,vjust=-0.5,size=4,fontface="bold",col="white")+
        annotate("text",x=-0.75*cos(pi),y=0,label=min,vjust=-0.5,size=4,fontface="bold",col="white")+
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
          temp = get.poly3(pos[i], 0.2, 0.85)
          d.arrow = rbind(d.arrow, cbind(temp, Date[i]))          
        }
      }
      colnames(d.arrow)[3] = "Date"

      ggplot() + 
        geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="OrangeRed")+
        geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="Gold1")+
        geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="SeaGreen")+
        annotate("text",x=0.75*cos(pi),y=0,label=min,vjust=-0.5,size=4,fontface="bold",col="white")+
        annotate("text",x=-0.75*cos(pi),y=0,label=max,vjust=-0.5,size=4,fontface="bold",col="white")+
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

f.get_value4gauge <- function(x, y) {
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

server <- function(input, output, session) {
  # Plot Defence
  output$DefenceRanking <- renderPlot({
    df.def.display = df.def.overall[, c("Player", input$sel_defence, "MinutesTotal")]
    colnames(df.def.display) = c("X", "Y", "Z")
    if (input$chk_defence){
      df.def.display$Y = round(df.def.display$Y / df.def.display$Z, digit = 2)
    }
    ggplot(data=df.def.display, aes(x=reorder(X, Y), y=Y)) + 
      scale_x_discrete(name = "Player", expand = c(0,0)) + 
      scale_y_discrete(name = input$sel_defence, expand = c(0,0)) +
      geom_bar(stat="identity", fill="steelblue") + coord_flip() + 
      geom_text(aes(label=Y), hjust=1.2, color="white", size=2.5) 
  })
  
  output$DefencePosition <- renderPlot({
    df.def.display = df.def.combine[, c("Position", input$sel_defence, "MinutesTotal")]
    colnames(df.def.display) = c("X", "Y", "Z")
    
    if (input$chk_defence){
      df.def.display$Y = round(df.def.display$Y / df.def.display$Z, digit = 2)
    }
    
    ggplot(data=df.def.display, aes(x=X, y=Y)) + 
      geom_boxplot(color = "#0072B2", fill = "steelblue") + 
      theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1)) +
      scale_x_discrete(name = "Position", expand = c(0,0)) + 
      scale_y_continuous(name = input$sel_defence)
  })

  # Plot Carries
  output$CarriesRanking <- renderPlot({
    df.car.display = df.car.overall[, c("Player", input$sel_carries, "MinutesTotal")]
    colnames(df.car.display) = c("X", "Y", "Z")
    if (input$chk_carries){
      df.car.display$Y = round(df.car.display$Y / df.car.display$Z, digit = 2)
    }
    ggplot(data=df.car.display, aes(x=reorder(X, Y), y=Y)) + 
      scale_x_discrete(name = "Player", expand = c(0,0)) + 
      scale_y_discrete(name = input$sel_carries, expand = c(0,0)) +
      geom_bar(stat="identity", fill="steelblue") + coord_flip() + 
      geom_text(aes(label=Y), hjust=1.2, color="white", size=2.5) 
  })
  
  output$CarriesPosition <- renderPlot({
    df.car.display = df.car.combine[, c("Position", input$sel_carries, "MinutesTotal")]
    colnames(df.car.display) = c("X", "Y", "Z")
    
    if (input$chk_carries){
      df.car.display$Y = round(df.car.display$Y / df.car.display$Z, digit = 2)
    }
    
    ggplot(data=df.car.display, aes(x=X, y=Y)) + 
      geom_boxplot(color = "#0072B2", fill = "steelblue") + 
      theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1)) +
      scale_x_discrete(name = "Position", expand = c(0,0)) + 
      scale_y_continuous(name = input$sel_carries)
  })
  
  # Plot Breakdown
  output$BreakdownRanking <- renderPlot({
    df.brk.display = df.brk.overall[, c("Player", input$sel_breakdown, "MinutesTotal")]
    colnames(df.brk.display) = c("X", "Y", "Z")
    if (input$chk_breakdown){
      df.brk.display$Y = round(df.brk.display$Y / df.brk.display$Z, digit = 2)
    }
    ggplot(data=df.brk.display, aes(x=reorder(X, Y), y=Y)) + 
      scale_x_discrete(name = "Player", expand = c(0,0)) + 
      scale_y_discrete(name = input$sel_breakdown, expand = c(0,0)) +
      geom_bar(stat="identity", fill="steelblue") + coord_flip() + 
      geom_text(aes(label=Y), hjust=1.2, color="white", size=2.5) 
  })

  output$BreakdownPosition <- renderPlot({
    df.brk.display = df.brk.combine[, c("Position", input$sel_breakdown, "MinutesTotal")]
    colnames(df.brk.display) = c("X", "Y", "Z")
    
    if (input$chk_breakdown){
      df.brk.display$Y = round(df.brk.display$Y / df.brk.display$Z, digit = 2)
    }
    
    ggplot(data=df.brk.display, aes(x=X, y=Y)) + 
      geom_boxplot(color = "#0072B2", fill = "steelblue") + 
      theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1)) +
      scale_x_discrete(name = "Position", expand = c(0,0)) + 
      scale_y_continuous(name = input$sel_breakdown)
  })

  output$Anthropometric <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                           as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                           as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 5:6)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 5:6)]
    
    output$Player_Pos <- renderText({
      df.player.display$Position[1]
    })

    nLen = nrow(df.player.display)
    df.player.display = df.player.display[order(df.player.display$BW), ]
    BW = f.get_value4gauge(df.pos.display$BW, df.player.display$BW)
    d.BW = data.frame(x1 = c(1, 1 + 5 * unlist(BW[4])[-nLen] / 100),
                      x2 = 1 + 5 * unlist(BW[4]) / 100,
                      y1 = 1.3,
                      y2 = 1.35,
                      Value = unlist(BW[3]),
                      Date = df.player.display$Date)
    
    df.player.display = df.player.display[order(df.player.display$BodyFatsumof8mm), ]
    BF = f.get_value4gauge(df.pos.display$BodyFatsumof8mm, df.player.display$BodyFatsumof8mm)
    d.BF = data.frame(x1 = c(1, 1 + 5 * unlist(BF[4])[-nLen] / 100),
                      x2 = 1 + 5 * unlist(BF[4]) / 100,
                      y1 = 1,
                      y2 = 1.05,
                      Value = unlist(BF[3]),
                      Date = df.player.display$Date)
    
    d = data.frame(x1=c(1, 1), x2=c(6, 6), y1=c(1, 1.3), y2=c(1.05, 1.35), 
                   minValue=unlist(c(BF[1], BW[1])),
                   maxValue=unlist(c(BF[2], BW[2])))
    
    ggplot() + 
      geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0.5) +
      geom_text(data=d, aes(x=x1, y=y1 - 0.03, label=minValue), size=4) +
      geom_text(data=d, aes(x=x2, y=y1 - 0.03, label=maxValue), size=4) +
      geom_text(data=d, aes(x=3.5, y=y2[2] + 0.1, label="Body Weight"), size=5) +
      geom_text(data=d, aes(x=3.5, y=y2[1] + 0.1, label="Body Fat(Sum of 8) "), size=5) +
      geom_rect(data=d.BW, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Date), color="black", alpha=0.5) +
      geom_text(data=d.BW, aes(x=x2, y=rep(c(1.27,1.37), len=nLen), label=Value), size=4) +
      geom_point(data=d.BW, aes(x=x2, y=1.3 - 0.01, fill=Date), shape=24, size=4) +
      geom_rect(data=d.BF, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Date), color="black", alpha=0.5) +
      geom_text(data=d.BF, aes(x=x2, y=rep(c(0.97,1.07), len=nLen), label=Value), size=4) +
      geom_point(data=d.BF, aes(x=x2, y=1 - 0.01, fill=Date), shape=24, size=4) +
      theme(panel.grid = element_blank(),
            panel.background = element_blank(), 
            axis.text = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank())
  })

  output$YoYo <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 30:31)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 30:31)]
    
    YOYO = f.get_value4gauge(df.pos.display$YoYoScore, df.player.display$YoYoScore)
    BRONCO = f.get_value4gauge(df.pos.display$BroncoTime, df.player.display$BroncoTime)
    
    
    grid.arrange(arrangeGrob(gg.gauge(YOYO[4], YOYO[3], YOYO[1], YOYO[2], "YoYo Score", df.player.display$Date),
                             gg.gauge(BRONCO[4], BRONCO[3], BRONCO[1], BRONCO[2], "Bronco Time",
                                      df.player.display$Date, opposite = 1),
                             ncol=2))
  })
  
  output$Strength_Upper <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 7:10)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 7:10)]
    
    BP = f.get_value4gauge(df.pos.display$BenchPress, df.player.display$BenchPress)
    DBP = f.get_value4gauge(df.pos.display$DBPress, df.player.display$DBPress)
    BOR = f.get_value4gauge(df.pos.display$BentOverRow, df.player.display$BentOverRow)
    WC = f.get_value4gauge(df.pos.display$WeightedChin, df.player.display$WeightedChin)
    
    
    grid.arrange(arrangeGrob(gg.gauge(BP[4], BP[3], BP[1], BP[2], "BenchPress", df.player.display$Date),
                             gg.gauge(DBP[4], DBP[3], DBP[1], DBP[2], "DB Press", df.player.display$Date),
                             gg.gauge(BOR[4], BOR[3], BOR[1], BOR[2], "BentOverRow", df.player.display$Date),
                             gg.gauge(WC[4], WC[3], WC[1], WC[2], "WeightedChin", df.player.display$Date),
                             ncol=2))
  })
  
  output$Strength_Lower <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 11:16)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 11:16)]

    Squat = f.get_value4gauge(df.pos.display$Squat, df.player.display$Squat)
    FS = f.get_value4gauge(df.pos.display$FrontSquat, df.player.display$FrontSquat)
    TBDL = f.get_value4gauge(df.pos.display$TrapBarDeadlift, df.player.display$TrapBarDeadlift)
    SU = f.get_value4gauge(df.pos.display$StepUp, df.player.display$StepUp)
    HT = f.get_value4gauge(df.pos.display$HipThrust, df.player.display$HipThrust)
    HTOB = f.get_value4gauge(df.pos.display$HipThrustOffBench, df.player.display$HipThrustOffBench)

    grid.arrange(arrangeGrob(gg.gauge(Squat[4], Squat[3], Squat[1], Squat[2], "Squat", df.player.display$Date),
                             gg.gauge(FS[4], FS[3], FS[1], FS[2], "FrontSquat", df.player.display$Date),
                             gg.gauge(TBDL[4], TBDL[3], TBDL[1], TBDL[2], "TrapBarDeadlift", df.player.display$Date),
                             gg.gauge(SU[4], SU[3], SU[1], SU[2], "StepUp", df.player.display$Date),
                             gg.gauge(HT[4], HT[3], HT[1], HT[2], "HipThrust", df.player.display$Date),
                             gg.gauge(HTOB[4], HTOB[3], HTOB[1], HTOB[2], "HipThrustOffBench", df.player.display$Date),
                             ncol=3))
  })
  
  output$Power <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 17:25)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 17:25)]    
    
    PC = f.get_value4gauge(df.pos.display$PowerClean, df.player.display$PowerClean)
    HC = f.get_value4gauge(df.pos.display$HangClean, df.player.display$HangClean)
    MC = f.get_value4gauge(df.pos.display$MuscleClean, df.player.display$MuscleClean)
    X20KG = f.get_value4gauge(df.pos.display$X20kg, df.player.display$X20kg)
    X30kg = f.get_value4gauge(df.pos.display$X30kg, df.player.display$X30kg)
    X40kg = f.get_value4gauge(df.pos.display$X40kg, df.player.display$X40kg)
    X60kg = f.get_value4gauge(df.pos.display$X60kg, df.player.display$X60kg)
    X80kg = f.get_value4gauge(df.pos.display$X80kg, df.player.display$X80kg)
    X100kg = f.get_value4gauge(df.pos.display$X100kg, df.player.display$X100kg)

    grid.arrange(arrangeGrob(gg.gauge(PC[4], PC[3], PC[1], PC[2], "PowerClean", df.player.display$Date),
                             gg.gauge(HC[4], HC[3], HC[1], HC[2], "HangClean", df.player.display$Date),
                             gg.gauge(MC[4], MC[3], MC[1], MC[2], "MuscleClean", df.player.display$Date),
                             gg.gauge(X20KG[4], X20KG[3], X20KG[1], X20KG[2], "Jump Squat 20KG", df.player.display$Date),
                             gg.gauge(X30kg[4], X30kg[3], X30kg[1], X30kg[2], "Jump Squat 30KG", df.player.display$Date),
                             gg.gauge(X40kg[4], X40kg[3], X40kg[1], X40kg[2], "Jump Squat 40KG", df.player.display$Date),
                             gg.gauge(X60kg[4], X60kg[3], X60kg[1], X60kg[2], "Bench Press 60KG", df.player.display$Date),
                             gg.gauge(X80kg[4], X80kg[3], X80kg[1], X80kg[2], "Bench Press 80KG", df.player.display$Date),
                             gg.gauge(X100kg[4], X100kg[3], X100kg[1], X100kg[2], "Bench Press 100KG", df.player.display$Date),
                             ncol=3))
  })
  
  output$Speed <- renderPlot({
    datarange = as.Date(input$dateRange, "%d/%m/%y")
    df.player.display = df.playerprofiles[which(df.playerprofiles$Player == input$sel_player &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                                  as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                          c(1, 4, 26:29)]
    
    df.pos.display = df.playerprofiles[which(df.playerprofiles$Position == df.player.display$Position[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") >= datarange[1] &
                                               as.Date(df.playerprofiles$Date, "%d/%m/%y") <= datarange[2]),
                                       c(1, 4, 26:29)]    
    
    X10m = f.get_value4gauge(df.pos.display$X10m, df.player.display$X10m)
    X20m = f.get_value4gauge(df.pos.display$X20m, df.player.display$X20m)
    X30m = f.get_value4gauge(df.pos.display$X30m, df.player.display$X30m)
    X40m = f.get_value4gauge(df.pos.display$X40m, df.player.display$X40m)

    grid.arrange(arrangeGrob(gg.gauge(X10m[4], X10m[3], X10m[1], X10m[2], "Rolling Speed 10m",
                                      df.player.display$Date, opposite = 1),
                             gg.gauge(X20m[4], X20m[3], X20m[1], X20m[2], "Rolling Speed 20m",
                                      df.player.display$Date, opposite = 1),
                             gg.gauge(X30m[4], X30m[3], X30m[1], X30m[2], "Rolling Speed 30m",
                                      df.player.display$Date, opposite = 1),
                             gg.gauge(X40m[4], X40m[3], X40m[1], X40m[2], "Rolling Speed 40m",
                                      df.player.display$Date, opposite = 1),
                             ncol=2))
  })
  
  # Rawdata
  output$DefenceTable <- renderDataTable({df.defence}, options = list(scrollX = TRUE))
  output$BreakdownTable <- renderDataTable({df.breakdown}, options = list(scrollX = TRUE))
  output$CarriesTable <- renderDataTable({df.carries}, options = list(scrollX = TRUE))
  output$ProfileTable <- renderDataTable({df.playerprofiles}, options = list(scrollX = TRUE))
  
  # Analytics
  output$CorDefence <- renderPlot({
    if (input$sel_positon_cordef != "ALL")
    {
      df.def.cor = df.def.cor[which(df.def.cor$Position == input$sel_positon_cordef), ]
    }
    df.def.cor[, 4:12] = round(df.def.cor[, 4:12] / df.def.cor[, 3], digit = 5) 
    df.CorDenfence = df.def.cor[, c(4:12, 15:41)]
    corr = cor(df.CorDenfence, use = "pairwise.complete.obs")
    corrplot(corr[1:9,-(1:9)], method="circle")
  })
  
  output$CorCarries <- renderPlot({
    if (input$sel_positon_corcar != "ALL")
    {
      df.car.cor = df.car.cor[which(df.car.cor$Position == input$sel_positon_corcar), ]
    }
    df.car.cor[, 5:15] = round(df.car.cor[, 5:15] / df.def.cor[, 4], digit = 5) 
    df.CorCarries = df.car.combine[, c(5:15, 18:44)]
    corr = cor(df.CorCarries, use = "pairwise.complete.obs")
    corrplot(corr[1:11,-(1:11)], method="circle")
  })
  
  output$CorBreakdown <- renderPlot({
    if (input$sel_positon_corbrk != "ALL")
    {
      df.brk.cor = df.brk.cor[which(df.brk.cor$Position == input$sel_positon_corbrk), ]
    }
    df.brk.cor[, 4:11] = round(df.brk.cor[, 4:11] / df.def.cor[, 3], digit = 5) 
    df.CorBreakdown = df.brk.combine[, c(4:11, 14:40)]
    corr = cor(df.CorBreakdown, use = "pairwise.complete.obs")
    corrplot(corr[1:8,-(1:8)], method="circle")
  })
}
