rm(list = ls())
library(ggplot2)
library(scales)
setwd("C:/Users/user/Documents/Fiverr projects/Housing Price")
dataFrame = read.csv("CountyInfo.csv")
tolerance = 35 #50 is good

dataTemp = subset(dataFrame, County.Name == "El Paso County")

dataFrame$Two.Bedroom<-gsub(" ","",as.character(dataFrame$Two.Bedroom))
dataFrame$Two.Bedroom<-gsub(",","",as.character(dataFrame$Two.Bedroom))
dataFrame$Two.Bedroom<-gsub("$","",as.character(dataFrame$Two.Bedroom))
dataFrame$Two.Bedroom = as.numeric(substring(dataFrame$Two.Bedroom, 2))
dataFrame$RatioTwoBed = dataFrame$Two.Bedroom/dataFrame$Median.Home.Price.Q4.2021

dataFrame$Efficiency<-gsub(" ","",as.character(dataFrame$Efficiency))
dataFrame$Efficiency<-gsub(",","",as.character(dataFrame$Efficiency))
dataFrame$Efficiency<-gsub("$","",as.character(dataFrame$Efficiency))
dataFrame$Efficiency = as.numeric(substring(dataFrame$Efficiency, 2))
dataFrame$RatioEff = dataFrame$Efficiency/dataFrame$Median.Home.Price.Q4.2021

dataFrame$One.Bedroom<-gsub(" ","",as.character(dataFrame$One.Bedroom))
dataFrame$One.Bedroom<-gsub(",","",as.character(dataFrame$One.Bedroom))
dataFrame$One.Bedroom<-gsub("$","",as.character(dataFrame$One.Bedroom))
dataFrame$One.Bedroom = as.numeric(substring(dataFrame$One.Bedroom, 2))
dataFrame$RatioOneBed = dataFrame$One.Bedroom/dataFrame$Median.Home.Price.Q4.2021

dataFrame$Three.Bedroom<-gsub(" ","",as.character(dataFrame$Three.Bedroom))
dataFrame$Three.Bedroom<-gsub(",","",as.character(dataFrame$Three.Bedroom))
dataFrame$Three.Bedroom<-gsub("$","",as.character(dataFrame$Three.Bedroom))
dataFrame$Three.Bedroom = as.numeric(substring(dataFrame$Three.Bedroom, 2))
dataFrame$RatioThreeBed = dataFrame$Three.Bedroom/dataFrame$Median.Home.Price.Q4.2021

dataFrame$Four.Bedroom<-gsub(" ","",as.character(dataFrame$Four.Bedroom))
dataFrame$Four.Bedroom<-gsub(",","",as.character(dataFrame$Four.Bedroom))
dataFrame$Four.Bedroom<-gsub("$","",as.character(dataFrame$Four.Bedroom))
dataFrame$Four.Bedroom = as.numeric(substring(dataFrame$Four.Bedroom, 2))
dataFrame$RatioFourBed = dataFrame$Four.Bedroom/dataFrame$Median.Home.Price.Q4.2021

#two bedroom 
model = lm(Median.Home.Price.Q4.2021~Two.Bedroom, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:6]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Two.Bedroom > dataFrame$Two.Bedroom[i]-tolerance & Two.Bedroom < dataFrame$Two.Bedroom[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioTwoBed > .01)
dataPlot4 = subset(dataFrame, RatioTwoBed > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = Two.Bedroom, y = Median.Home.Price.Q4.2021, color = RatioTwoBed)) + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Two.Bedroom, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Two.Bedroom, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1)+ ggtitle("Two Bedroom") + scale_color_viridis_c(option = "magma")

#+ geom_point(data = dataPlot3, aes(x = Two.Bedroom, y = Median.Home.Price.Q4.2021), color = "yellow", size = 1.25) 

#ggplot(dataFrame, aes(x = RatioTwoBed, y = 1)) + geom_point(size = 3) + geom_point(data = dataPlot2, aes(x = RatioTwoBed, y = 1), color = "green", size = 5) + geom_vline(xintercept = .01, linetype= "dashed", color  = 'red', size= 1.5)+ xlab("HUD FMR / Median House Price") + theme_bw() + theme(text = element_text(size = 14)) +  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) + ylab("") + geom_point(data = dataPlot4, aes(x = RatioTwoBed, y = 1), color = "yellow", size = 3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Two Bedroom") 

dataTwoBed = subset(dataFrame, RatioTwoBed > .01 & Optimal == "P")
dataTwoBed$OptimalityType = "1%"
dataPlot2$OptimalityType = "LR"
dataTwoBed <- rbind(dataTwoBed, dataPlot2)
write.csv(dataTwoBed, "OptimalTwoBedroom.csv")

#efficiency 
rm(list=setdiff(ls(), c("dataFrame", "dataTwoBed", "tolerance")))
model = lm(Median.Home.Price.Q4.2021~Efficiency, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:9]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Efficiency > dataFrame$Efficiency[i]-tolerance & Efficiency < dataFrame$Efficiency[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioEff > .01)
dataPlot4 = subset(dataFrame, RatioEff > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = Efficiency, y = Median.Home.Price.Q4.2021, color = RatioEff))  + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Efficiency, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Efficiency, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1) + ggtitle("Efficiency") + scale_color_viridis_c(option = "magma")

dataEfficiency = subset(dataFrame, RatioEff > .01 & Optimal == "P")
dataEfficiency$OptimalityType = "1%"
dataPlot2$OptimalityType = "LR"
dataEfficiency <- rbind(dataEfficiency, dataPlot2)
write.csv(dataEfficiency, "OptimalEfficiency.csv")

#one bedroom
rm(list=setdiff(ls(), c("dataFrame", "dataTwoBed", "tolerance", "dataEfficiency")))
model = lm(Median.Home.Price.Q4.2021~One.Bedroom, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:7]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, One.Bedroom > dataFrame$One.Bedroom[i]-tolerance & One.Bedroom < dataFrame$One.Bedroom[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioOneBed > .01)
dataPlot4 = subset(dataFrame, RatioOneBed > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = One.Bedroom, y = Median.Home.Price.Q4.2021, color = RatioOneBed))  + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = One.Bedroom, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = One.Bedroom, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1) + ggtitle("One Bedroom") + scale_color_viridis_c(option = "magma")

dataOneBed = subset(dataFrame, RatioOneBed > .01 & Optimal == "P")
dataOneBed$OptimalityType = "1%"
dataPlot2$OptimalityType = "LR"
dataOneBed <- rbind(dataOneBed, dataPlot2)
write.csv(dataOneBed, "OptimalOneBedroom.csv")

#three bedroom
rm(list=setdiff(ls(), c("dataFrame", "dataTwoBed", "tolerance", "dataEfficiency", "dataOneBed")))
model = lm(Median.Home.Price.Q4.2021~Three.Bedroom, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:7]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Three.Bedroom > dataFrame$Three.Bedroom[i]-tolerance & Three.Bedroom < dataFrame$Three.Bedroom[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioThreeBed > .01)
dataPlot4 = subset(dataFrame, RatioThreeBed > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = Three.Bedroom, y = Median.Home.Price.Q4.2021, color = RatioThreeBed))  + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Three.Bedroom, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Three.Bedroom, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1) + ggtitle("Three Bedroom") + scale_color_viridis_c(option = "magma")

dataThreeBed = subset(dataFrame, RatioThreeBed > .01 & Optimal == "P")
dataThreeBed$OptimalityType = "1%"
dataPlot2$OptimalityType = "LR"
dataThreeBed <- rbind(dataThreeBed, dataPlot2)
write.csv(dataThreeBed, "OptimalThreeBedroom.csv")

#four bedroom
rm(list=setdiff(ls(), c("dataFrame", "dataTwoBed", "tolerance", "dataEfficiency", "dataOneBed", "dataThreeBed")))
model = lm(Median.Home.Price.Q4.2021~Four.Bedroom, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:8]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Four.Bedroom > dataFrame$Four.Bedroom[i]-tolerance & Four.Bedroom < dataFrame$Four.Bedroom[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioFourBed > .01)
dataPlot4 = subset(dataFrame, RatioFourBed > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = Four.Bedroom, y = Median.Home.Price.Q4.2021, color = RatioFourBed))  + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Four.Bedroom, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Four.Bedroom, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1) + ggtitle("Four Bedroom") + scale_color_viridis_c(option = "magma")

dataFourBed = subset(dataFrame, RatioFourBed > .01 & Optimal == "P")
dataFourBed$OptimalityType = "1%"
dataPlot2$OptimalityType = "LR"
dataFourBed <- rbind(dataFourBed, dataPlot2)
write.csv(dataFourBed, "OptimalFourBedroom.csv")

#cross section 
rm(list=setdiff(ls(), c("dataFrame", "dataTwoBed", "dataFourBed", "dataEfficiency", "dataOneBed", "dataThreeBed")))
dataOneBed$Size = "One Bed"
dataTwoBed$Size = "Two Bed"
dataThreeBed$Size = "Three Bed"
dataFourBed$Size = "Four Bed"
dataEfficiency$Size = "Efficiency"

dataCombined = rbind(dataOneBed, dataTwoBed, dataThreeBed, dataFourBed, dataEfficiency)
dataCombined$Place = paste(dataCombined$..State, dataCombined$County.Name)
dataCombined1 = subset(dataCombined, OptimalityType == "1%")
dataCombinedLR = subset(dataCombined, OptimalityType == "LR")

Overall = data.frame(table(dataCombined$Place))
OnePercent = data.frame(table(dataCombined1$Place))
LR = data.frame(table(dataCombinedLR$Place))

write.csv(dataCombined, "OptimalLocationsFullDataset.csv")
write.csv(OnePercent, "OptimalLocationsCountsYellow.csv")
write.csv(LR, "OptimalLocationsCountsGreen.csv")

ggplot(Overall, aes(x = reorder(Var1, -Freq), y = Freq)) + geom_bar(stat = "identity") + xlab("") + theme_bw()  + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + ylab("Count")+ 
  scale_y_continuous(breaks= pretty_breaks()) + ggtitle("Overall Count")

common <- intersect(OnePercent$Var1, LR$Var1)  
commonOnePercent = subset(OnePercent, Var1 %in% common)
commonLR = subset(LR, Var1 %in% common)
commonOnePercent$OptimalityType = "One Percent"
commonLR$OptimalityType = "LR"
commonData = rbind(commonLR, commonOnePercent)

ggplot(commonData, aes(x = reorder(Var1, -Freq), y = Freq, fill = OptimalityType)) + geom_bar(stat = "identity") + xlab("") + theme_bw()  + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + ylab("Count")+ scale_y_continuous(breaks= pretty_breaks()) 

#average
rm(list=setdiff(ls(), c("dataFrame", "tolerance")))
tolerance = 35
dataFrame$Average = (dataFrame$Efficiency + dataFrame$One.Bedroom + dataFrame$Two.Bedroom+ dataFrame$Three.Bedroom+ dataFrame$Four.Bedroom)/5
dataFrame$RatioAverage = dataFrame$Average/dataFrame$Median.Home.Price.Q4.2021
model = lm(Median.Home.Price.Q4.2021~Average, data = dataFrame)
summary(model)

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:8]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Average > dataFrame$Average[i]-tolerance & Average < dataFrame$Average[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioAverage > .01)
dataPlot4 = subset(dataFrame, RatioAverage > .01 & Optimal == "P")

ggplot(dataFrame, aes(x = Average, y = Median.Home.Price.Q4.2021, color = RatioAverage))  + guides(color=guide_legend(title="FMR/Home Price")) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + theme(text = element_text(size = 14))  + theme(aspect.ratio=1) + ggtitle("Average") + scale_color_viridis_c(option = "magma")

dataFrame <- dataFrame[order(-dataFrame$Median.Home.Price.Q4.2021),]
drop <- c("Residuals")
dataFrame = dataFrame[,!(names(dataFrame) %in% drop)]
write.csv(dataFrame, "FullDataset.csv")

###population average

rm(list=setdiff(ls(), c("dataFrame", "tolerance")))
tolerance = 35
dataFrame$Average = (dataFrame$Efficiency + dataFrame$One.Bedroom + dataFrame$Two.Bedroom+ dataFrame$Three.Bedroom+ dataFrame$Four.Bedroom)/5
dataFrame$RatioAverage = dataFrame$Average/dataFrame$Median.Home.Price.Q4.2021
model = lm(Median.Home.Price.Q4.2021~Average, data = dataFrame)
summary(model)
popThreshold = 20000

dataFrame$Residuals = resid(model)
lowestRes = sort(dataFrame$Residuals)[1:8]
flag = c()
resVec = c()

for(i in 1:nrow(dataFrame)){
  if(dataFrame$Residuals[i]>0){
    flag[i] = "NP"
  } else {
    dataTemp = subset(dataFrame, Average > dataFrame$Average[i]-tolerance & Average < dataFrame$Average[i]+tolerance )
    if(dataFrame$Median.Home.Price.Q4.2021[i] == min(dataTemp$Median.Home.Price.Q4.2021)){
      flag[i] = "P"
    } else {
      flag[i] = "NP"
    }
  }
  print(i/nrow(dataFrame))
  
  if(dataFrame$Residuals[i] %in% lowestRes){
    resVec[i] = "min"
  } else {
    resVec[i] = "max"
  }
  
}


dataFrame$Optimal = flag
dataFrame$BestRes = resVec
dataFrame$Population<-gsub(",","",as.character(dataFrame$Population))
dataFrame$Population = as.numeric(dataFrame$Population)

dataPlot = subset(dataFrame, Optimal == "P")
dataPlot2 = subset(dataFrame, BestRes == "min" & Optimal == "P")
dataPlot3 = subset(dataFrame, RatioAverage > .01)
dataPlot4 = subset(dataFrame, RatioAverage > .01 & Optimal == "P")
dataPlot5 = subset(dataFrame, Population > popThreshold & RatioAverage > .01)

ggplot(dataFrame, aes(x = Average, y = Median.Home.Price.Q4.2021)) + geom_point(size = 1.5) + geom_smooth(method = 'lm',se=F, size = 1.25) + geom_line(data = dataPlot, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "green", size = 3) + theme_bw() + xlab("HUD FMR ($)") + ylab("Median Home Price ($)") + ggtitle("Average") + geom_point(data = dataPlot3, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "yellow", size = 1.25) + geom_point(data = dataPlot5, aes(x = Average, y = Median.Home.Price.Q4.2021), color = "gray", size = 1.25) + theme_bw()+ theme(aspect.ratio=1)  + theme(text = element_text(size = 14))

ggplot(dataFrame, aes(x = log(Population), y = RatioAverage)) + geom_point(size = 2) + geom_vline(xintercept = log(popThreshold), linetype="dashed", color = "blue", size=1.25) + theme_bw()+ geom_hline(yintercept = .01, linetype="dashed", color = "blue", size=1.25) + geom_point(data = dataPlot2, aes(x = log(Population), y = RatioAverage), color = "green", size = 4) + geom_point(data = dataPlot, aes(x = log(Population), y = RatioAverage), color = "red", size = 2) + annotate("rect", xmin = log(popThreshold), xmax = max(log(dataFrame$Population)), ymin = .01, ymax = max(dataFrame$RatioAverage),alpha = .2, fill = "orange") + ylab("FMR / Home Price")  + theme(text = element_text(size = 14))

finalCounty = subset(dataFrame, Population > popThreshold & RatioAverage > .01 & Optimal == "P")
finalCounties = subset(dataFrame, Population > popThreshold & RatioAverage > .01)

paretoRatio = c()
tolerance = .5

for(i in 1:nrow(dataFrame)){

    dataTemp = subset(dataFrame, log(dataFrame$Population) > log(dataFrame$Population[i])-tolerance & log(dataFrame$Population) < log(dataFrame$Population[i])+tolerance )
    if(dataFrame$RatioAverage[i] == max(dataTemp$RatioAverage)){
      paretoRatio[i] = "P"
    } else {
      paretoRatio[i] = "NP"
    }
  print(i/nrow(dataFrame))
  
}

dataFrame$paretoRatio = paretoRatio
dataPlot = subset(dataFrame, paretoRatio == "P")
dataPlotOhio = subset(dataFrame, County.Name == "Cuyahoga County")

ggplot(dataFrame, aes(x = log(Population), y = RatioAverage)) + geom_point(size = 2) + geom_vline(xintercept = log(popThreshold), linetype="dashed", color = "blue", size=1.25) + theme_bw()+ geom_hline(yintercept = .01, linetype="dashed", color = "blue", size=1.25) + geom_point(data = dataPlot, aes(x = log(Population), y = RatioAverage), color = "red", size = 2) + annotate("rect", xmin = log(popThreshold), xmax = max(log(dataFrame$Population)), ymin = .01, ymax = max(dataFrame$RatioAverage),alpha = .2, fill = "orange") + ylab("FMR / Home Price")  + theme(text = element_text(size = 14)) + geom_line(data = dataPlot, aes(x = log(Population), y = RatioAverage), color = "red", size = 1.25) + geom_point(data = dataPlotOhio, aes(x = log(Population), y = RatioAverage), color = "orange", size = 5)

###population average of 3 and 4 bedroom 

rm(list=setdiff(ls(), c("dataFrame", "tolerance")))

library(scales)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

tolerance = .5
dataFrame$Average3and4 = (dataFrame$Three.Bedroom+ dataFrame$Four.Bedroom)/2
dataFrame$RatioAverage3and4 = dataFrame$Average3and4/dataFrame$Median.Home.Price.Q4.2021
popThreshold = 20000
ratioThrshold = .01
#dataFrame$ScaledRatio = min_max_norm(dataFrame$RatioAverage3and4)
#dataFrame$ScaledPop = min_max_norm(log(dataFrame$RatioAverage3and4))
dataFrame$ScaledRatio = rescale(dataFrame$RatioAverage3and4, to = c(0,1))
dataFrame$ScaledPop = rescale((dataFrame$Population), to = c(0,1))
dataPlotOhio = subset(dataFrame, County.Name == "Cuyahoga County")
dataFrame$EuclidDistance = sqrt((dataFrame$ScaledRatio-dataPlotOhio$ScaledRatio)^2+(dataFrame$ScaledPop-dataPlotOhio$ScaledPop)^2)

distanceThreshold = max(sort(dataFrame$EuclidDistance)[1:11])

paretoRatio = c()
thresholdVec = c()
similarityVec = c()
color = c()

for(i in 1:nrow(dataFrame)){
  
  dataTemp = subset(dataFrame, log(dataFrame$Population) > log(dataFrame$Population[i])-tolerance & log(dataFrame$Population) < log(dataFrame$Population[i])+tolerance )
  if(dataFrame$RatioAverage3and4[i] == max(dataTemp$RatioAverage3and4)){
    paretoRatio[i] = "P"
  } else {
    paretoRatio[i] = "NP"
  }
  
  if(dataFrame$RatioAverage3and4[i] > ratioThrshold & dataFrame$Population[i] > popThreshold){
    thresholdVec[i] = "Optimal"
  } else {
    thresholdVec[i] = "Not Optimal"
  }
  
  if(dataFrame$EuclidDistance[i] <= distanceThreshold){
    similarityVec[i] = "Similiar"
  } else {
    similarityVec[i] = "Not Similiar"
  }
  
  if(similarityVec[i] == "Similiar"){
    color[i] = "Orange"
  } else if(thresholdVec[i] == "Optimal" & paretoRatio[i] == "P"){
    color[i] = "Red and Green"
  } else if(thresholdVec[i] == "Optimal"){
    color[i] = "Green"
  } else if(paretoRatio[i] == "P"){
    color[i] = "Red"
  } else {
    color[i] = "Black"
  }
  
  print(i/nrow(dataFrame))
  
}

dataFrame$paretoRatio = paretoRatio
dataFrame$thresholdVec = thresholdVec
dataFrame$similarityVec = similarityVec
dataFrame$Color = color
dataPlot = subset(dataFrame, paretoRatio == "P")
dataPlot2 = subset(dataFrame, thresholdVec == "Optimal")
dataPlot3 = subset(dataFrame, similarityVec == "Similiar")

ggplot(dataFrame, aes(x = log(Population), y = RatioAverage3and4)) + geom_point(size = 2) + geom_vline(xintercept = log(popThreshold), linetype="dashed", color = "blue", size=1.25) + theme_bw()+ geom_hline(yintercept = .01, linetype="dashed", color = "blue", size=1.25) + geom_point(data = dataPlot, aes(x = log(Population), y = RatioAverage3and4), color = "red", size = 2) + annotate("rect", xmin = log(popThreshold), xmax = max(log(dataFrame$Population)), ymin = .01, ymax = max(dataFrame$RatioAverage3and4),alpha = .2, fill = "orange") + ylab("FMR / Home Price")  + theme(text = element_text(size = 14)) + geom_line(data = dataPlot, aes(x = log(Population), y = RatioAverage3and4), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = log(Population), y = RatioAverage3and4), color = "green", size = 1.25) + geom_point(data = dataPlot3, aes(x = log(Population), y = RatioAverage3and4), color = "orange", size = 1.25) + geom_point(data = dataPlotOhio, aes(x = log(Population), y = RatioAverage3and4), shape = 18, size = 6) + geom_point(data = dataPlotOhio, aes(x = log(Population), y = RatioAverage3and4), shape = 18,color = "orange", size = 5)

empty_columns <- colSums(is.na(dataFrame) | dataFrame == "") == nrow(dataFrame)
dataFrame = dataFrame[, !empty_columns]
popThreshold = 20000

dataSubset = subset(dataFrame, Color != "Black")

write.csv(dataFrame, "FullDataset.csv")
write.csv(dataSubset, "OptimalDataset.csv")

### Only subset

states = c("Indiana", "Alabama", "Illinois", "Pennsylvania", "Ohio", "Georgia", "Mississippi", "Kentucky", "Michigan", "Arkansas", "Tennessee", "Missouri")

dataFrameSub = subset(dataFrame, ?..State %in% states)

#write.csv(dataFrameSub, "SubsetStatesFullDataset.csv")
tolerance = .3

distanceThreshold = max(sort(dataFrameSub$EuclidDistance)[1:11])

paretoRatio = c()
thresholdVec = c()
similarityVec = c()
color = c()

for(i in 1:nrow(dataFrameSub)){
  
  dataTemp = subset(dataFrameSub, log(dataFrameSub$Population) > log(dataFrameSub$Population[i])-tolerance & log(dataFrameSub$Population) < log(dataFrameSub$Population[i])+tolerance )
  if(dataFrameSub$RatioAverage3and4[i] == max(dataTemp$RatioAverage3and4)){
    paretoRatio[i] = "P"
  } else {
    paretoRatio[i] = "NP"
  }
  
  if(dataFrameSub$RatioAverage3and4[i] > ratioThrshold & dataFrameSub$Population[i] > popThreshold){
    thresholdVec[i] = "Optimal"
  } else {
    thresholdVec[i] = "Not Optimal"
  }
  
  if(dataFrameSub$EuclidDistance[i] <= distanceThreshold){
    similarityVec[i] = "Similiar"
  } else {
    similarityVec[i] = "Not Similiar"
  }
  
  if(similarityVec[i] == "Similiar"){
    color[i] = "Orange"
  } else if(thresholdVec[i] == "Optimal" & paretoRatio[i] == "P"){
    color[i] = "Red and Green"
  } else if(thresholdVec[i] == "Optimal"){
    color[i] = "Green"
  } else if(paretoRatio[i] == "P"){
    color[i] = "Red"
  } else if(similarityVec[i] == "Similiar" & thresholdVec[i] == "Optimal") {
    color[i] = "Red and Orange"
  } else{
    color[i] = "Black"
  }
  
  print(i/nrow(dataFrameSub))
  
}

dataFrameSub$paretoRatio = paretoRatio
dataFrameSub$thresholdVec = thresholdVec
dataFrameSub$similarityVec = similarityVec
dataFrameSub$Color = color
dataPlot = subset(dataFrameSub, paretoRatio == "P")
dataPlot2 = subset(dataFrameSub, thresholdVec == "Optimal")
dataPlot3 = subset(dataFrameSub, similarityVec == "Similiar")

dataSubsetFinal = subset(dataFrameSub, Color != "Black")
dataSubsetFinal = dataSubsetFinal[order(dataSubsetFinal$?..State),]
dataSubsetFinal = subset(dataSubsetFinal, County.Name != "St. Francis County")
dataSubsetFinal = subset(dataSubsetFinal, County.Name != "LaSalle County")
dataSubsetFinal = subset(dataSubsetFinal, County.Name != "Shelby County")
dataSubsetFinal = subset(dataSubsetFinal, County.Name != "Saline County")
dataSubsetFinal = subset(dataSubsetFinal, County.Name != "Scott County")

ggplot(dataFrameSub, aes(x = log(Population), y = RatioAverage3and4)) + geom_point(size = 2) + geom_vline(xintercept = log(popThreshold), linetype="dashed", color = "blue", size=1.25) + theme_bw()+ geom_hline(yintercept = .01, linetype="dashed", color = "blue", size=1.25) + geom_point(data = dataPlot, aes(x = log(Population), y = RatioAverage3and4), color = "red", size = 2) + annotate("rect", xmin = log(popThreshold), xmax = max(log(dataFrameSub$Population)), ymin = .01, ymax = max(dataFrameSub$RatioAverage3and4),alpha = .2, fill = "orange") + ylab("FMR / Home Price")  + theme(text = element_text(size = 14)) + geom_line(data = dataPlot, aes(x = log(Population), y = RatioAverage3and4), color = "red", size = 1.25) + geom_point(data = dataPlot2, aes(x = log(Population), y = RatioAverage3and4), color = "green", size = 1.25) + geom_point(data = dataPlot3, aes(x = log(Population), y = RatioAverage3and4), color = "orange", size = 1.25) + geom_point(data = dataPlotOhio, aes(x = log(Population), y = RatioAverage3and4), shape = 18, size = 6) + geom_point(data = dataPlotOhio, aes(x = log(Population), y = RatioAverage3and4), shape = 18,color = "orange", size = 5)

write.csv(dataSubsetFinal, "SubsetOptimalStates.csv")
