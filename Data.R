setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)

data <- readxl::read_excel("Data/Tomate.xlsx", skip = 1,
                           col_names = c("season","treatment","block","harvest",
                                         "meanWeight","meanNumber","meanLength","meanWidth"))
head(data)

data = data %>% mutate(Number = 10*meanNumber)

freq = function(data){
 tab = table(data)
 prop.tab = prop.table(tab)
 names(prop.tab)=NULL
 
 out = cbind(tab,round(prop.tab,2))
 colnames(out) = c("freq","rel.freq")
 
 return(out)
}

# var 1: season
freq(data$season)

# var 2: treatment
freq(data$treatment)

# var 3: block
freq(data$block)

# var 4: harvest
freq(data$harvest)

################################################################################
# number of fruits
################################################################################

# boxplots

boxplot(data$Number~data$season)
boxplot(data$Number~data$treatment)
boxplot(data$Number~data$block)
boxplot(data$Number~data$harvest)

 # means

tmp_byseason = tapply(data$Number,data$season,mean)
tmp_bytreatment = tapply(data$Number,data$treatment,mean)
tmp_byblock = tapply(data$Number,data$block,mean)
tmp_byharvest = tapply(data$Number,data$harvest,mean)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

# standard deviation

tmp_byseason = tapply(data$Number,data$season,sd)
tmp_bytreatment = tapply(data$Number,data$treatment,sd)
tmp_byblock = tapply(data$Number,data$block,sd)
tmp_byharvest = tapply(data$Number,data$harvest,sd)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

################################################################################
# mean weight
################################################################################

# boxplots

boxplot(data$meanWeight~data$season)
boxplot(data$meanWeight~data$treatment)
boxplot(data$meanWeight~data$block)
boxplot(data$meanWeight~data$harvest)

# means

tmp_byseason = tapply(data$meanWeight,data$season,mean)
tmp_bytreatment = tapply(data$meanWeight,data$treatment,mean)
tmp_byblock = tapply(data$meanWeight,data$block,mean)
tmp_byharvest = tapply(data$meanWeight,data$harvest,mean)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

# standard deviation

tmp_byseason = tapply(data$meanWeight,data$season,sd)
tmp_bytreatment = tapply(data$meanWeight,data$treatment,sd)
tmp_byblock = tapply(data$meanWeight,data$block,sd)
tmp_byharvest = tapply(data$meanWeight,data$harvest,sd)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

################################################################################
# mean weight
################################################################################

# boxplots

boxplot(data$meanLength~data$season)
boxplot(data$meanLength~data$treatment)
boxplot(data$meanLength~data$block)
boxplot(data$meanLength~data$harvest)

# means

tmp_byseason = tapply(data$meanLength,data$season,mean)
tmp_bytreatment = tapply(data$meanLength,data$treatment,mean)
tmp_byblock = tapply(data$meanLength,data$block,mean)
tmp_byharvest = tapply(data$meanLength,data$harvest,mean)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

# standard deviation

tmp_byseason = tapply(data$meanLength,data$season,sd)
tmp_bytreatment = tapply(data$meanLength,data$treatment,sd)
tmp_byblock = tapply(data$meanLength,data$block,sd)
tmp_byharvest = tapply(data$meanLength,data$harvest,sd)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

