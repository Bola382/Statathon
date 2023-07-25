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

# summary for numeric data
summary(data[,5:9])
cor(data[,5:9])

# numeric data by
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
# mean lenght
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

################################################################################
# mean width
################################################################################

# boxplots

boxplot(data$meanWidth~data$season)
boxplot(data$meanWidth~data$treatment)
boxplot(data$meanWidth~data$block)
boxplot(data$meanWidth~data$harvest)

# means

tmp_byseason = tapply(data$meanWidth,data$season,mean)
tmp_bytreatment = tapply(data$meanWidth,data$treatment,mean)
tmp_byblock = tapply(data$meanWidth,data$block,mean)
tmp_byharvest = tapply(data$meanWidth,data$harvest,mean)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

# standard deviation

tmp_byseason = tapply(data$meanWidth,data$season,sd)
tmp_bytreatment = tapply(data$meanWidth,data$treatment,sd)
tmp_byblock = tapply(data$meanWidth,data$block,sd)
tmp_byharvest = tapply(data$meanWidth,data$harvest,sd)

plot(tmp_byseason,type="o")
plot(tmp_bytreatment,type="o")
plot(tmp_byblock,type="o")
plot(tmp_byharvest,type="o")

# 

tmp = data %>% filter(season=="P-V") %>% as.data.frame
tmp_matrix = matrix(NA, nrow = dim(tmp)[1], ncol = 10)

for(i in 1:nrow(tmp_matrix)){
 for(j in 1:10){
  tmp_matrix[i,tmp$harvest[i]] = tmp[i,"Number"]
 }
}

