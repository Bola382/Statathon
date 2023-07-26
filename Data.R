setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)

data <- readxl::read_excel("Data/Tomate.xlsx", skip = 1,
                           col_names = c("season","treatment","block","harvest",
                                         "Weight","meanNumber","meanLength","meanWidth"))
head(data)

data = data %>% mutate(Number = 10*meanNumber, meanWeight = Weight/Number)

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

# tests

# 1,3,5,7,9

tmp = data %>% filter(harvest==10)
tmp$treatment = tmp$treatment %>% as.factor
tmp$block = tmp$block %>% as.factor

boxplot(tmp$meanWeight~tmp$treatment)
vioplot::vioplot(tmp$meanWeight~tmp$treatment)

library(ggplot2)
p <- ggplot(tmp, aes(x=treatment, y=meanWeight)) + 
 geom_violin(trim=FALSE) + geom_boxplot(width=0.1)
p

MVN::mvn(select(tmp,!c(Weight,meanNumber))[,5:8], univariateTest = "SW")

aa = manova(lm(cbind(tmp$meanLength,tmp$meanNumber,tmp$meanWeight,tmp$meanWidth) ~ factor(tmp$block)+factor(tmp$treatment)))

summary(aa)

aa = aov(tmp$meanWeight~factor(tmp$block)+factor(tmp$treatment))
hist(aa$res)
shapiro.test(residuals(aa))
plot(residuals(aa))
?bartlett.test(aa$res~tmp$treatment)
bartlett.test(aa$res~tmp$block)
plot(hnp::hnp(aa))
lmtest::dwtest(aa)

anova(aa)

tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

xtabs(tmp$Number~tmp_trat+tmp_block)

dumie = makedummies::makedummies(data.frame(tmp_trat,tmp_block))
dumie = cbind(1,dumie)

mod = glm(tmp$Number~dumie+0, family = poisson())
m1 <- MASS::glm.nb(tmp$Number~dumie+0)
m2 = lme4::lmer(meanWeight~(1|treatment)+(1|block),data=tmp)

summary(m1)

hist(m1$res)
plot(m1$res)
shapiro.test(m1$res)
plot(hnp::hnp(m1))

boxplot(tmp$meanWeight~tmp_block)

boxplot(tmp$meanWeight~tmp$treatment)
aa = lm(tmp$meanWeight~factor(tmp$treatment)+factor(tmp$block))


