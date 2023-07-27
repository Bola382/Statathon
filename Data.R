setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(dplyr)

data <- readxl::read_excel("Data/Tomate.xlsx", skip = 1,
                           col_names = c("season","treatment","block","harvest",
                                         "Weight","meanNumber","meanLength","meanWidth"))
head(data)

data = data %>% mutate(success = ifelse(Weight==0,0,1),
                       Number = 10*meanNumber, 
                       meanWeight = ifelse(is.nan(Weight/Number),0,Weight/Number),
                       id = 1:nrow(data))

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

################################################################################
# mean weight
################################################################################

# boxplots

boxplot(data$meanWeight~data$season)
boxplot(data$meanWeight~data$treatment)
boxplot(data$meanWeight~data$block)
boxplot(data$meanWeight~data$harvest)

################################################################################
# mean lenght
################################################################################

# boxplots

boxplot(data$meanLength~data$season)
boxplot(data$meanLength~data$treatment)
boxplot(data$meanLength~data$block)
boxplot(data$meanLength~data$harvest)

################################################################################
# mean width
################################################################################

# boxplots

boxplot(data$meanWidth~data$season)
boxplot(data$meanWidth~data$treatment)
boxplot(data$meanWidth~data$block)
boxplot(data$meanWidth~data$harvest)

# tests



# by harvest
tmp = data %>% filter(season=="P-V", harvest==1)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==2)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==3)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==4)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==5)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==6)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==7)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==8)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==9)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

tmp = data %>% filter(season=="P-V", harvest==10)
tmp_trat = factor(tmp$treatment)
tmp_block = factor(tmp$block)

friedman.test(y = tmp$meanWeight, groups = tmp$treatment, blocks = tmp$block)

# cumulativo
tmp = data %>% filter(season=="P-V")
tmp$treatment = tmp$treatment %>% as.factor
tmp$block = tmp$block %>% as.factor

tmp2 = array(NA, dim = c(3,12,1))

for(i in 1:3){
 for(j in 1:12){
  tmp2[i,j,] = tmp %>% filter(block==i,treatment==j) %>% select(meanWeight) %>%sum
 }
}

tmp_trat3 = factor(rep(1:12,3))
tmp_block3 = factor(rep(1:3,each=12))

tmp_data3 = data.frame(block = tmp_block3, trat = tmp_trat3, "meanWheight")

for(i in 1:36){
 tmp_data3[i,3] = tmp2[tmp_data3[i,1],tmp_data3[i,2],]
}

tmp_resp = as.numeric(tmp_data3$X.meanWheight.)

aa = lm(tmp_resp ~ tmp_trat3+tmp_block3)
anova(aa)

shapiro.test(resid(aa))
plot(resid(aa))
hnp::hnp(aa)