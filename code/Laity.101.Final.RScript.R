library(car)
library(readr)
library(psych)
library(gplots)
library(rgl)
library(scatterplot3d)
library(Rcmdr)

#import downloaded datafile
mturk.data.raw <- read.csv("2015.11.22.PC.Results.MTurk.csv", 
                           sep = "\t", stringsAsFactors = F)

#needed good conversation values: entered right value on attention check
#AND didn't answer bad conversation attention check
attention.filter.good <- mturk.data.raw$good.ATTENTIONCHECK == 6 & 
  is.na(mturk.data.raw$bad.ATTENTIONCHECK)
#needed bad conversation values: entered right value on attention check
#AND didn't asnwer good conversation attention check
attention.filter.bad <- mturk.data.raw$bad.ATTENTIONCHECK == 6 &
  is.na(mturk.data.raw$good.ATTENTIONCHECK)
#make new dataframe with only approved subject data
mturk.data.df <- subset(mturk.data.raw, 
                        subset = attention.filter.good | attention.filter.bad)

#Reverse-scored: 2, 5, 6, 11, 12, 17, 18, 19
#there's a way to automate this with a reference vector and a grep generator 
#but the less moving parts the better
mturk.data.df$good.CSI.2r <- 7 - mturk.data.df$good.CSI.2

mturk.data.df$bad.CSI.2r <- 7 - mturk.data.df$bad.CSI.2

mturk.data.df$good.CSI.5r <- 7 - mturk.data.df$good.CSI.5

mturk.data.df$bad.CSI.5r <- 7 - mturk.data.df$bad.CSI.5

mturk.data.df$good.CSI.6r <- 7 - mturk.data.df$good.CSI.6

mturk.data.df$bad.CSI.6r <- 7 - mturk.data.df$bad.CSI.6

mturk.data.df$good.CSI.11r <- 7 - mturk.data.df$good.CSI.11

mturk.data.df$bad.CSI.11r <- 7 - mturk.data.df$bad.CSI.11

mturk.data.df$good.CSI.12r <- 7 - mturk.data.df$good.CSI.12

mturk.data.df$bad.CSI.12r <- 7 - mturk.data.df$bad.CSI.12

mturk.data.df$good.CSI.17r <- 7 - mturk.data.df$good.CSI.17

mturk.data.df$bad.CSI.17r <- 7 - mturk.data.df$bad.CSI.17

mturk.data.df$good.CSI.18r <- 7 - mturk.data.df$good.CSI.18

mturk.data.df$bad.CSI.18r <- 7 - mturk.data.df$bad.CSI.18

mturk.data.df$good.CSI.19r <- 7 - mturk.data.df$good.CSI.19

mturk.data.df$bad.CSI.19r <- 7 - mturk.data.df$bad.CSI.19

#create helper function to average good/bad CSI, positivity, insert into df
helper.function <- function(df){
  #have to calculate average cSI per subject -- traverse by row
  count <- nrow(df)
  #initialize variables, columns
  good.sum <- 0
  bad.sum <- 0
  df$good.CSI.AVG <- 0
  df$bad.CSI.AVG <- 0
  df$relationship.measure <- 0
  for(i in 1:count){
    #traverse by column to sum CSI; have to do this manually
    #not trying to head down the assign/get rabbit hole with grep & paste
    #good first
    good.sum <- sum(df$good.CSI.1[i], df$good.CSI.2r[i], df$good.CSI.3[i],
                    df$good.CSI.4[i], df$good.CSI.5r[i], df$good.CSI.6r[i],
                    df$good.CSI.7[i], df$good.CSI.8[i], df$good.CSI.9[i],
                    df$good.CSI.10[i], df$good.CSI.11r[i], df$good.CSI.12r[i],
                    df$good.CSI.13[i], df$good.CSI.14[i], df$good.CSI.15[i],
                    df$good.CSI.16[i], df$good.CSI.17r[i], df$good.CSI.18r[i],
                    df$good.CSI.19r[i], na.rm = T)
    #then bad
    bad.sum <- sum(df$bad.CSI.1[i], df$bad.CSI.2r[i], df$bad.CSI.3[i],
                   df$bad.CSI.4[i], df$bad.CSI.5r[i], df$bad.CSI.6r[i],
                   df$bad.CSI.7[i], df$bad.CSI.8[i], df$bad.CSI.9[i],
                   df$bad.CSI.10[i], df$bad.CSI.11r[i], df$bad.CSI.12r[i],
                   df$bad.CSI.13[i], df$bad.CSI.14[i], df$bad.CSI.15[i],
                   df$bad.CSI.16[i], df$bad.CSI.17r[i], df$bad.CSI.18r[i],
                   df$bad.CSI.19r[i], na.rm = T)
    good.mean <- good.sum/19
    bad.mean <- bad.sum/19
    
    #positivity
    relationship.sum <- sum(df$get.along[i],
                            df$enjoy.company[i],
                            df$comfortable.personal[i])
    relationship.mean <- relationship.sum/3
    
    #assign to spot in corresponding column
    df$good.CSI.AVG[i] <- good.mean
    df$bad.CSI.AVG[i]  <- bad.mean
    
    df$relationship.measure[i] <- relationship.mean
  }
  df
}

#apply to mturk.data.df
mturk.data.avg <- helper.function(mturk.data.df)

#initialize empty matrix of 66 rows and 9 columns
data.slim <- as.data.frame(matrix(data = 0, nrow = 66, ncol = 1))
for(j in 1:nrow(mturk.data.df)){
  #condition
  data.slim$condition[j] <- mturk.data.avg$condition[j]
  #conversation: good or bad
  if (mturk.data.avg$condition[j] == 1 | mturk.data.avg$condition[j] == 2){
    data.slim$conversation[j] <- "good"
  }
  else
    data.slim$conversation[j] <- "bad"
  #aurhority of target: friend/boss
  if (mturk.data.avg$condition[j] %% 2 == 0){
    data.slim$authority[j] <- "friend"
  }
  else
    data.slim$authority[j] <- "boss"
  
  #relationship
  data.slim$relationship[j] <- mturk.data.avg$relationship.measure[j]
  #equality
  data.slim$equality[j] <- mturk.data.avg$equality[j]
  #perceived.power
  data.slim$power[j] <- mturk.data.avg$perceived.power[j]
  #shared.views
  data.slim$shared.views[j] <- mturk.data.avg$shared.views[j]
  #CSI          - good.CSI.AVG | bad.CSI.AVG
  if(data.slim$conversation[j] == "bad"){
    data.slim$CSI[j] <- mturk.data.avg$bad.CSI.AVG[j]
  }
  else{
    data.slim$CSI[j] <- mturk.data.avg$good.CSI.AVG[j]
  }
  #age          - good.age
  data.slim$age[j] <- mturk.data.avg$good.age[j]
  #gender       - good.gender
  data.slim$gender[j] <- mturk.data.avg$good.gender[j]
  #edu          - good.college
  data.slim$edu[j] <- mturk.data.avg$good.college[j]
}

#strings -> factors
data.slim$conversation <- as.factor(data.slim$conversation)
data.slim$authority <- relevel(as.factor(data.slim$authority), "friend")

#first col is placeholder--remove
data.slim <- data.slim[,-1]
attach(data.slim)

#descriptive statistics
describe(CSI)
describe(power)
describe(relationship)
describe(shared.views)
describe(equality)

#dist
hist(CSI)
hist(power)
hist(relationship)
hist(shared.views)
hist(equality)

modCSP <- lm(CSI ~ power + conversation +
               age + edu + gender, data = data.slim)
modCSR <- lm(CSI ~ relationship + conversation +
               age + edu + gender, data = data.slim)
modCSS <- lm(CSI ~ shared.views + conversation + 
               age + edu + gender, data = data.slim)
modCSC <- lm(CSI ~ conversation + age + edu + gender,
             data = data.slim)
modCSE <- lm(CSI ~ equality + conversation +
               age + edu + gender, data = data.slim)
modCSA <- lm(CSI ~ authority + conversation + 
               age + edu + gender, data = data.slim)
modPA <- lm(power ~ authority  + conversation
            + age + edu + gender, data= data.slim)
modPR <- lm(power ~ relationship + conversation +
              age + edu + gender, data = data.slim)
modPS <- lm(power ~ shared.views + conversation + 
              age + edu + gender, data = data.slim)
modPE <- lm(power ~ equality + conversation +
              age + edu + gender, data = data.slim)
modRS <- lm(relationship ~ shared.views + conversation +
              age + edu + gender, data = data.slim)
modRE <- lm(relationship ~ equality + conversation + 
              age + gender + edu, data = data.slim)
modSE <- lm(shared.views ~ equality + conversation +
              age + gender + edu, data = data.slim)
modCSE.S.R <- lm(CSI ~ equality * shared.views * relationship + conversation +
                   age + gender + edu, data = data.slim)
modCSC.A <- lm(CSI ~ conversation * authority + power + relationship + 
                 shared.views + equality + age + edu + gender,
               data = data.slim)
modCSP.A <- lm(CSI ~ power * authority + conversation
               + age + gender + edu, data = data.slim)

summary(modCSP)
confint(modCSP)

summary(modCSR)
confint(modCSR)

summary(modCSS)
confint(modCSS)

summary(modCSE)
confint(modCSE)

summary(modPA)

summary(modPR)

summary(modPS)

summary(modPE)

summary(modRS)
confint(modRS)

summary(modRE)
confint(modRE)

summary(modSE)
confint(modSE)

summary(modCSE.S.R)
confint(modCSE.S.R)

summary(modCSC.A)
confint(modCSC.A)

plotMeans(CSI, as.factor(power), ylim = c(0,7))
plotMeans(CSI, as.factor(relationship), ylim = c(0,7))
plotMeans(CSI, as.factor(shared.views), ylim = c(0,7))
plotMeans(CSI, as.factor(equality), ylim = c(0,7))
plotMeans(relationship, as.factor(equality), ylim = c(0,7))
plotMeans(relationship, as.factor(equality), ylim = c(0,7))
plotMeans(shared.views, as.factor(equality), ylim = c(0,7))

scatter3d(CSI ~ power + as.numeric(conversation), data = data.slim)

scatter3d(as.numeric(authority), CSI, as.numeric(power))

summary(modCSA)
confint(modCSA)
plotMeans(CSI, authority, ylim = c(3,6))

summary(modCSP)
plotMeans(CSI, as.factor(power), ylim = c(3,7))
summary(modCSP.A)
plotMeans(CSI, as.factor(authority), ylim = c(3, 7))
scatter3d(as.numeric(authority), CSI, as.numeric(power))

summary(modCSR)
summary(modCSS)
summary(modCSE)

plotMeans(CSI, as.factor(relationship), ylim = c(0,7))
plotMeans(CSI, as.factor(shared.views), ylim = c(1,6))
plotMeans(CSI, as.factor(equality), ylim = c(0,7))

plotMeans(equality, as.factor(power), ylim = c(4,7))

summary(modCSC)
plotMeans(CSI, as.factor(conversation), ylim = c(1, 7))

