setwd("C:\\Users\\Admin\\Desktop\\datasets")
fundamental.data <- read.csv("fundamental_data_election.csv",stringsAsFactors = F, header = T)

install.packages("magrittr")
install.packages("plyr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("lme4")
install.packages("mvtnorm")
install.packages("sf")
install.packages("heatmaply")
install.packages("ggcorrplot")

library(ggcorrplot)
library(sf)
library(magrittr)
library(tidyverse)
library(dbplyr)
library(reshape2)
library(plyr)
library(lme4)
library(heatmaply)

dim(fundamental.data);head(fundamental.data)
colnames(fundamental.data)
str(fundamental.data)
glimpse(fundamental.data)

#region of the us with the highest population density
ggplot(fundamental.data,aes(x= region , y = density, color = region)) + geom_point() +
  xlab("US Region") + ylab("population in thousands")

#The district of Columbia has the highest population density in the US.
fundamental.data %>% filter(region =="South") %>%
  ggplot(aes(x = state ,y = density)) + geom_boxplot() +ylab("Popular Density") + ggtitle("District Of Columbia: Highest") +
  theme_minimal()

#correlation between average house income and republican voting 
glimpse(fundamental.data)
data <- fundamental.data
df <- data.frame(data$state_unemployment, data$gender_male, data$republican_vote_share_adj, data$democratic_vote_share_adj, data$state_personal_income, data$educ_ba_or_post_grad, data$state_med_income)
ggcorrplot::ggcorrplot(cor(df))



#setting the training and test datasets
test.election.data <- 2020
train<- fundamental.data[fundamental.data$election_year < 2020,]
test <- fundamental.data[fundamental.data$election_year == 2020,]
head(test); head(train)


fundamental.model <- fundamental.model<-lmer(republican_vote_share_adj~PVI+state_unemployment+nat_gdp+race_white+
                                               (1|election_year)+(1|region),
                                             data=train)
summary(fundamental.model)

vote.share.var <- as.data.frame(VarCorr(fundamental.model))
vote.share.var

#extract random variances from model
national.error.var <- matrix(vote.share.var$vcov[vote.share.var$grp=="election_year"])
regional.error.var <- matrix(vote.share.var$vcov[vote.share.var$grp=="region"])
state.error.var <- matrix(vote.share.var$vcov[vote.share.var$grp=="Residual"])


#Setting constants for sim
B <- 1000 #simulation amount
n.years <-length(unique(train$election_year))
n.states <-length(unique(train$state))
n.regions <-length(unique(train$region))
df <- n.years-1 #degree of freedom
state.region.mapping <- unique(fundamental.data[,c("state","region")])
electoral.votes <-unique(fundamental.data[,c("state","electoral_votes")])

#simulate election results B times
test.sim <- test[rep(1:nrow(test),times= B),]
dim(test.sim)

#create matrix of test set predictions:
fund.pred <- predict(fundamental.model, newdata=test.sim,re.form = NA)
pred.rep.share <- matrix(fund.pred,ncol = B)
dim(pred.rep.share); pred.rep.share[1:5,1:5]

#calculate win probabilities for republican based on simulation
rep.state.win.prob<-apply(pred.rep.share,1,function(x) sum(x>0.5)/length(x))
rep.state.win.prob<-data.frame("state"=electoral.votes$state,"rep_win_prob"=rep.state.win.prob)
rep.state.win.prob

#Calculate electoral votes that republicans win by state in each election 
rep.ev <- apply(pred.rep.share,2,function(x)
  sum(electoral.votes$electoral_votes[x>.5]))

#calculate probability that each part wins the election
winner <-ifelse(rep.ev >=270,"Repulicans","Democrats")
prop.table(table(winner))
