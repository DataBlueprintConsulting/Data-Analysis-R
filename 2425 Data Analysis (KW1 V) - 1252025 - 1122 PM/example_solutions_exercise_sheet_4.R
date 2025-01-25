library(tidyverse)
library(readxl)
library(ggbeeswarm)

rbinom( n=10, p=0.5, size=1) 
sum(rbinom( n=10, p=0.5, size=1) )
test<- replicate (10000, sum(rbinom( n=10, p=0.5, size=1 )))

d <- tibble(x=test)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+theme_minimal()+
  labs(x="Number of heads in 10 coin flips", y="Frequency")

sum( test >= 7 ) / 10000

stat_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

stat_mode(test)
mean(test)

d <- tibble(x=test)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ 
  stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+
  theme_minimal()+
  labs(x="Number of heads in 10 coin flips", y="Frequency")

test_06 <- replicate (1000, sum(rbinom( n=10, p=0.6, size=1 )))

d <- tibble(x=test_06)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+theme_minimal()+
  labs(x="Number of heads in 10 coin flips, p(head)=0.6", y="Frequency")

test_03 <- replicate (1000, sum(rbinom( n=10, p=0.3, size=1 )))

d <- tibble(x=test_03)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+theme_minimal()+
  labs(x="Number of heads in 10 coin flips, p(head)=0.6", y="Frequency")

toads_null <- replicate (1000, sum(rbinom( n=18, p=0.5, size=1 )))

d <- tibble(x=toads_null)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+theme_minimal()+
  labs(x="Number of left-handed toads among samples of 18, p(left-handed)=0.5", y="Frequency")

toads_null_100000 <- replicate (100000, sum(rbinom( n=18, p=0.5, size=1 )))

d <- tibble(x=toads_null_100000)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1, col="black", fill="white")+ stat_bin( binwidth=1, geom="text",aes(label=..count..), vjust=-1 )+theme_minimal()+
  labs(x="Number of left-handed toads among samples of 18, p(left-handed)=0.5", y="Frequency")

library( tidyverse )

mu_diamonds <- mean(diamonds[['price']]) 

min(diamonds[['price']]) 
max(diamonds[['price']])

h0 <- 3910

diamonds.h0 <- diamonds
diamonds.h0[['price']] <- (diamonds.h0[['price']]-mu_diamonds) + h0

min(diamonds.h0[['price']])
max(diamonds.h0[['price']])

replications_number <- 1000 
sample.price.h0 <- function(){
slice_sample( diamonds.h0, n=nrow(diamonds), replace=TRUE )[['price']] }
bootstrap_distribution <- replicate(replications_number, mean( sample.price.h0() ))

d<-tibble(x=bootstrap_distribution)
ggplot( d, aes(x=x)) + geom_histogram(binwidth=1)+
geom_vline(xintercept=mu_diamonds, linetype="dashed", color = "red")

delta <- mu_diamonds - h0
number_outside_h0 <- sum( bootstrap_distribution < (h0-abs(delta)) |
                            bootstrap_distribution > (h0+abs(delta)) )
pvalue <- number_outside_h0/replications_number
pvalue



survey_res <- read_excel("survey-results.xlsx")

mu_survey_res <- mean(survey_res[['V1']]) 

min(survey_res[['V1']]) 
max(survey_res[['V1']])


h0 <- 1.5
survey_res.h0 <- survey_res
survey_res.h0[['V1']] <- (survey_res.h0[['V1']]-mu_survey_res) + h0

min(survey_res.h0[['V1']])
max(survey_res.h0[['V1']])

replications_number <- 1000 
sample.survey_res.h0 <- function(){
  slice_sample( survey_res.h0, n=nrow(survey_res), replace=TRUE )[['V1']] }
bootstrap_distribution <- replicate(replications_number, mean( sample.survey_res.h0() ))

d<-tibble(x=bootstrap_distribution)
ggplot( d, aes(x="", y=x)) + geom_quasirandom()+ 
  geom_hline(yintercept=mu_survey_res, color = "red") + coord_flip()

delta <- mu_survey_res - h0
number_outside_h0 <- sum( bootstrap_distribution < (h0-abs(delta)) |
                            bootstrap_distribution > (h0+abs(delta)) )
pvalue <- number_outside_h0/replications_number
pvalue
