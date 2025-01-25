library(tidyverse)
library(readxl)
library(ggbeeswarm)

mean_sd <- function(x){ return(list( ymin=mean(x)-sd(x),y=mean(x),ymax=mean(x)+sd(x) )) } 

qrange <- function(r){
  return(function(x) list(ymin=quantile(x,(100-r)/200), y=quantile(x,.5),
                        ymax=quantile(x,1-(100-r)/200)))
} 

twitter_accounts <- read_excel( "twitter-accounts.xlsx" )

median(twitter_accounts[["followers_count"]])

some_twitter_accounts <- slice_sample(twitter_accounts, n = 1000)

resample_followers <- function(){
  x <- slice_sample( some_twitter_accounts, n=nrow(some_twitter_accounts),
                     replace=TRUE )
  return( x[['followers_count']] )
}

median(resample_followers())

replicate( 10, median(resample_followers()) )

bs_dist <- tibble(median=replicate(1000,median(resample_followers())))
ggplot(bs_dist,aes(x="",y=median))+
  theme_minimal()+
  geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=mean_sd, geom="pointrange")+
  coord_flip()+xlab("")

ggplot(bs_dist,aes(x="",y=median))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  coord_flip()+xlab("")+theme_minimal()


survey_results <- read_excel("survey-results.xlsx")
resample_hours <- function(){
  x <- slice_sample( survey_results, n=nrow(survey_results),
                     replace=TRUE )
  return( x[['V1']] )
}

mean(resample_hours())

bs_dist_hours <- tibble(mean=replicate(1000,mean(resample_hours())))

mean_sd(unlist(bs_dist_hours))

ggplot(bs_dist_hours,aes(x="",y=mean))+
  theme_minimal()+
  geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=mean_sd, geom="pointrange")+
  coord_flip()+xlab("")


qrange(95)(unlist(bs_dist_hours))

ggplot(bs_dist_hours,aes(x="",y=mean))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  coord_flip()+xlab("")+theme_minimal()


resample_answers <- function(){
  x <- slice_sample( survey_results, n=nrow(survey_results), replace=TRUE )
  return( x[['V2']] )
}

calc_prop <- function(x){
  return (table(x)["Yes"]/length(x))
}

bs_dist_frac_yes <- tibble(frac_yes=replicate(1000,calc_prop(resample_answers())))

mean_sd(unlist(bs_dist_frac_yes))


ggplot(bs_dist_frac_yes,aes(x="",y=frac_yes))+
  theme_minimal()+
  geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=mean_sd, geom="pointrange")+
  coord_flip()+xlab("")


qrange(95)(unlist(bs_dist_frac_yes))

ggplot(bs_dist_frac_yes,aes(x="",y=frac_yes))+geom_quasirandom(col="gray")+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  coord_flip()+labs(x="", y="Proportion of 'Yes' answers")+theme_minimal()

second_rank <- function(x){
  names(sort(table(x),decreasing=TRUE))[2]
}

resample_reasons <- function(){
  x <- slice_sample( survey_results, n=nrow(survey_results),
                     replace=TRUE )
  return( x[['V4']] )
}

table(replicate(1000, second_rank(resample_reasons())))

var1 <- function(x){ return( sum( (x-mean(x))^2 ) / (length(x)-1) ) } # standard, unbiased 
var2 <- function(x){ return( sum( (x-mean(x))^2 ) / length(x) ) } # slightly biased

unbias_var_price <-var1(diamonds[["price"]])
bias_var_price <- var2(diamonds[["price"]])


resample_price <- function(){
  x <- slice_sample( diamonds, n=10, replace=TRUE )
  return( x[['price']] )
}

bs_dist_var1_price <- tibble(var1=replicate(5000, var1(resample_price())))
bs_dist_var2_price <- tibble(var2=replicate(5000, var2(resample_price())))

var1_diamonds_price <- var1(diamonds[['price']])
var2_diamonds_price <- var2(diamonds[['price']])

ggplot( bs_dist_var1_price, aes( x = "", y=var1) ) +
  geom_quasirandom(col="gray") + stat_summary( fun="mean",
                                               geom="crossbar",col="red") +
  geom_hline(yintercept=var1_diamonds_price)+
  coord_flip()+theme_minimal()+labs(x="")

ggplot( bs_dist_var2_price, aes( x = "", y=var2) ) +
  geom_quasirandom(col="gray") + stat_summary( fun="mean",
                                               geom="crossbar",col="red") +
  geom_hline(yintercept=var2_diamonds_price)+
  coord_flip()+theme_minimal()+labs(x="")

sd1 <- function(x){  return( sqrt(sum( (x-mean(x))^2 ) / (length(x)-1) )  )}
sd2 <- function(x){  return( sqrt(sum( (x-mean(x))^2 ) / length(x) ) )}

unbias_sd_price <-sd1(diamonds[["price"]])
bias_sd_price <- sd2(diamonds[["price"]])


resample_price <- function(){
  x <- slice_sample( diamonds, n=10, replace=TRUE )
  return( x[['price']] )
}

bs_dist_sd1_price <- tibble(sd1=replicate(5000, sd1(resample_price())))
bs_dist_sd2_price <- tibble(sd2=replicate(5000, sd2(resample_price())))

sd1_diamonds_price <- sd1(diamonds[['price']])
sd2_diamonds_price <- sd2(diamonds[['price']])

ggplot( bs_dist_sd1_price, aes( x = "", y=sd1) ) +
  geom_quasirandom(col="gray") + stat_summary( fun="mean",
                                               geom="crossbar",col="red") +
  geom_hline(yintercept=sd1_diamonds_price)+
  coord_flip()+theme_minimal()+labs(x="")

ggplot( bs_dist_sd2_price, aes( x = "", y=sd2) ) +
  geom_quasirandom(col="gray") + stat_summary( fun="mean",
                                               geom="crossbar",col="red") +
  geom_hline(yintercept=sd2_diamonds_price)+
  coord_flip()+theme_minimal()+labs(x="")