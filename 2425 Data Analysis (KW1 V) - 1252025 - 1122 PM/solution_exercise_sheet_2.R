library(tidyverse)
library(readxl)
library(ggbeeswarm)

twitchdata <- read_excel( "twitchdata.xlsx" )

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_boxplot()

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_point()

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_jitter()
library(ggbeeswarm)

ggplot( iris, aes(x="",y=Sepal.Length) ) + geom_quasirandom()

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_quasirandom() + theme_minimal()

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_quasirandom() + theme_minimal() + theme(axis.title.x=element_blank())

ggplot( iris, aes(x="", y=Sepal.Length) ) + geom_quasirandom() +
  theme_minimal() + theme(axis.title.x=element_blank()) + labs(y="Sepal Length")

ggplot( twitchdata, aes(x="", y=Followers) ) + geom_quasirandom(alpha = 0.5) +
theme_minimal() + theme(axis.title.x=element_blank()) + labs(y="Followers")

ggplot( twitchdata, aes(x="", y=Followers) ) + geom_jitter(alpha=0.5) + theme_minimal() +
  theme(axis.title.x=element_blank()) + labs(y="Followers")

ggplot( twitchdata, aes(x="", y=Followers) ) +
  geom_quasirandom(alpha = 0.5) +
  theme_minimal() + 
  theme(axis.title.x=element_blank()) + 
  scale_y_continuous( trans="log10" ) +
  labs(y="Followers")

ggplot( twitchdata, aes(x="",y=Followers) ) +
  geom_boxplot() + theme_classic() + labs(x="")

ggplot( twitchdata, aes(x="",y=Followers) ) +
  geom_boxplot() +
  scale_y_continuous( trans="log10" ) + 
  theme_classic() +
  labs(x="")
  
ggplot( twitchdata, aes(x=Followers)) +
  geom_histogram(binwidth = 625000, boundary=0) +
  scale_x_continuous(breaks = seq(0,7500000,2500000))+
  labs(y="Count") + theme_classic()

ggplot(data=twitchdata, aes(x=Language, y=Followers)) +
  geom_quasirandom() +
  theme_classic()

ggplot(data=twitchdata, aes(x=Language, y=Followers)) +
  geom_boxplot() +
  theme_classic()

ggplot( twitchdata, aes(x=`Stream time (minutes)`, y=Followers, colour=Partnered)) +
  geom_point(alpha=0.4) + 
  scale_x_continuous(trans="log10") +
  scale_y_continuous(trans="log10") +
  theme_classic()
survey_res <- read_excel("survey-results.xlsx")

ggplot(survey_res, aes(x=as.factor(F1), fill=as.factor(F1) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  labs(x="Fashion style", y="Count") +
  theme(legend.position = "none",
        axis.line = element_line(),
        axis.ticks = element_line()) 

ggplot(survey_res, aes(x="", y=V1)) + 
  geom_boxplot( ) +
  labs(x="", y="Hours of gaming")  + theme_classic()

ggplot(survey_res, aes(x=as.factor(V4), fill=as.factor(V4) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  labs(x="Reason", y="Count", title="Why computing students play video games") +
  theme(legend.position="none") 

ggplot(survey_res, aes(x=as.factor(S1), fill=as.factor(S1) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  labs(x="Score", y="Count", title = "Level of comfort of computing students in a group of strangers") +
  theme(legend.position="none")