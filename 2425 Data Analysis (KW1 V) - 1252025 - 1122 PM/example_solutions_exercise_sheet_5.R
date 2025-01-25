library(tidyverse)
library(readxl)
library(ggbeeswarm)

qrange <- function(r){
  return(function(x) list(ymin=quantile(x,(100-r)/200), y=quantile(x,.5),
                          ymax=quantile(x,1-(100-r)/200)))
} 

survey_res_2024 <- read_excel("survey-results.xlsx")
survey_res_2023 <- read_excel("survey-results-2023.xlsx")

sample_2023_h <- function(){ return(
  slice_sample(survey_res_2023,n=nrow(survey_res_2023),replace=TRUE)[['V1']]) }

sample_2024_h <- function(){ return(
  slice_sample(survey_res_2024,n=nrow(survey_res_2024),replace=TRUE)[['V1']]) }

bs_dist_mean_diff_h <- tibble( d_in_means=replicate( 1000,
   mean( sample_2024_h() ) - mean( sample_2023_h() ) ) )

ggplot( bs_dist_mean_diff_h, aes(y=d_in_means,x="") ) + geom_quasirandom()+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  xlab("")+coord_flip()

qrange(95)(bs_dist_mean_diff_h$d_in_means)

mean_h_2024 <- mean(survey_res_2024[['V1']])
mean_h_2023 <- mean(survey_res_2023[['V1']])

survey_res_2024_mod <- survey_res_2024
survey_res_2024_mod[['V1']] <- survey_res_2024_mod[['V1']] - mean_h_2024 + mean_h_2023

sample_2024_h_mod <- function(){ return(
  slice_sample(survey_res_2024_mod,n=nrow(survey_res_2024_mod),replace=TRUE)[['V1']]) }

bs_dist_h0 <- tibble( d_in_means=replicate( 1000,
            mean( sample_2024_h_mod() ) - mean( sample_2023_h() ) ) )

bs_dist_h0[['extreme']] <- abs(bs_dist_h0[['d_in_means']]) > abs(mean_h_2024-mean_h_2023)

ggplot( bs_dist_h0, aes(y=d_in_means,x="",col=extreme) ) +
  geom_quasirandom() +
  scale_color_manual( values = c( "TRUE" = "red", "FALSE" = "darkgray" ) )+
  geom_hline( yintercept = mean_h_2024-mean_h_2023 ) +
  geom_hline( yintercept = -(mean_h_2024-mean_h_2023) ) +
  xlab("")+coord_flip()


pval <- sum(bs_dist_h0[['extreme']])/length(bs_dist_h0[['d_in_means']])
pval

delta_vacc <- tibble(infection_status = c(rep(0,7472-20), rep(1,20)))
delta_unvacc <- tibble(infection_status = c(rep(0,117822-1006), rep(1,1006)))

sample_delta_vacc <- function(){ return(
  slice_sample(delta_vacc,n=nrow(delta_vacc),replace=TRUE)[['infection_status']]) 
  }

sample_delta_unvacc <- function(){ return(
  slice_sample(delta_unvacc,n=nrow(delta_unvacc),replace=TRUE)[['infection_status']]) 
}

num_of_replications <- 1000
bs_dist_delta <- tibble( vaccine_efficacy=replicate( num_of_replications,
                                             100*(1-mean( sample_delta_vacc() )/mean( sample_delta_unvacc() ) ) ) )

qrange(95)(bs_dist_delta$vaccine_efficacy)

delta_vacc_mod <- tibble(infection_status = c(rep(0,7472-61), rep(1,61)))
delta_unvacc_mod <- tibble(infection_status = c(rep(0,117822-965), rep(1,965)))

# sampling functions modified
sample_delta_vacc_mod <- function(){ return(
  slice_sample(delta_vacc_mod,n=nrow(delta_vacc_mod),replace=TRUE)[['infection_status']]) 
}

sample_delta_unvacc_mod <- function(){ return(
  slice_sample(delta_unvacc_mod,n=nrow(delta_unvacc_mod),replace=TRUE)[['infection_status']]) 
}

num_of_replications <- 1000
bs_dist_delta_mod <- tibble( risk_ratio=replicate( num_of_replications,
                            mean( sample_delta_vacc_mod() )/mean( sample_delta_unvacc_mod() ) ) )

rr_sample <- (20/7472)/(1006/117822)

bs_dist_delta_mod[['extreme']] <- bs_dist_delta_mod[['risk_ratio']] < rr_sample | bs_dist_delta_mod[['risk_ratio']] > 1/rr_sample
pvalue <- sum(bs_dist_delta_mod[['extreme']])/num_of_replications
pvalue


ggplot( bs_dist_delta_mod, aes(y=risk_ratio,x="",col=extreme) ) +
  geom_quasirandom() +
  scale_color_manual( values = c( "TRUE" = "red", "FALSE" = "darkgray" ) )+
  geom_hline( yintercept = rr_sample ) +
  coord_flip()

omicron_vacc <- tibble(infection_status = c(rep(0,36498-158), rep(1,158)))
omicron_unvacc <- tibble(infection_status = c(rep(0,99776-488), rep(1,488)))

# sampling functions
sample_omicron_vacc <- function(){ return(
  slice_sample(omicron_vacc,n=nrow(omicron_vacc),replace=TRUE)[['infection_status']]) 
}

sample_omicron_unvacc <- function(){ return(
  slice_sample(omicron_unvacc,n=nrow(omicron_unvacc),replace=TRUE)[['infection_status']]) 
}

num_of_replications <- 1000
bs_dist_omicron <- tibble( vaccine_efficacy=replicate( num_of_replications,
                                                     100*(1-mean(sample_omicron_vacc())/mean( sample_omicron_unvacc()))))

qrange(95)(bs_dist_omicron$vaccine_efficacy)

omicron_vacc_mod <- tibble(infection_status = c(rep(0,36498-173), rep(1,173)))
omicron_unvacc_mod <- tibble(infection_status = c(rep(0,99776-473), rep(1,473)))

sample_omicron_vacc_mod <- function(){ return(
  slice_sample(omicron_vacc_mod,n=nrow(omicron_vacc_mod),replace=TRUE)[['infection_status']]) 
}

sample_omicron_unvacc_mod <- function(){ return(
  slice_sample(omicron_unvacc_mod,n=nrow(omicron_unvacc_mod),replace=TRUE)[['infection_status']]) 
}

num_of_replications <- 1000
bs_dist_omicron_mod <- tibble( risk_ratio=replicate(num_of_replications,
                                                   mean(sample_omicron_vacc_mod())/mean( sample_omicron_unvacc_mod()))
)

rr_sample <- (158/36498)/(488/99776)

bs_dist_omicron_mod[['extreme']] <- bs_dist_omicron_mod[['risk_ratio']] < rr_sample | bs_dist_omicron_mod[['risk_ratio']] > 1/rr_sample
pvalue <- sum(bs_dist_omicron_mod[['extreme']])/num_of_replications
pvalue


ggplot( bs_dist_omicron_mod, aes(y=risk_ratio,x="",col=extreme) ) +
  geom_quasirandom() +
  scale_color_manual( values = c( "TRUE" = "red", "FALSE" = "darkgray" ) )+
  geom_hline( yintercept = rr_sample ) +
  coord_flip()

stud_mean_diff <- function(x,y){
  mean_diff = mean(x)-mean(y)
  sd_norm = sqrt(( (sd(x)^2)/length(x) ) + ( (sd(y)^2)/length(y) ))
  mean_diff/sd_norm
}

bs_dist_reg_mean <- tibble(d_in_means = replicate(1000, mean(sample_2024_h()) - mean(sample_2023_h()) ))

ggplot( bs_dist_reg_mean, aes(y=d_in_means,x="") ) + geom_quasirandom()+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  xlab("")+coord_flip()

qrange(95)(bs_dist_reg_mean$d_in_means)

bs_dist_stud_mean <- tibble(d_in_means = replicate(1000, stud_mean_diff(sample_2024_h(), sample_2023_h()) ))

ggplot( bs_dist_stud_mean, aes(y=d_in_means,x="") ) + geom_quasirandom()+
  stat_summary(col="red",fun.data=qrange(95),geom="errorbar")+
  xlab("")+coord_flip()

qrange(95)(bs_dist_stud_mean$d_in_means)

bs_dist_reg_mean_h0 <- tibble(d_in_means_h0 = replicate(1000, 
                                                        mean(sample_2024_h_mod())-mean(sample_2023_h()) ))
bs_dist_reg_mean_h0[['extreme']] <- bs_dist_reg_mean_h0[['d_in_means_h0']] > abs(mean_h_2024 - mean_h_2023) |
                                        bs_dist_reg_mean_h0[['d_in_means_h0']] < -abs(mean_h_2024 - mean_h_2023)

ggplot( bs_dist_reg_mean_h0, aes(y=d_in_means_h0,x="",col=extreme) ) +
  geom_quasirandom() +
  scale_color_manual( values = c( "TRUE" = "red", "FALSE" = "darkgray" ) )+
  geom_hline( yintercept = mean_h_2024 - mean_h_2023 ) +
  xlab("")+coord_flip()

pval_reg_mean <- sum(bs_dist_reg_mean_h0[['extreme']])/length(bs_dist_reg_mean_h0[['d_in_means_h0']])
pval_reg_mean

bs_dist_stud_mean_h0 <- tibble(d_in_means_h0 = replicate(1000, stud_mean_diff(sample_2024_h_mod(), sample_2023_h())))
bs_dist_stud_mean_h0[['extreme']] <- bs_dist_stud_mean_h0[['d_in_means_h0']] > stud_mean_diff(survey_res_2024[['V1']], survey_res_2023[['V1']]) |
                                       bs_dist_stud_mean_h0[['d_in_means_h0']] < -stud_mean_diff(survey_res_2024[['V1']], survey_res_2023[['V1']])

ggplot( bs_dist_stud_mean_h0, aes(y=d_in_means_h0,x="",col=extreme) ) +
  geom_quasirandom() +
  scale_color_manual( values = c( "TRUE" = "red", "FALSE" = "darkgray" ) )+
  geom_hline( yintercept = stud_mean_diff(survey_res_2024[['V1']], survey_res_2023[['V1']]) ) +
  xlab("")+coord_flip()

pval_stud_mean <- sum(bs_dist_stud_mean_h0[['extreme']])/length(bs_dist_stud_mean_h0[['d_in_means_h0']])
pval_stud_mean