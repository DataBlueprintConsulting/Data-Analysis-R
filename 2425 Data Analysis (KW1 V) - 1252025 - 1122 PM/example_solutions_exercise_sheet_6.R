library(tidyverse)
library(ggbeeswarm)
library(readr)
library( readxl )
qrange <- function(r){
  return(function(x) list(ymin=quantile(x,(100-r)/200), y=quantile(x,.5),
                          ymax=quantile(x,1-(100-r)/200)))
} 
ears <- read_delim("ears.csv", delim=",", show_col_types=FALSE)

shuffle_age <- function(){
  d2 <- slice_sample( ears, n=nrow(ears), replace=FALSE )
  return( cov( ears[['age']], d2[['length']] )/var(ears[['age']]) )
}

perm_dist <- tibble( beta=replicate( 2000, shuffle_age() ) ) 
ggplot( perm_dist, aes(x="",y=beta) )+
  geom_hline( yintercept=cov( ears[['age']], ears[['length']] )/var(ears[['age']]) ) + 
  geom_quasirandom()+
  coord_flip()+xlab("")

beta_sample = cov( ears[['age']], ears[['length']] )/var(ears[['age']])
pval <- sum(perm_dist$beta>=beta_sample | perm_dist$beta<= -beta_sample)/length(perm_dist$beta)
pval

ears_small <- slice_sample( ears, n=25 )
shuffle_age <- function(){
  d2 <- slice_sample( ears_small, n=nrow(ears_small), replace=FALSE )
  return( cov( ears_small[['age']], d2[['length']] )/var(ears_small[['age']]) )
}

perm_dist <- tibble( beta=replicate( 2000, shuffle_age() ) ) 
ggplot( perm_dist, aes(x="",y=beta) )+
  geom_hline( yintercept=cov( ears_small[['age']], ears_small[['length']] )/var(ears_small[['age']]) ) + 
  geom_quasirandom()+
  coord_flip()+xlab("")


beta_sample = cov( ears_small[['age']], ears_small[['length']] )/var(ears_small[['age']])
pval <- sum(perm_dist$beta>=beta_sample | perm_dist$beta<= -beta_sample)/length(perm_dist$beta)
pval

library("datasauRus") 
datasaurus_dozen

table( datasaurus_dozen[['dataset']] )

ds <- filter( datasaurus_dozen, dataset=='dino' ) 
ggplot( ds, aes(x=x,y=y) ) + geom_point()

regression_coefficients <- function( x, y ){
  b <- cov( x, y ) / var(x)
  return( c( mean(y)-b*mean(x), b ) )
}

r <- regression_coefficients( ds[['x']], ds[['y']] )
r

ggplot( ds, aes(y=y,x=x) ) + geom_point( )+
  geom_abline( intercept=r[1], slope=r[2], col=2 )

ds_star <- filter( datasaurus_dozen, dataset=='star' ) 
ggplot( ds_star, aes(x=x,y=y) ) + geom_point()

r1 <- regression_coefficients( ds_star[['x']], ds_star[['y']] )
r1
ggplot( ds_star, aes(y=y,x=x) ) + geom_point( )+
  geom_abline( intercept=r[1], slope=r[2], col=2 )


twitch_data <- read_excel("twitchdata.xlsx")

ggplot( twitch_data, aes(y=`Average viewers`,x=Followers) ) +
  geom_point( )


cor_fv = cor(twitch_data[["Followers"]], twitch_data[["Average viewers"]])


cor_fv^2

r <- regression_coefficients( twitch_data[["Followers"]], twitch_data[["Average viewers"]] )

twitch_data[["predicted_length"]] <- twitch_data[["Followers"]] * r[2] + r[1]
twitch_data[["residual"]] <- twitch_data$`Average viewers` - twitch_data[["predicted_length"]]

ggplot( twitch_data, aes( x=Followers, y=residual ) )+geom_point()+geom_smooth()


twitch_data[["log_followers"]] <- log10(twitch_data[["Followers"]])
twitch_data[["log_average_viewers"]] <- log10(twitch_data[["Average viewers"]])

r <- regression_coefficients( twitch_data[["log_followers"]], twitch_data[["log_average_viewers"]] )

twitch_data[["predicted_viewers_logged"]] <- twitch_data[["log_followers"]] * r[2] + r[1]
twitch_data[["residual_logged"]] <- twitch_data$log_average_viewers - twitch_data[["predicted_viewers_logged"]]

ggplot( twitch_data, aes( x=log_followers, y=residual_logged ) )+geom_point()+geom_smooth()

log_foll <- log10(600000)

log_pred_view <- log_foll * r[2] + r[1]

10^log_pred_view


sample_prediction <- function( log_followers ){
  d <- slice_sample( twitch_data, n=nrow(ears), replace=TRUE )
  r <- regression_coefficients( d[["log_followers"]], d[["log_average_viewers"]] )
  d[["residual"]] <- d[["log_average_viewers"]] - (r[1] + d[["log_followers"]]*r[2])
  return( r[1] + log_followers*r[2] + sample( d[["residual"]], 1 ) )
}
pred_dist <- tibble(average_viewers=replicate(100,sample_prediction( log_foll )))
pred_dist
qrange(95)(pred_dist$average_viewers)

qrange(95)(10^pred_dist$average_viewers)