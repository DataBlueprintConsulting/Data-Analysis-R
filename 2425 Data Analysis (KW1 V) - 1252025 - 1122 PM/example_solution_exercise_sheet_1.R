library( tidyverse )
library( readxl )

iris

class( iris )

iristibble <- as_tibble( iris )

iristibble

twitchdata <- read_excel("twitchdata.xlsx")
twitchdata

View( twitchdata )

colnames( twitchdata )

twitchdata[["Language"]]

class( twitchdata[["Language"]] )

table( twitchdata[["Language"]] )


mean(twitchdata[["Followers"]])


sd_twitch_watchtime <- sd(twitchdata[["Watch time (minutes)"]])
sd_twitch_watchtime

sd_twitch_watchtime^2
var(twitchdata[["Watch time (minutes)"]])

table(twitchdata[["Partnered"]])

dim(filter(twitchdata, Partnered == "True"))[1]

p <- table ( twitchdata [["Language"]] ) / 1000
sum( - p * log2(p) )

max_streamed_minutes <- max(twitchdata[["Stream time (minutes)"]])

quantile(twitchdata[["Stream time (minutes)"]])[['100%']]

max_streamed_minutes/(365*24*60)

twitchdata_spanish <- filter( twitchdata, Language=="Spanish")

mean_spanish_followers <- mean(twitchdata_spanish[["Followers"]])
mean_spanish_followers

spanish_average_followers <- mean(filter(twitchdata, Language=="Spanish")[["Followers"]])
english_average_followers <- mean(filter(twitchdata, Language=="English")[["Followers"]])
chinese_average_followers <- mean(filter(twitchdata, Language=="Chinese")[["Followers"]])

languages <- c("Spanish", "English", "Chinese")
mean_number_folls <- c(spanish_average_followers, english_average_followers, chinese_average_followers )
lang_table <- cbind(languages, mean_number_folls)
lang_table
max(mean_number_folls)

all_languages <- unique(twitchdata[["Language"]])

mean_num_folls_per_lang <- list()

for (lang in all_languages) {
  mean_num <- mean(filter(twitchdata, Language == lang)[["Followers"]])

  mean_num_folls_per_lang <- append(mean_num_folls_per_lang, mean_num)
}

all_langs <- unlist(all_languages)
mean_num_folls_per_lang <- unlist(mean_num_folls_per_lang)
max_mean_num <- max(mean_num_folls_per_lang)

quantile(mean_num_folls_per_lang)[['100%']]

langs_means <- cbind(all_langs, mean_num_folls_per_lang)

langs_means <- as_tibble(langs_means)

filter(langs_means, mean_num_folls_per_lang == max_mean_num)

max_average_per_lang <- 0

for (lang in all_languages ) {
  mean_num <- mean(filter(twitchdata, Language == lang)[["Followers"]])
  if (mean_num > max_average_per_lang){
    max_average_per_lang <- mean_num
    lang_with_max_average <- list(lang)
  } 
  else if(mean_num == max_average_per_lang){
    lang_with_max_average <- append(lang_with_max_average, lang)
  }
  
}

print(paste0("Language(s) with max average followers is/are ",lang_with_max_average, " with ", max_average_per_lang))