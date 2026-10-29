#----------------------------------------------------------------
# Reference Objects
#----------------------------------------------------------------

## Variable Lists

#Listening Frequency By Genre
vars_lfbg <-c(
 "Listening Frequency: Classical Music" =   "frequency_classical",
 "Listening Frequency: Country Music" 	=   "frequency_country",
 "Listening Frequency: EDM Music"       =   "frequency_edm",
 "Listening Frequency: Folk Music"  		=   "frequency_folk",
 "Listening Frequency: Gospel Music" 		=   "frequency_gospel",
 "Listening Frequency: Hip-Hop Music" 	=   "frequency_hip_hop",
 "Listening Frequency: Jazz Music" 	  	=   "frequency_jazz",
 "Listening Frequency: K-Pop Music" 		=   "frequency_k_pop",
 "Listening Frequency: Latin Music" 		=   "frequency_latin",
 "Listening Frequency: Lofi Music" 	  	=   "frequency_lofi",
 "Listening Frequency: Metal Music" 		=   "frequency_metal",
 "Listening Frequency: Pop Music" 		  =   "frequency_pop",
 "Listening Frequency: R&B Music"       =   "frequency_r_b",
 "Listening Frequency: Rap Music"       =   "frequency_rap",
 "Listening Frequency: Rock Music"      =   "frequency_rock",
 "Listening Frequency: Video Game Music" =  "frequency_video_game_music"  
)

#Mental Health Outcomes
vars_mh <- c(
  "Self-Reported Anxiety"     = "anxiety", 
  "Self-Reported Depression"  = "depression",
  "Self-Reported Insomnia"    = "insomnia",
  "Self-Reported OCD"         = "ocd"
)

#Other Numeric Variables
vars_oth_n <-c(
  "Respondent's Age (years)" = "age", 
  "Beats per Minute (bpm) of favorite genre" = "bpm",
  "Daily listening frequency (hours per day)" = "hours_per_day"
)

#Other Categorical Variables
vars_oth_c <- c(
"Respondent's favorite genre"                 = "fav_genre", 
"Primary streaming service"                   = "primary_streaming_service", 
"Listens while working/studying"              = "while_working",
"Respondent regularly plays an instrument"    = "instrumentalist", 
"Respondent regularly composes music"         = "composer", 
"Respondent regularly explores new artists/genres"  = "exploratory",
"Respondent regularly listens to music in another language" ="foreign_languages" , 
"Perceived impact of music on mental health" =   "music_effects"
)

vars_all <- c(vars_lfbg, vars_mh, vars_oth_n, vars_oth_c)

