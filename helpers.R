#----------------------------------------------------------------
# Reference Objects
#----------------------------------------------------------------

## Variable Lists

#Listening Frequency By Genre
vars_lfbg <-c(
 "Frequency: Classical Music" =   "frequency_classical",
 "Frequency: Country Music" 	=   "frequency_country",
 "Frequency: EDM Music"       =   "frequency_edm",
 "Frequency: Folk Music"  		=   "frequency_folk",
 "Frequency: Gospel Music" 		=   "frequency_gospel",
 "Frequency: Hip-Hop Music" 	=   "frequency_hip_hop",
 "Frequency: Jazz Music" 	  	=   "frequency_jazz",
 "Frequency: K-Pop Music" 		=   "frequency_k_pop",
 "Frequency: Latin Music" 		=   "frequency_latin",
 "Frequency: Lofi Music" 	  	=   "frequency_lofi",
 "Frequency: Metal Music" 		=   "frequency_metal",
 "Frequency: Pop Music" 		  =   "frequency_pop",
 "Frequency: R&B Music"       =   "frequency_r_b",
 "Frequency: Rap Music"       =   "frequency_rap",
 "Frequency: Rock Music"      =   "frequency_rock",
 "Frequency: Video Game Music" =  "frequency_video_game_music"  
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

