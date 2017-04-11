## Uses rex package (https://cran.r-project.org/web/packages/rex/index.html) to offload heavy regex 
## for each race into a dataframe

library(rex)
library(dplyr)

### SETUP COUNTERS & PATHS ###

m <- 1
n <- 1
p <- 1
q <- 1
r <- 1
#s <- 1  #put inside function
t <- 1
u <- 1
v <- 1
x <- 1
y <- 1
z <- 1

trks <- file.path("E:","Horse","Tracks")

source("functions_text_to_df.R")
source("support_text_to_df.R");


### START OF CODE ###

# remove other df_final, declare new df, assign col names
rm(df_final); df_final <- data.frame(matrix(0, ncol = 142));  colnames(df_final) <- col_names

# reset racelist & txtfiles lists, create list of all file paths to access
rm(racelist); racelist <- list(0)
rm(txtfiles); txtfiles <- 0
files <- list.files(trks, recursive = TRUE)
for (n in 1:length(files)) {txtfiles[n] <- paste(trks, "/", files[n], sep="", collapse = "")}


count_m <- 1


## Process text ## nrow(txtfiles)
for (m in 30:30) {
  
    # Clear dataframe
    df_subraces <- data.frame(matrix(0,ncol=142))
    count_r <- 1
    colnames(df_subraces) <- col_names
    
  
    # Read in raw text file  
    data_con <- file(txtfiles[m]); open(data_con)
    data_in <- readLines(data_con, ok = TRUE, warn = FALSE); close(data_con)
  
    # Find out how many races & divide into new vectors
    nraces <- length(grep(substr(data_in[1], 1, nchar(data_in[1])-2), data_in))
    divrows <- grep(substr(data_in[1], 1, nchar(data_in[1])-2), data_in) 

    for (q in 1:(nraces-1)) {racelist[[q]] <- assign(paste("race_",q,sep = ""), data_in[divrows[q]:(divrows[q+1]-1)])}
    q <- nraces; racelist[[q]] <- assign(paste("race_",q,sep = ""),data_in[divrows[q]:length(data_in)])
    
    for (r in 1:nraces) {
      
      # Look at each race
      parse_race <- racelist[[r]]
    
      # Parsing text
      trk_row <- 1; trk_tmp <- track_fn(parse_race, trk_row) ## TRACK
      can_row <- trk_row + 1; can_tmp <- canceled_fn(parse_race, can_row, trk_tmp) ## CANCELLATION
      rtype_row <- trk_row + 1; rtype_tmp <- racetype_fn(parse_race, rtype_row) ## RACE TYPE - STILL TO DO: race type additional info (eg: arabian stakes)  ##grep(('^.*CLAIMING|^.*STAKES|^.*ALLOWANCE|^.*SPEED INDEX RACE|^.*SPECIAL WEIGHT|^.*MAIDEN'),parse_race)
      ttype_row <- grep(('^.*Track Record:|^.*On The'),parse_race); ttype_tmp <- tracktype_fn(parse_race, ttype_row) ## TRACK TYPE - distance etc - STILL TO DO: when trackrecordtime is blank, take care of "originally scheduled for"
      rc_tmp <- racecriteria_fn(rtype_row, ttype_row, parse_race) ## RACE CRITERIA
      prs_row <- grep("^Purse:",parse_race); prs_tmp <- purse_fn(parse_race, prs_row) ## PURSE - STILL TO DO: deal with "check for anything else on this row"Added", "Guaranteed"
      avm_row <- grep("^Available Money:",parse_race); avm_tmp <- availmoney_fn(parse_race, avm_row) ## AVAILABLE MONEY
      wea_row <- grep("^Weather:",parse_race); wea_tmp <- weather_fn(parse_race, wea_row) ## WEATHER
      vor_tmp <- valueofrace_fn(parse_race, avm_row, wea_row) ## VALUE OF RACE
      off_row <- grep("^Off at:", parse_race); off_tmp <- offat_fn(parse_race, off_row) ## OFF AT
      ft_row <- grep("^Fractional Times:", parse_race); ft_tmp <- fractimes_fn(parse_race, ft_row) ## FRACTIONAL TIMES
      fint_row <- grep("*Final Time:", parse_race); fint_tmp <- finaltime_fn(parse_race, fint_row) ## FINAL TIME
      st_row <- grep("^Split Times:", parse_race); st_tmp <- splittimes_fn(parse_race, st_row) ## SPLIT TIMES
      ru_row <- grep("^Run-Up:", parse_race); ru_tmp <- runup_fn(parse_race, ru_row) ## RUN UP
      win_row <- grep("^Winner: ", parse_race); win_tmp <- winner_fn(parse_race, win_row) ## WINNER
      breed_row <- grep("^Breeder: ", parse_race); breed_tmp <- breeder_fn(parse_race, breed_row) ## BREEDER
      scratch_row <- grep("^Scratched Horse", parse_race); scratch_tmp <- scratching_fn(parse_race, scratch_row) ## SCRATCHINGS - still to do amend for no scratchings
      ht_starow <- (grep("^Last Raced Pgm Horse", parse_race) + 1); df_htrow <- horsetable_fn(parse_race, ht_starow, ft_row, fint_row, ru_row) ## HORSE TABLE
      wpspl_row <- grep("Total WPS Pool:", parse_race); wpspl_tmp <- wpspool_fn(parse_race, wpspl_row) ## WPS POOL
      
      train_row <- grep("^Trainers: ", parse_race); train_tmp <- trainers_fn(parse_race, train_row) ## TRAINERS
      owners_row <- grep("^Owners: ", parse_race); owners_tmp <- owners_fn(parse_race, owners_row) ## OWNERS
      foot_row <- grep("^Footnotes", parse_race); foot_tmp <- footnotes_fn(parse_race, foot_row) ## FOOTNOTES


      
      
      
      #df_subraces[r,] <- combine(trk_tmp, can_tmp, rtype_tmp, rc_tmp, ttype_tmp, prs_tmp, avm_tmp, wea_tmp, vor_tmp, off_tmp, ft_tmp, fint_tmp, st_tmp, ru_tmp, win_tmp, breed_tmp, scratch_tmp, df_htrow[1,], wpspl_tmp, first_tmp, second_tmp, third_tmp, exact_tmp, trif_tmp, super_tmp, train_tmp, owners_tmp, foot_tmp)
      #count_r <- count_r + 1

      
      # c(trk_tmp, can_tmp, rtype_tmp, rc_tmp, ttype_tmp, prs_tmp, avm_tmp, wea_tmp, vor_tmp, off_tmp, ft_tmp, fint_tmp, st_tmp, ru_tmp, win_tmp, breed_tmp, scratch_tmp, df_htrow[1,], wpspl_tmp, first_tmp, second_tmp, third_tmp, exact_tmp, trif_tmp, super_tmp, train_tmp, owners_tmp, foot_tmp)

      
      df_subraces[r,] <- bind_rows(data.frame(df_merge))

      # before wspl
      

        
    }

  
              
#df_final <- rbind(df_final,df_subraces)
        
}                               



# # WPS Results rows      ## BUILD IN TIES/DEAD HEATS IN HERE AND TRIFECTAS ETC
# wpsr_row <- grep("^Pgm Horse Win Place Show", parse_race)
# exotics_row <- grep("^Wager Type Winning Numbers", parse_race)
# count_places <- exotics_row - wpsr_row - 1
# 
# if(count_places == 3) {
#   first_row <- wpsr_row + 1; first_tmp <- re_matches(parse_race[first_row], rex(capture(numbers, name = "first_num"), space, capture(any_non_numbers, name = "first_name"), space, capture(anything, name = "first_win"), space, capture(anything, name = "first_place"), space, capture(anything, name = "first_show")))
#   second_row <- wpsr_row + 2; second_tmp <- re_matches(parse_race[second_row], rex(capture(numbers, name = "second_num"), space, capture(any_non_numbers, name = "second_name"), space, capture(anything, name = "second_place"), space, capture(anything, name = "second_show")))
#   third_row <- wpsr_row + 3; third_tmp <- re_matches(parse_race[third_row], rex(capture(numbers, name = "third_num"), space, capture(any_non_numbers, name = "third_name"), space, capture(anything, name = "third_show")))
# }
# 
# if(count_places == 4) {
#   first_row <- wpsr_row + 1; first_tmp <- re_matches(parse_race[first_row], rex(capture(numbers, name = "first_num"), space, capture(any_non_numbers, name = "first_name"), space, capture(anything, name = "first_win"), space, capture(anything, name = "first_place"), space, capture(anything, name = "first_show")))
#   second_row <- wpsr_row + 2; second_tmp <- re_matches(parse_race[second_row], rex(capture(numbers, name = "second_num"), space, capture(any_non_numbers, name = "second_name"), space, capture(anything, name = "second_place"), space, capture(anything, name = "second_show")))
#   third_row <- wpsr_row + 3; third_tmp <- re_matches(parse_race[third_row], rex(capture(numbers, name = "third_num"), space, capture(any_non_numbers, name = "third_name"), space, capture(anything, name = "third_show")))
# }
#   
# # Exotics rows - look for Exacta/Perfecta(1,2), Quinella(1,2 or 2,1), Trifecta(1,2,3), Superfecta(1,2,3,4)
# 
# # Exacta/Perfecta rows
# exact_row <- grep("Exacta", parse_race)
# if (length(exact_row) == 0) {exact_row <- grep("Perfecta", parse_race)}
# if (length(exact_row) == 0) {exact_row <- grep("Exactor", parse_race)}
# if (length(exact_row) == 1) {
#   parse_race[exact_row] <- sub(",","",parse_race[exact_row])
#   exact_tmp <- re_matches(parse_race[exact_row], rex("$", capture(anything, name = "exactbetunit"), space, letters, space, capture(anything, name = "exactwn"), space, capture(anything, name = "exactpayoff"), space, capture(anything, name = "exactpool")))
# } else {exact_tmp <- data.frame(matrix(NaN, ncol = 4))}
#   
# # Trifecta row
# trif_row <- grep("Trifecta", parse_race)
# if (length(trif_row) == 0) {trif_row <- grep("Triactor", parse_race)}
# if (length(trif_row) == 1) {
#   parse_race[trif_row] <- sub(",","",parse_race[trif_row])
#   trif_tmp <- re_matches(parse_race[trif_row], rex("$", capture(anything, name = "trifbetunit"), space, letters, space, capture(anything, name = "trifwn"), space, capture(anything, name = "trifpayoff"), space, capture(anything, name = "trifpool")))
# } else {trif_tmp <- data.frame(matrix(NaN, ncol = 4))}
# 
# # Superfecta row
# super_row <- grep("Superfecta", parse_race)
# if (length(super_row) == 1) {
#   parse_race[super_row] <- sub(",","",parse_race[super_row])
#   super_tmp <- re_matches(parse_race[super_row], rex("$", capture(anything, name = "superbetunit"), space, letters, space, capture(anything, name = "superwn"), space, capture(anything, name = "superpayoff"), space, capture(anything, name = "superpool")))
# } else {super_tmp <- data.frame(matrix(NaN, ncol = 4))}


#pp_row <- grep("^Past Performance Running", parse_race)



# PP Running Line Preview rows
#      pprl_row <- grep("^Pgm Horse Name", parse_race)
#      pprl_tmp <- re_matches(parse_race[pprl_row], rex("Pgm Horse Name", space, maybe(capture(graphs, name = "first"), space), maybe(capture(graphs, name = "second"), space), maybe(capture(graphs, name = "third"), space), maybe(capture(graphs, name = "fourth"), space), "Str", space, "Fin"))
#      ntimepts <- length(which(pprl_tmp != "")) + 2

# pprl_starow <- pprl_row + 1
# pprl_finrow <- train_row - 1
# u <- seq(pprl_starow, pprl_finrow, 1)
# pprl_nrow <- length(u)
# df_pprlrow <- data.frame(matrix(0,ncol=12))
#      colnames(df_pprlrow) <- c("fst_pos", "fst_lth", "snd_pos", "snd_lth", "thd_pos", "thd_lth", "fth_pos", "fth_lth", "str_pos", "str_lth", "fin_pos", "fin_lth" )

#  for (u in pprl_starow:pprl_finrow) {

#   pprl_onesent <- parse_race[u[1]]
#  pprl_row <- re_matches(pprl_onesent, rex(start, digits, spaces, non_digits, space, capture(anything, name = "tp_string")))
#pprl_split <- unlist(strsplit(pprl_row$tp_string, " "))

# test <- pprl_row
#test <- gsub(" 1/2",".5", test); test <- gsub(" 1/4",".25", test); gsub(" 3/4",".75", test)

#     test <- unlist(strsplit(test, " "))
#      ntp <- length(test)

#    if (ntp == 6) {fst_pos<-test[1];snd_pos<-test[2];thd_pos<-test[3];fth_pos<-test[4];str_pos<-test[5];fin_pos<-test[6]}
#    if (ntp == 5) {fst_pos<-test[1];snd_pos<-test[2];thd_pos<-test[3];str_pos<-test[4];fin_pos<-test[5]}
#    if (ntp == 4) {fst_pos<-test[1];snd_pos<-test[2];str_pos<-test[3];fin_pos<-test[4]}
#   if (ntp == 3) {fst_pos<-test[1];str_pos<-test[2];fin_pos<-test[3]}

#examine fst_pos
#    if (nchar(fst_pos)==1) {fst_lth <- "0"}
#   if (nchar(fst_pos)==2 && pprl_tmp$first == "Start") {fst_lth <- "0"} else {fst_lth <- substr(fst_pos, 2, 2); fst_pos <- substr(fst_pos, 1, 1)}
#    if (nchar(fst_pos)==3 && pprl_nrow < 10) {fst_lth <- substr(fst_pos, 2, 3); fst_pos <- substr(fst_pos, 1, 1)}
#    if (nchar(fst_pos)==3 && pprl_nrow == 10 && substr(fst_pos, 1, 2) == "10") {fst_lth <- substr(fst_pos, 3, 3); fst_pos <- substr(fst_pos, 1, 1)}
#    if (nchar(fst_pos)==3 && pprl_nrow > 10) {fst_lth <- substr(fst_pos, 2, 3); fst_pos <- substr(fst_pos, 1, 1)}



#for (v in 1:ntp) {
#  if (v == 1 && nchar(test[v]) == 1) {fst_pos <- test[v]; fst_lth <- "0"}
#}


#    df_pprlrow <- rbind(df_pprlrow, pprl_row)
#  }
























#    x[[m]] <- data.table(re_matches(text2[row1:rown], rex(start, capture(non_spaces, name = "lr_date"),
#                                      maybe(space), 
#                                      maybe(group(capture(digits, name = "lr_rn"), 
#                                                   capture(alphas, name = "lr_trk"), 
#                                                   capture(digits, name = "lr_pos"))), 
#                                      spaces, group(capture(digits, name = "pgm_pos"),maybe(alphas)),
#                                      spaces, capture(anything, name = "horse"), 
#                                      " (", capture(something, name = "jockey"),
#                                      ")", spaces, group(capture(non_spaces, name = "wgt")),
#                                      spaces, maybe(capture(group(non_spaces,maybe(spaces,not(spaces %or% digits))), name="meds_eq")),
#                                      spaces, capture(digits, name = "post_pos"),
#                                      spaces, capture(digit, name = "start"),
#                                      spaces, group(capture(digit, name = "qtr_pos"),maybe(capture(group(non_spaces,maybe(space),maybe(non_spaces)), name = "qtr_lth"))),
#                                      spaces, group(capture(digit, name = "hlf_pos"),maybe(capture(group(non_spaces,maybe(space),maybe(non_spaces)), name = "hlf_lth"))),
#                                      spaces, group(capture(digit, name = "str_pos"),maybe(capture(group(non_spaces,maybe(space),maybe(non_spaces)), name = "str_lth"))),
#                                      spaces, group(capture(digit, name = "fin_pos"),maybe(capture(group(non_spaces,maybe(space),maybe(non_spaces)), name = "fin_lth"))),
#                                     spaces, group(capture(group(digits,".",digits), name = "odds"),maybe("*")),
#                                     spaces, capture(anything, name = "comments")
#                                     )))
#    
    #print(data.frame(x[[m]])  )  

#df <- rbindlist(x)
#print(df)
#write.csv(df, file = paste(BASE_DL,"/text.csv", sep = "", collapse = ""))
