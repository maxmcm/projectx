## Uses rex package (https://cran.r-project.org/web/packages/rex/index.html) to offload heavy regex for each race into a dataframe

library(rex)



df_final <- data.frame(matrix(0,ncol=45))   
colnames(df_final) <- c("trackname", "date", "racenum", "canyn", "canreason", "racetype", "horsetype"
                           , "trackdistance", "tracktype", "trackrecordhorse", "trackrecordtime", "trackrecorddate", "racecriteria", "detailracecriteria"
                           , "purse", "availmoney","weather", "wind", "trackspeed", "valtotal", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9"
                           , "off_at", "start", "timer", "wpspool"
                           , "exactbetunit", "exactwn", "exactpayoff", "exactpool", "trifbetunit", "trifwn", "trifpayoff", "trifpool", "superbetunit", "superwn", "superpayoff", "superpool"
                           )

m <- 1
n <- 1
p <- 1
q <- 1
r <- 1
s <- 1
t <- 1
u <- 1
v <- 1
x <- 1
y <- 1
z <- 1

## Set up file paths
trks <- file.path("E:","Horse","Tracks")

racelist <- list(0)
txtfiles <- 0
files <- list.files(trks, recursive = TRUE)
for (n in 1:length(files)) {txtfiles[n] <- paste(trks, "/", files[n], sep="", collapse = "")}

count_m <- 1


## Process text ## nrow(txtfiles)
for (m in 1:1) {
  
    # Clear dataframe
    df_subraces <- data.frame(matrix(0,ncol=143))
    count_r <- 1
    colnames(df_subraces) <- c("trackname", "date", "racenum"
                               , "canyn", "canreason"
                               , "racetype", "horsetype"
                               , "racecriteria", "detailracecriteria"
                               , "trackdistance", "tracktype", "trackrecordhorse", "trackrecordtime", "trackrecorddate"
                               , "purse", "availmoney"
                               , "weather", "wind", "trackspeed"
                               , "valtotal", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9"
                               , "off_at", "start", "timer"
                               , "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7"
                               , "fintime", "st1", "st2", "st3", "st4", "st5", "st6"
                               , "runup", "winname", "winhrstype", "sire", "dam", "stud", "foaldate", "foalloc"
                               , "breeder", "winown", "scratchings"
                               , "lr_date", "lr_rn", "lr_trk", "lr_pos", "pgm_pos", "horse", "jockey", "wgt", "meds_eq", "post_pos", "odds", "comments" 
                               , "wpspool"
                               , "first_num", "first_name", "first_win", "first_place", "first_show"
                               , "second_num", "second_name", "second_place", "second_show"
                               , "third_num", "third_name", "third_show"
                               , "exactbetunit", "exactwn", "exactpayoff", "exactpool"
                               , "trifbetunit", "trifwn", "trifpayoff", "trifpool"
                               , "superbetunit", "superwn", "superpayoff", "superpool"
                               , "train1", "train2", "train3", "train4", "train5", "train6", "train7", "train8", "train9", "train10", "train11", "train12", "train13", "train14", "train15", "train16"
                               , "owners1", "owners2", "owners3", "owners4", "owners5", "owners6", "owners7", "owners8", "owners9", "owners10", "owners11", "owners12", "owners13", "owners14", "owners15", "owners16"
                               , "foot1", "foot2", "foot3", "foot4", "foot5", "foot6", "foot7", "foot8", "foot9", "foot10", "foot11", "foot12", "foot13", "foot14", "foot15", "foot16"
                               )
    
  
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
      
           
    ## Parse each variable
    
    # Track & race row
      #trk_row <- grep("^.*- Race", parse_race)
      trk_row <- 1
      trk_tmp <- re_matches(parse_race[trk_row], rex(maybe("\f"), capture(non_puncts, name = "trackname"), space, "-", space, capture(alphas, space, any_digits, ",", space, any_digits, name = "date"), non_alphas, "Race", space, capture(any_digits, name = "racenum")))
      
    # Check for cancellation
      can_tmp <- data.frame(matrix(0,ncol=2))
      colnames(can_tmp) <- c("canyn", "canreason")
      can_tmp$canyn <- "n"; can_tmp$canreason <- ""
      if (length(grep("Cancelled",parse_race[trk_row+1])) == 1) {
        can_row <- trk_row+1
        can_tmp <- re_matches(parse_race[can_row], rex(capture("Cancelled", name = "canyn"), maybe(" - "), capture(anything, name = "canreason")))
        can_tmp$canyn <- "y"
        fill_tmp <- data.frame(matrix(NaN,ncol=45))
        df_subraces[r,] <- rbind(data.frame(trk_tmp, can_tmp, fill_tmp))
        next
      }
      
    # Race type row - STILL TO DO: race type additional info (eg: arabian stakes)
      ##grep(('^.*CLAIMING|^.*STAKES|^.*ALLOWANCE|^.*SPEED INDEX RACE|^.*SPECIAL WEIGHT|^.*MAIDEN'),parse_race)
      rtype_row <- trk_row + 1
      rtype_tmp <- re_matches(parse_race[rtype_row], rex(capture(anything, name = "racetype"), " - ", capture(anything, name = "horsetype")))
      #print(rtype_row); print(rtype_tmp); Sys.sleep(2)
      
    # Track distance & type row - STILL TO DO: when trackrecordtime is blank, take care of "originally scheduled for"
      ttype_row <- grep(('^.*Track Record:|^.*On The'),parse_race)
      ttype_tmp <- re_matches(parse_race[ttype_row], rex(maybe("About "), capture(anything, name = "trackdistance"), " On The ", capture(anything, name = "tracktype"), " Track Record: (", capture(anything, name = "trackrecordhorse"), " -", maybe(space), maybe(capture(anything, name = "trackrecordtime")), maybe(space), "- ", capture(anything, name = "trackrecorddate"), ")"))
      #print(ttype_row); print(ttype_tmp); Sys.sleep(2)
      
    # Race criteria rows
      rc_rowsta <- rtype_row + 1
      rc_rowfin <- ttype_row - 1
      rc_lines <- 0 
      for (s in rc_rowsta:rc_rowfin) {rc_lines[s-rtype_row] <- parse_race[s]}
      rc_one <- paste(rc_lines, sep = "", collapse = " ")
      rc_tmp <- re_matches(rc_one[1], rex(capture(not(punct), name = "racecriteria"),". ", capture(anything, name = "detailracecriteria")))
      #print(rc_rowsta, rc_rowfin); print(rc_tmp); Sys.sleep(2)
      
    # Purse row - STILL TO DO: deal with "check for anything else on this row"Added", "Guaranteed"
      prs_row <- grep("^Purse:",parse_race)
      parse_race[prs_row] <- sub(",","",parse_race[prs_row])
      prs_tmp <- re_matches(parse_race[prs_row], rex("Purse:", space, "$", capture(anything, name = "purse"), maybe(" Added"), maybe(" Guaranteed")))
      #print(prs_row); print(prs_tmp); Sys.sleep(2)
         
    # Available money row
      avm_row <- grep("^Available Money:",parse_race)
      parse_race[avm_row] <- sub(",","",parse_race[avm_row])
      avm_tmp <- re_matches(parse_race[avm_row], rex("Available Money:", space, "$", capture(anything, name = "availmoney")))
      #print(avm_row); print(avm_tmp); Sys.sleep(2)

    # Weather row
      wea_row <- grep("^Weather:",parse_race)
      wea_tmp <- re_matches(parse_race[wea_row], rex("Weather:", space, capture(alphas, name = "weather"), space, maybe("Wind:", space, capture(alphas, name = "wind"), space), "Track:", space, capture(alphas, name = "trackspeed")))
      weather <- wea_tmp[1]; wind <- wea_tmp[2]; trackspeed <- wea_tmp[3]
      #print(wea_row); print(wea_tmp); Sys.sleep(2)
            
    # Value of race row
      vor_rowsta <- avm_row + 1
      vor_rowfin <- wea_row - 1
      vor_lines <- 0; s <- 1
      for (s in vor_rowsta:vor_rowfin) {vor_lines[s-avm_row] <- parse_race[s]}
      vor_one <- paste(vor_lines, sep = "", collapse = " ")
      vor_one <- gsub(",","", vor_one); vor_one <- gsub("  "," ", vor_one)
      if (length(grep("US",vor_one)) == 0) {
        vor_tmp <- re_matches(vor_one, rex("Value of Race: $", capture(numbers, name = "valtotal")
                                           , maybe(" 1st $", capture(numbers, name = "val1"))
                                           , maybe(" 2nd $", capture(numbers, name = "val2"))
                                           , maybe(" 3rd $", capture(numbers, name = "val3"))
                                           , maybe(" 4th $", capture(numbers, name = "val4"))
                                           , maybe(" 5th $", capture(numbers, name = "val5"))
                                           , maybe(" 6th $", capture(numbers, name = "val6"))
                                           , maybe(" 7th $", capture(numbers, name = "val7"))
                                           , maybe(" 8th $", capture(numbers, name = "val8"))
                                           , maybe(" 9th $", capture(numbers, name = "val9"))))
        
        
      } else {
        vor_tmp <- re_matches(vor_one, rex("Value of Race: $", numbers, " (US$", capture(numbers, name = "valtotal"), ")"
                                           , maybe(" 1st $", numbers, " (US$", capture(numbers, name = "val1"), ")")
                                           , maybe(" 2nd $", numbers, " (US$", capture(numbers, name = "val2"), ")")
                                           , maybe(" 3rd $", numbers, " (US$", capture(numbers, name = "val3"), ")")
                                           , maybe(" 4th $", numbers, " (US$", capture(numbers, name = "val4"), ")")
                                           , maybe(" 5th $", numbers, " (US$", capture(numbers, name = "val5"), ")")
                                           , maybe(" 6th $", numbers, " (US$", capture(numbers, name = "val6"), ")")
                                           , maybe(" 7th $", numbers, " (US$", capture(numbers, name = "val7"), ")")
                                           , maybe(" 8th $", numbers, " (US$", capture(numbers, name = "val8"), ")")
                                           , maybe(" 9th $", numbers, " (US$", capture(numbers, name = "val9"), ")")))
    }

    # Off at row  
      off_row <- grep("^Off at:", parse_race)
      off_tmp <- re_matches(parse_race[off_row], rex("Off at: ", capture(non_spaces, name = "off_at"), " Start: ", capture(not(" Timer:"), name = "start"), maybe(" Timer: ", capture(anything, name = "timer"))))
     
    # Fractional times row  
      ft_row <- grep("^Fractional Times:", parse_race)
      ft_tmp <- re_matches(parse_race[ft_row], rex("Fractional Times:", space, capture(not(space), name = "ft1"), 
                                                   maybe(space, capture(not(space), name = "ft2")),
                                                   maybe(space, capture(not(space), name = "ft3")),
                                                   maybe(space, capture(not(space), name = "ft4")),
                                                   maybe(space, capture(not(space), name = "ft5")),
                                                   maybe(space, capture(not(space), name = "ft6")),
                                                   maybe(space, capture(not(space), name = "ft7")),
                                                   space, "Final Time: ", anything))
    
    # Final time row  
      fint_row <- grep("*Final Time:", parse_race)
      fint_tmp <- re_matches(parse_race[fint_row], rex(anything, "Final Time:", space, capture(anything, name = "fintime")))
      
    # Split times row                           
      st_row <- grep("^Split Times:", parse_race)
      if (length(st_row) == 1) {
        st_tmp <- re_matches(parse_race[st_row], rex("Split Times:", space, "(",capture(not(")"), name = "st1"), ")", 
                                                   maybe(space, "(",capture(not(")"), name = "st2"), ")"),
                                                   maybe(space, "(",capture(not(")"), name = "st3"), ")"),
                                                   maybe(space, "(",capture(not(")"), name = "st4"), ")"),
                                                   maybe(space, "(",capture(not(")"), name = "st5"), ")"),
                                                   maybe(space, "(",capture(not(")"), name = "st6"), ")")))
      } else {st_tmp <- data.frame(NaN,NaN,NaN,NaN,NaN,NaN); colnames(st_tmp) <- c("st1","st2","st3","st4","st5","st6")}

    # Run up row  
      ru_row <- grep("^Run-Up:", parse_race)  
      ru_tmp <- re_matches(parse_race[ru_row], rex("Run-Up:", space, capture(anything, name = "runup")))
      
    # Winner, Breeder & Scratched Horses rows         ## AMY NEED TO AMEND FOR NO SCRATCHINGS
      win_row <- grep("^Winner: ", parse_race)
      breed_row <- grep("^Breeder: ", parse_race)
      scratch_row <- grep("^Scratched Horse", parse_race)
      pp_row <- grep("^Past Performance Running", parse_race)
      
      win_tmp <- re_matches(parse_race[win_row], rex("Winner:", space, capture(not(","), name = "winname"), ",", space, capture(not(","), name = "winhrstype"), ", by ", capture(not(" o"), name = "sire"), " out of ", capture(not(","), name = "dam"), ", by ", capture(not("."), name = "stud"), ". Foaled ", capture(not(" in"), name = "foaldate"), " in ", capture(not("."), name = "foalloc"), "."))
      breed_tmp <- re_matches(parse_race[breed_row], rex("Breeder: ", capture(not(" Winning"), name = "breeder"), " Winning Owner: ", capture(anything, name = "winown")))
      scratch_tmp <- re_matches(parse_race[scratch_row], rex("Scratched Horse(s): ", capture(anything, name = "scratchings")))
      
    # Horse table rows
      lr_row <- grep("^Last Raced Pgm Horse", parse_race)
  
      ht_starow <- lr_row + 1
      if(length(ft_row)==0){ht_finrow <- ru_row - 1} else {ht_finrow <- ft_row - 1}
      t <- seq(ht_starow, ht_finrow, 1)
      ht_nrow <- length(t)
      df_htrow <- data.frame(lr_date = character(), lr_rn = numeric(), lr_trk = character(), lr_pos = numeric(), pgm_pos = numeric(), horse = character(), jockey = character(), wgt = numeric(), meds_eq = character(), post_pos = numeric(), odds = character(), comments = character(), stringsAsFactors = FALSE)
      #colnames(df_htrow) <- c("lr_date", "lr_rn", "lr_trk", "lr_pos", "pgm_pos", "horse", "jockey", "wgt", "meds_eq", "post_pos", "odds", "comments")
      count_t <- 1
      
      
      
      for (t in ht_starow:ht_finrow) {
      

        ht_onesent <- parse_race[t]
        ht_row <- re_matches(ht_onesent, rex(rex(start, start, capture(non_spaces, name = "lr_date"),
                                         maybe(space), 
                                         maybe(group(capture(digits, name = "lr_rn"), 
                                         capture(alphas, name = "lr_trk"), 
                                         capture(digits, name = "lr_pos"))), 
                                         spaces, group(capture(digits, name = "pgm_pos"),maybe(alphas)),
                                         spaces, capture(anything, name = "horse"), 
                                         " (", capture(something, name = "jockey"),
                                         ")", spaces, group(capture(non_spaces, name = "wgt")),
                                         spaces, maybe(capture(group(non_spaces,maybe(spaces,not(spaces %or% digits))), name="meds_eq")),
                                         spaces, capture(digits, name = "post_pos"), anything,
                                         group(capture(group(digits,".",digits), name = "odds"), maybe("*")),
                                         spaces, capture(anything, name = "comments"))))
            
                                                
         df_htrow[count_t,] <- ht_row
         count_t <- count_t + 1
      }
      
      
    # Total WPS Pool row
      wpspl_row <- grep("Total WPS Pool:", parse_race)
      if (length(wpspl_row) == 1) {
        parse_race[wpspl_row] <- sub(",","",parse_race[wpspl_row])
        wpspl_tmp <- re_matches(parse_race[wpspl_row], rex("Total WPS Pool:", space, "$", capture(anything, name = "wpspool")))
      } else {wpspl_tmp <- data.frame(NaN); colnames(wpspl_tmp) <- c("wpspool")}
  
   #  WPS Results rows                                                ## BUILD IN TIES/DEAD HEATS
      wpsr_row <- grep("^Pgm Horse Win Place Show", parse_race)
      exotics_row <- grep("^Wager Type Winning Numbers", parse_race)
      count_places <- exotics_row - wpsr_row - 1
      
      if(count_places == 3) {
        first_row <- wpsr_row + 1; first_tmp <- re_matches(parse_race[first_row], rex(capture(numbers, name = "first_num"), space, capture(any_non_numbers, name = "first_name"), space, capture(anything, name = "first_win"), space, capture(anything, name = "first_place"), space, capture(anything, name = "first_show")))
        second_row <- wpsr_row + 2; second_tmp <- re_matches(parse_race[second_row], rex(capture(numbers, name = "second_num"), space, capture(any_non_numbers, name = "second_name"), space, capture(anything, name = "second_place"), space, capture(anything, name = "second_show")))
        third_row <- wpsr_row + 3; third_tmp <- re_matches(parse_race[third_row], rex(capture(numbers, name = "third_num"), space, capture(any_non_numbers, name = "third_name"), space, capture(anything, name = "third_show")))
      }
      
      
        
    # Exotics rows - look for Exacta/Perfecta(1,2), Quinella(1,2 or 2,1), Trifecta(1,2,3), Superfecta(1,2,3,4)

    # Exacta/Perfecta rows
      exact_row <- grep("Exacta", parse_race)
      if (length(exact_row) == 0) {exact_row <- grep("Perfecta", parse_race)}
      if (length(exact_row) == 0) {exact_row <- grep("Exactor", parse_race)}
      if (length(exact_row) == 1) {
        parse_race[exact_row] <- sub(",","",parse_race[exact_row])
        exact_tmp <- re_matches(parse_race[exact_row], rex("$", capture(anything, name = "exactbetunit"), space, letters, space, capture(anything, name = "exactwn"), space, capture(anything, name = "exactpayoff"), space, capture(anything, name = "exactpool")))
      } else {exact_tmp <- data.frame(matrix(NaN, ncol = 4))}
        
    # Trifecta row
      trif_row <- grep("Trifecta", parse_race)
      if (length(trif_row) == 0) {trif_row <- grep("Triactor", parse_race)}
      if (length(trif_row) == 1) {
        parse_race[trif_row] <- sub(",","",parse_race[trif_row])
        trif_tmp <- re_matches(parse_race[trif_row], rex("$", capture(anything, name = "trifbetunit"), space, letters, space, capture(anything, name = "trifwn"), space, capture(anything, name = "trifpayoff"), space, capture(anything, name = "trifpool")))
      } else {trif_tmp <- data.frame(matrix(NaN, ncol = 4))}
    
    # Superfecta row
      super_row <- grep("Superfecta", parse_race)
      if (length(super_row) == 1) {
        parse_race[super_row] <- sub(",","",parse_race[super_row])
        super_tmp <- re_matches(parse_race[super_row], rex("$", capture(anything, name = "superbetunit"), space, letters, space, capture(anything, name = "superwn"), space, capture(anything, name = "superpayoff"), space, capture(anything, name = "superpool")))
      } else {super_tmp <- data.frame(matrix(NaN, ncol = 4))}

      
    # PP Running Line Preview rows
#      pprl_row <- grep("^Pgm Horse Name", parse_race)
#      pprl_tmp <- re_matches(parse_race[pprl_row], rex("Pgm Horse Name", space, maybe(capture(graphs, name = "first"), space), maybe(capture(graphs, name = "second"), space), maybe(capture(graphs, name = "third"), space), maybe(capture(graphs, name = "fourth"), space), "Str", space, "Fin"))
#      ntimepts <- length(which(pprl_tmp != "")) + 2
      
#      pprl_starow <- pprl_row + 1
#      pprl_finrow <- train_row - 1
#      u <- seq(pprl_starow, pprl_finrow, 1)
#      pprl_nrow <- length(u)
#      df_pprlrow <- data.frame(matrix(0,ncol=12))
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
      
      
      
      # Trainers & owners rows 
      train_row <- grep("^Trainers: ", parse_race)
      owners_row <- grep("^Owners: ", parse_race)
      foot_row <- grep("^Footnotes", parse_race)
      train_lines <- 0
      owners_lines <- 0
      
      for(x in 1:(owners_row-train_row)) {train_lines[x] <- parse_race[x+(train_row-1)]}
      train_one <- paste(train_lines, sep = "", collapse = " ")
      
      for(y in 1:(foot_row-owners_row)) {owners_lines[y] <- parse_race[y+(owners_row-1)]}
      owners_one <- paste(owners_lines, sep = "", collapse = " ")
      
      train_tmp <- re_matches(train_one[1], rex("Trainers: ", numbers, maybe(space), capture(not(";"), name = "train1"), 
                                                maybe(";", maybe(space), capture(not(";"), name="train2")),
                                                maybe(";", maybe(space), capture(not(";"), name="train3")),
                                                maybe(";", maybe(space), capture(not(";"), name="train4")),
                                                maybe(";", maybe(space), capture(not(";"), name="train5")),
                                                maybe(";", maybe(space), capture(not(";"), name="train6")),
                                                maybe(";", maybe(space), capture(not(";"), name="train7")),
                                                maybe(";", maybe(space), capture(not(";"), name="train8")),
                                                maybe(";", maybe(space), capture(not(";"), name="train9")),
                                                maybe(";", maybe(space), capture(not(";"), name="train10")),
                                                maybe(";", maybe(space), capture(not(";"), name="train11")),
                                                maybe(";", maybe(space), capture(not(";"), name="train12")),
                                                maybe(";", maybe(space), capture(not(";"), name="train13")),
                                                maybe(";", maybe(space), capture(not(";"), name="train14")),
                                                maybe(";", maybe(space), capture(not(";"), name="train15")),
                                                maybe(";", maybe(space), capture(not(";"), name="train16"))))
      
      
      owners_tmp <- re_matches(owners_one[1], rex("Owners: ", numbers, maybe(space), capture(not(";"), name = "owners1"), 
                                                  maybe(";", maybe(space), capture(not(";"), name="owners2")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners3")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners4")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners5")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners6")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners7")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners8")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners9")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners10")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners11")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners12")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners13")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners14")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners15")),
                                                  maybe(";", maybe(space), capture(not(";"), name="owners16"))))
      
      
      # Footnotes
      foot_row <- grep("^Footnotes", parse_race)
      copy_row <- grep("^Copyright", parse_race)
      winner_row <- grep("^Winner sold", parse_race)
      foot_lines <- 0
      
      if (length(winner_row) == 1) {
        for(z in 1:(winner_row-foot_row)) {foot_lines[z] <- parse_race[z+(foot_row)]}
        foot_one <- paste(foot_lines, sep = "", collapse = " ")
      } else {
        for(z in 1:(copy_row-foot_row-1)) {foot_lines[z] <- parse_race[z+(foot_row)]}
        foot_one <- paste(foot_lines, sep = "", collapse = " ")
      }
      
      foot_one <- sub(regex("([a-z])(\\.)( [a-z])"), regex("\\1,\\3") , foot_one)
      foot_one <- sub(regex("([a-z])(,)([a-z])"), regex("\\1, \\3") , foot_one)
      
      foot_tmp <- re_matches(foot_one[1], rex(capture(not("."), name = "foot1"), 
                                                  maybe(".", maybe(space), capture(not("."), name="foot2")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot3")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot4")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot5")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot6")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot7")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot8")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot9")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot10")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot11")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot12")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot13")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot14")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot15")),
                                                  maybe(".", maybe(space), capture(not("."), name="foot16"))))
      
      
      # Clear dataframe
      #df_subraces <- data.frame(matrix(0,ncol=12)) 


      df_subraces[count_r,] <- c(trk_tmp, can_tmp, rtype_tmp, rc_tmp, ttype_tmp, prs_tmp, avm_tmp, wea_tmp, vor_tmp, off_tmp, ft_tmp, fint_tmp, st_tmp, ru_tmp, win_tmp, breed_tmp, scratch_tmp, df_htrow[1,], wpspl_tmp, first_tmp, second_tmp, third_tmp, exact_tmp, trif_tmp, super_tmp, train_tmp, owners_tmp, foot_tmp)
      count_r <- count_r + 1
      
      
      
      
      #df_subraces[r,] <- rbind(data.frame(trk_tmp, can_tmp, rtype_tmp, ttype_tmp, rc_tmp, prs_tmp, avm_tmp, wea_tmp, vor_tmp, off_tmp, wpspl_tmp, exact_tmp, trif_tmp, super_tmp, lr_tmp))
  
    }
          
df_final <- rbind(df_final,df_subraces)
        
}                               
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
