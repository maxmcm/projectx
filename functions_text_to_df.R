### FUNCTIONS ###

track_fn <- function(x,y){
  trk_tmp <- re_matches(parse_race[trk_row], rex(maybe("\f"), capture(non_puncts, name = "trackname"), space, "-", space, capture(alphas, space, any_digits, ",", space, any_digits, name = "date"), non_alphas, "Race", space, capture(any_digits, name = "racenum")))
  return(trk_tmp)
}

canceled_fn <- function(x,y,z){
  can_tmp <- data.frame(matrix(0,ncol=2))
  colnames(can_tmp) <- c("canyn", "canreason")
  can_tmp$canyn <- "n"; can_tmp$canreason <- ""
  if (length(grep("Cancelled",parse_race[can_row])) == 1) {
    can_tmp <- re_matches(parse_race[can_row], rex(capture("Cancelled", name = "canyn"), maybe(" - "), capture(anything, name = "canreason")))
    can_tmp$canyn <- "y"
    fill_tmp <- data.frame(matrix(NaN,ncol=45))
    df_subraces[r,] <- rbind(data.frame(trk_tmp, can_tmp, fill_tmp))
    next
  }
  return(can_tmp)
}

racetype_fn <- function(x,y){
  rtype_tmp <- re_matches(parse_race[rtype_row], rex(capture(anything, name = "racetype"), " - ", capture(anything, name = "horsetype")))
  return(rtype_tmp)
}

tracktype_fn <- function(x,y){
  ttype_tmp <- re_matches(parse_race[ttype_row], rex(maybe("About "), capture(anything, name = "trackdistance"), " On The ", capture(anything, name = "tracktype"), " Track Record: (", capture(anything, name = "trackrecordhorse"), " -", maybe(space), maybe(capture(anything, name = "trackrecordtime")), maybe(space), "- ", capture(anything, name = "trackrecorddate"), ")"))
  return(ttype_tmp)
}

racecriteria_fn <- function(x,y,z) {
  s <- 1
  rc_rowsta <- rtype_row + 1
  rc_rowfin <- ttype_row - 1
  rc_lines <- 0 
  for (s in rc_rowsta:rc_rowfin) {rc_lines[s-rtype_row] <- parse_race[s]}
  rc_one <- paste(rc_lines, sep = "", collapse = " ")
  rc_tmp <- re_matches(rc_one[1], rex(capture(not(punct), name = "racecriteria"),". ", capture(anything, name = "detailracecriteria")))
  return(rc_tmp)
}

purse_fn <- function(x,y){
  parse_race[prs_row] <- sub(",","",parse_race[prs_row])
  prs_tmp <- re_matches(parse_race[prs_row], rex("Purse:", space, "$", capture(anything, name = "purse"), maybe(" Added"), maybe(" Guaranteed")))
  return(prs_tmp)
}

availmoney_fn <- function(x,y){
  parse_race[avm_row] <- sub(",","",parse_race[avm_row])
  avm_tmp <- re_matches(parse_race[avm_row], rex("Available Money:", space, "$", capture(anything, name = "availmoney")))
  return(avm_tmp)
}

weather_fn <- function(x,y){
  wea_tmp <- re_matches(parse_race[wea_row], rex("Weather:", space, capture(alphas, name = "weather"), space, maybe("Wind:", space, capture(alphas, name = "wind"), space), "Track:", space, capture(alphas, name = "trackspeed")))
  weather <- wea_tmp[1]; wind <- wea_tmp[2]; trackspeed <- wea_tmp[3]
  return(wea_tmp)
}

valueofrace_fn <- function(x,y,z){
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
  return(vor_tmp)
}

offat_fn <- function(x,y){
  off_tmp <- re_matches(parse_race[off_row], rex("Off at: ", capture(non_spaces, name = "off_at"), " Start: ", capture(not(" Timer:"), name = "start"), maybe(" Timer: ", capture(anything, name = "timer"))))
  return(off_tmp)
}

fractimes_fn <- function(x,y){
  if(length(ft_row) == 1) {
  ft_tmp <- re_matches(parse_race[ft_row], rex("Fractional Times:", space, capture(not(space), name = "ft1"), 
                                               maybe(space, capture(not(space), name = "ft2")),
                                               maybe(space, capture(not(space), name = "ft3")),
                                               maybe(space, capture(not(space), name = "ft4")),
                                               maybe(space, capture(not(space), name = "ft5")),
                                               maybe(space, capture(not(space), name = "ft6")),
                                               maybe(space, capture(not(space), name = "ft7")),
                                               space, "Final Time: ", anything))
  } else {ft_tmp <- data.frame(NaN,NaN,NaN,NaN,NaN,NaN,NaN); colnames(ft_tmp) <- c("ft1","ft2","ft3","ft4","ft5","ft6","ft7")}
  return(ft_tmp)
}

splittimes_fn <- function(x,y){
  if (length(st_row) == 1) {
    st_tmp <- re_matches(parse_race[st_row], rex("Split Times:", space, "(",capture(not(")"), name = "st1"), ")", 
                                                 maybe(space, "(",capture(not(")"), name = "st2"), ")"),
                                                 maybe(space, "(",capture(not(")"), name = "st3"), ")"),
                                                 maybe(space, "(",capture(not(")"), name = "st4"), ")"),
                                                 maybe(space, "(",capture(not(")"), name = "st5"), ")"),
                                                 maybe(space, "(",capture(not(")"), name = "st6"), ")")))
  } else {st_tmp <- data.frame(NaN,NaN,NaN,NaN,NaN,NaN); colnames(st_tmp) <- c("st1","st2","st3","st4","st5","st6")}
  return(st_tmp)
}

finaltime_fn <- function(x,y){
  if(length(fint_row) == 1) {
    fint_tmp <- re_matches(parse_race[fint_row], rex(anything, "Final Time:", space, capture(anything, name = "fintime")))
  } else {fint_tmp <- data.frame(NaN); colnames(fint_tmp) <- "fintime"}
  return(fint_tmp)
}

runup_fn <- function(x,y){
  ru_tmp <- re_matches(parse_race[ru_row], rex("Run-Up:", space, capture(anything, name = "runup")))
  return(ru_tmp)
}

winner_fn <- function(x,y){
  win_tmp <- re_matches(parse_race[win_row], rex("Winner:", space, capture(not(","), name = "winname"), ",", space, capture(not(","), name = "winhrstype"), ", by ", capture(not(" o"), name = "sire"), " out of ", capture(not(","), name = "dam"), ", by ", capture(not("."), name = "stud"), ". Foaled ", capture(not(" in"), name = "foaldate"), " in ", capture(not("."), name = "foalloc"), "."))
  return(win_tmp)
}

breeder_fn <- function(x,y){
  breed_tmp <- re_matches(parse_race[breed_row], rex("Breeder: ", capture(not(" Winning"), name = "breeder"), " Winning Owner: ", capture(anything, name = "winown")))
  return(breed_tmp)
}

scratching_fn <- function(x,y){
  if(length(scratch_row) == 1) {
    scratch_tmp <- re_matches(parse_race[scratch_row], rex("Scratched Horse(s): ", capture(anything, name = "scratchings")))
  } else {scratch_tmp <- data.frame(NaN); colnames <- "scratchings"}
  return(scratch_tmp)
}

horsetable_fn <- function(x,y,z,w,v){
  count_t <- 1
  ifelse(length(ft_row)==0, ifelse(length(fint_row)==0, ht_finrow <- ru_row - 1, ht_finrow <- fint_row -1), ht_finrow <- ft_row - 1) ## negates - possibly not in correct order
  ht_nrow <- ht_finrow - ht_starow
  df_htrow <- data.frame(lr_date = character(), lr_rn = numeric(), lr_trk = character(), lr_pos = numeric(), pgm_pos = numeric(), horse = character(), jockey = character(), wgt = numeric(), meds_eq = character(), post_pos = numeric(), odds = character(), comments = character(), stringsAsFactors = FALSE)
  
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
                                             maybe(group(capture(group(digits,".",digits), name = "odds"), maybe("*"))),
                                             spaces, capture(anything, name = "comments"))))
    df_htrow[count_t,] <- ht_row
    count_t <- count_t + 1
  } 
  return(df_htrow)
}

wpspool_fn <- function(x,y){
  if (length(wpspl_row) == 1) {
    parse_race[wpspl_row] <- sub(",","",parse_race[wpspl_row])
    wpspl_tmp <- re_matches(parse_race[wpspl_row], rex("Total WPS Pool:", space, "$", capture(anything, name = "wpspool")))
  } else {wpspl_tmp <- data.frame(NaN); colnames(wpspl_tmp) <- c("wpspool")}
  return(wpspl_tmp)
}



trainers_fn <- function(x,y){
  train_lines <- 0
  owners_row <- grep("^Owners: ", parse_race)
  for(x in 1:(owners_row-train_row)) {train_lines[x] <- parse_race[x+(train_row-1)]}
  train_one <- paste(train_lines, sep = "", collapse = " ")
  
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
  return(train_tmp)
}

owners_fn <- function(x,y){
  foot_row <- grep("^Footnotes", parse_race)
  owners_lines <- 0
  for(y in 1:(foot_row-owners_row)) {owners_lines[y] <- parse_race[y+(owners_row-1)]}
  owners_one <- paste(owners_lines, sep = "", collapse = " ")
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
  return(owners_tmp)
}

footnotes_fn <- function(x,y){
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
  
  return(foot_tmp)
}