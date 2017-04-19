## SUPPORTING DOC FOR DATA PARSE VARIABLES


col_names <- c("trackname", "date", "racenum"
               , "canyn", "canreason"
               , "racetype", "horsetype"
               , "racecriteria", "detailracecriteria"
               , "trackdistance", "tracktype", "trackrecordhorse", "trackrecordtime", "trackrecorddate"
               , "purse"
               , "availmoney"
               , "weather", "wind", "trackspeed"
               , "valtotal", "val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9"
               , "off_at", "start", "timer"
               , "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7"
               , "fintime"
               , "st1", "st2", "st3", "st4", "st5", "st6"
               , "runup"
               , "winname", "winhrstype", "sire", "dam", "stud", "foaldate", "foalloc"
               , "breeder", "winown"
               , "scratchings"
               #, "lr_date", "lr_rn", "lr_trk", "lr_pos", "pgm_pos", "horse", "jockey", "wgt", "meds_eq", "post_pos", "odds", "comments" 
               , "wpspool"
               # , "first_num", "first_name", "first_win", "first_place", "first_show"
               # , "second_num", "second_name", "second_place", "second_show"
               # , "third_num", "third_name", "third_show"
               # , "exactbetunit", "exactwn", "exactpayoff", "exactpool"
               # , "trifbetunit", "trifwn", "trifpayoff", "trifpool"
               # , "superbetunit", "superwn", "superpayoff", "superpool"
               , "train1", "train2", "train3", "train4", "train5", "train6", "train7", "train8", "train9", "train10", "train11", "train12", "train13", "train14", "train15", "train16"
               , "owners1", "owners2", "owners3", "owners4", "owners5", "owners6", "owners7", "owners8", "owners9", "owners10", "owners11", "owners12", "owners13", "owners14", "owners15", "owners16"
               , "foot1", "foot2", "foot3", "foot4", "foot5", "foot6", "foot7", "foot8", "foot9", "foot10", "foot11", "foot12", "foot13", "foot14", "foot15", "foot16"
)

df_merge <- c(trk_tmp
          , can_tmp
          , rtype_tmp
          , rc_tmp
          , ttype_tmp
          , prs_tmp
          , avm_tmp
          , wea_tmp
          , vor_tmp
          , off_tmp
          , ft_tmp
          , fint_tmp
          , st_tmp
          , ru_tmp
          , win_tmp
          , breed_tmp
          , scratch_tmp
          , wpspl_tmp
          # , first_tmp
          # , second_tmp
          # , third_tmp
          # , exact_tmp
          # , trif_tmp
          # , super_tmp
          , train_tmp
          , owners_tmp
          , foot_tmp, use.names=T)




