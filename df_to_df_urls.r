## Process df into df_urls for webscraper to use
## Previously mannualy aggregated df.xlsx needs to be saved as df.csv
## then this script run


#library(RCurl,XML)
#require(RSelenium)

# Set up variables
DD
MM
YYYY
TID
DT # date MM/DD/YYYY - need to use formatc
CTRY

# Set up counters

# Set up urls dataframe
df_urls<-data.frame(matrix(0,ncol=10))


# Foreign tracks
trks_can <- c("AJX", "ABT", "ASD", "DEP", "FE", "GPR", "HST", "KAM", "KIN", "LBG", "MD", "MDA", "MIL", "NP", "PIC", "RPD", "STP", "SND", "WO")
trks_pr <- c("CMR", "PR")

# Load datafile with all dates & tracks, find number of race meets and construct df with all htmls
df_races <- read.csv("E:/Horse/df.csv", header = TRUE, stringsAsFactors = FALSE)
tracks <- colnames(df_races)

# Find out how many races in dataframe
cum <- 0
for (i in 5:ncol(df_races)) {cum <- cum + sum(df_races[i] == '1', na.rm = TRUE)}

# Data into urls dataframe
for (j in 1:nrow(df_races)) {
  for (k in 5:ncol(df_races)) {
    if (df_races[j,k] == 1) {
      TID <- tracks[k]
      if (TID %in% trks_can) {
        CTRY = "CAN"
      } else if (TID == "CMR") {
        CTRY = "PR"
      } else
        CTRY = "USA"
      DD <- df_races[j,3]
      MM <- df_races[j,2]
      YYYY <- df_races[j,1]
      DT <- paste(formatC(MM, width=2, flag="0"), formatC(DD, width = 2, flag = "0"), YYYY, sep = "/", collapse = "")
      
      # Set up html addresses for navigation with RSel (in descending order)
      url_hist <- file.path("http://www.equibase.com", "premium", "eqbRaceChartCalendar.cfm?SAP=TN")
      url_date <- file.path("http://www.equibase.com", "premium", paste("eqpVchartBuy.cfm?mo=", MM, "&da=", DD, "&yr=", YYYY, "&trackco=ALL;ALL&cl=Y",sep = "", collapse = ""))
      url_trak <- file.path("http://www.equibase.com", "premium", paste("eqbPDFChartPlusIndex.cfm?tid=", TID, "&dt=", DT, "&ctry=", CTRY ,sep = "", collapse = ""))
      url_card <- file.path("http://www.equibase.com", "premium", paste("chartEmb.cfm?track=", TID, "&raceDate=", DT, "&cy=", CTRY, sep = "", collapse = ""))
      url_pdf  <- file.path("http://www.equibase.com", "premium", paste("eqbPDFChartPlus.cfm?RACE=A&BorP=P&TID=", TID, "&CTRY=", CTRY, "&DT=", DT, "&DAY=D&STYLE=EQB", sep = "", collapse = ""))
      
      df_row <- c(DD, MM, YYYY, DT, TID, CTRY, url_date, url_trak, url_card, url_pdf)
      print(df_row[1:5])
      
      df_urls <- rbind(df_urls,df_row)
      
      
    }
  }
}


file.create(file.path("E:","Horse",paste("df_urls",".csv", sep = "", collapse = "")))
write.csv(df_urls, file.path("E:","Horse",paste("df_urls",".csv", sep = "", collapse = "")))





