## NO CAPTCHA YET

library(XML)
library(RSelenium)
library(mail)

### ENSURE TOR RUNNING & IEXPLORE PROXY CHANGED

### START TOR & RSELENIUM SERVER EXTERNALLY!!!!!!!!!!!

# Start RSelenium server
#RSelenium::checkForServer()
#RSelenium::startServer()
# Get new IP
#writeLines('AUTHENTICATE\r\nSIGNAL NEWNYM\r\n', con=tor_con)

# Set up file paths
ldg <- file.path("E:","Horse","Landing") # also change in firefox profile
trks <- file.path("E:","Horse","Tracks")
qp <- file.path("C:","qpdf","bin","qpdf.exe --decrypt")
xp <- file.path("C:","xpdf","bin64","pdftotext.exe -raw")

# Set up tor connection 
tor_con <- socketConnection(host="127.0.0.1",port=9151)

# Set up random url sequence
url_rand <- sample(2:(nrow(df_urls)),(nrow(df_urls)-1))

# For loop to run through random url sequence, fills in the gaps each time it runs
for (i in url_rand[]) {
  
  # Check for existing file
  if (file.exists(file.path(trks, df_urls[i,5], paste(df_urls[i,3],"_", df_urls[i,2], "_", df_urls[i,1], ".txt", sep = "", collapse = ""))) == FALSE) { 
    
    # Check for track folder, if doesn't exist create it 
    if (dir.exists(file.path(trks, df_urls[i,5])) == FALSE) {dir.create(file.path(trks, df_urls[i,5]))}
    
    # Set Firefox profile then make Selenium webdriver connection
    fprof <- makeFirefoxProfile(list(browser.startup.homepage = "about:blank"
                                     , startup.homepage_override_url = "about:blank"
                                     , startup.homepage_welcome_url = "about:blank"
                                     , startup.homepage_welcome_url.additional = "about:blank"
                                     , browser.download.dir = "E:\\Horse\\Landing"
                                     , browser.download.folderList = 2L
                                     , browser.download.manager.showWhenStarting = FALSE
                                     , browser.helperApps.neverAsk.saveToDisk = "application/pdf, application/octet-stream"
                                     , pdfjs.disabled = TRUE
                                     , plugin.scan.plid.all = FALSE
                                     , plugin.scan.Acrobat = 99L))
    remDr <- remoteDriver(extraCapabilities = fprof)
    
    # Open RSel & start at historical results page
    remDr$open(silent = TRUE); Sys.sleep(2)
    remDr$navigate("http://127.0.0.1"); Sys.sleep(5)
    
    #remDr$navigate("www.equibase.com/premium/eqbRaceChartCalendar.cfm"); Sys.sleep(sample(4:8,1))
    
    #remDr$navigate(df_urls[i,7]); Sys.sleep(sample(4:8,1))
    
    # Navigate direct to pdf
    remDr$navigate(df_urls[i,10]); Sys.sleep(10)

    # Find downloaded file
    if (file.exists(paste(ldg, "/eqbPDFChartPlus.cfm", sep = "", collapse = "")) == TRUE) {

      file.rename(paste(ldg, "/eqbPDFChartPlus.cfm", sep = "", collapse = ""), paste(ldg, "/eqbPDFChartPlus.pdf", sep = "", collapse = "")); Sys.sleep(2)
      
      print(file.size(paste(ldg, "/eqbPDFChartPlus.pdf", sep = "", collapse = ""))/1024)
      
      system(paste(qp, paste(ldg, "/eqbPDFChartPlus.pdf", sep = "", collapse = ""), paste(ldg, "/eqbPDFChartPlus_us.pdf", sep = "", collapse = ""))); Sys.sleep(2)
      
      system(paste(xp, paste(ldg, "/eqbPDFChartPlus_us.pdf", sep = "", collapse = ""), paste(trks, df_urls[i,5], paste(df_urls[i,3],"_", df_urls[i,2], "_", df_urls[i,1], ".txt", sep = "", collapse = ""),sep="/",collapse=""))); Sys.sleep(2)
        
      unlink(paste(ldg,"/*",sep="",collapse="")); Sys.sleep(2)
      
    }
    
    # Flush connection & cookies & tor IP
    remDr$deleteAllCookies(); Sys.sleep(2)
    writeLines('AUTHENTICATE\r\nSIGNAL NEWNYM\r\n', con=tor_con); Sys.sleep(5)
    print(file.size(paste(trks, df_urls[i,5], paste(df_urls[i,3],"_", df_urls[i,2], "_", df_urls[i,1], ".txt", sep = "", collapse = ""),sep="/",collapse=""))/1024)
    print(paste(df_urls[i,5],"_",df_urls[i,3],"_", df_urls[i,2], "_", df_urls[i,1], sep = "", collapse = ""))
    remDr$close(); Sys.sleep(sample(5:15,1))
    
    
  }
}

