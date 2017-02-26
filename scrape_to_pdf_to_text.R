##########################################
#                                        #
#  Web scraping to text file - SMc 2017  #
#                                        #
##########################################


# RSelenium package: https://cran.r-project.org/web/packages/RSelenium/
#
# RSelenium basics: https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
#
# RSelenium docker setup: https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-docker.html
#
# Privoxy download: https://www.privoxy.org/sf-download-mirror/
#
# Tor download (Expert Bundle not Tor Browser): https://www.torproject.org/download/download.html.en
#
# Using Privoxy with Tor: https://www.privoxy.org/faq/misc.html#TOR


### 1) ENSURE PRIVOXY IS RUNNING
### 2) START TOR EXTERNALLY (LEAVE WINDOW OPEN)
### 3) CHANGE IEXPLORE PROXY (127.0.0.1:8118)
### 4) START SELENIUM DOCKER IMAGE


# Libraries
library(RSelenium)


# File Paths
rt <- file.path("E:","LOCAL_GIT","projectx")
qp <- file.path(rt,"includes","qpdf","bin","qpdf.exe --decrypt")
xp <- file.path(rt,"includes","xpdf","bin64","pdftotext.exe -raw")
ldg <- file.path("E:","Horse","Landing") # also change in firefox profile
trks <- file.path("E:","Horse","Tracks")


# Connections 
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
    remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L, extraCapabilities = fprof)
    
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

