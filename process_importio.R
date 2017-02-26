## Process new import.io files into dataframe
## test change

input_year <- 2010

read_in <- file.path("E:","Horse",paste(input_year,".csv", sep = "", collapse = ""))
read_df <- data.frame(read.csv(read_in))

## Should load trackdf independently

m <- 1
n <- 1
p <- 1

df<-data.frame(matrix(0,ncol=207))
colnames(df)<-c("YYYY","MM","DD",trackdf$abbreviation)

test2 <- subset(read_df, Day > 0, select = Day:Year)

for (m in 1:nrow(test2)) {
  
  mat <- matrix(0, ncol = 207)
  colnames(mat)<-c("YYYY","MM","DD",trackdf$abbreviation)
  
  mat[1,1:3] <- c(test2[m,3], test2[m,2], test2[m,1])
  
  test <- unname(as.matrix(subset(read_df, DD == test2[m,1] & MM == test2[m,2] & YYYY == test2[m,3], select=my_column)))
  
  for (p in 1:length(test)) {
    
    for (n in 1:length(trackdf$track_name)){
      
      if (test[p] == trackdf$track_name[n]) {
        
        mat[1,n+3]<-1
        
      }
    }
  }
  
  df <- rbind(df,mat)
  
}

file.create(file.path("E:","Horse",paste(input_year,"_out.csv", sep = "", collapse = "")))
write.csv(df, file.path("E:","Horse",paste(input_year,"_out.csv", sep = "", collapse = "")))
