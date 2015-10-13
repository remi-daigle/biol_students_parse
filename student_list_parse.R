students <- read.table("student_list.txt", sep="\t", quote="", stringsAsFactors=FALSE)

df = data.frame(matrix(NA, 1, 8,
                       dimnames=list(c(), c("year","name", "title", "sex","supervisor","hometown","pud","currentstatus"))),
                stringsAsFactors=F)
newrow <- df
n <- 0
for(l in seq(students$V1)){
  # assign temporary variable
  temp <- students[l,]
  if(grepl("^_",temp)) temp <- gsub("^ ","",temp)
  
  
  # assign year
  if(grepl(pattern="^[1-2][0-9]{3}\\:",temp)){
    oldy <- as.numeric(substr(temp,1,4))
  } else {
    df$year[n] <- oldy
  }
  
  
  # assign name and title (assumes title is always below name) and move to next row
  if(grepl(pattern="^[0-9]\\.|^[1-9][0-9]\\.",temp)){
    n <- n+1
    df <- rbind(df,newrow)
    df$name[n] <- temp
    df$title[n] <- students[l+1,]
  } 
  
  # assign sex
  if(grepl(pattern="Sex\\:",temp)){
    df$sex[n] <- temp
  } 
  
  # assign sex
  if(grepl(pattern="Supervisors?\\(s\\)\\:",temp)){
    df$supervisor[n] <- temp
  } 
  
  # assign Hometown
  if(grepl(pattern="Hometown\\:",temp)){
    df$hometown[n] <- temp
  } 
  
}
