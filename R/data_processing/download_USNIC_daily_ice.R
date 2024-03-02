library(tidyverse)

date_seq <- seq(as.Date("2022-01-01"), as.Date("2022-12-31"), "day")
d = strftime(date_seq, format = "%d")
m = strftime(date_seq, format = "%m")
y = strftime(date_seq, format = "%Y")

loop_table <- as.data.frame(cbind(d,m,y))

for (i in 1:nrow(loop_table)){
  tryCatch({
  y_tmp = loop_table[i,3]
  m_tmp = loop_table[i,2]
  d_tmp = loop_table[i,1]
  print(paste(y_tmp,m_tmp,d_tmp,sep="..."))
  temp_path = paste("data/USNIC_ice/USNIC_daily_",y_tmp,"_",m_tmp,"_",d_tmp,".zip",sep="")
  dl_link = paste("https://usicecenter.gov/File/DownloadArchive?prd=26",m_tmp,d_tmp,y_tmp,sep="")
  download.file(dl_link,temp_path)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
}


