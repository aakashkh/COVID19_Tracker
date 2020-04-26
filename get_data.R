library(httr)
library(tidyverse)

req <- GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")

file_path <- data.frame(unlist(lapply(content(req)$tree, function(x) x$path)))
colnames(file_path) = c('Path')


file_path <- file_path %>%
  separate(Path,c('base','folder','filename'),'/') %>%
  filter(folder == 'csse_covid_19_daily_reports') %>%
  filter(str_detect(filename,'.csv'))


i<-1
dataset <- tibble()
for (i in seq(i,nrow(file_path))){
  path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',file_path$filename[i])
  daily_data = readr::read_csv(content(GET(path)))
  daily_data$filename <- file_path$filename[i]
  daily_data <- tibble(data.frame(lapply(daily_data, as.character)))
  dataset = bind_rows(dataset,daily_data)
  print(path)
  Sys.sleep(1)
} 

readr::write_csv(dataset,'dataset.csv')




#readr::read_csv(content(GET('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/01-22-2020.csv')))

#out <- lapply(list.files(), FUN = function(x) {
#  m <- mean(read.table(x, header = TRUE)$V4)
#  return(m)
#})
#result <- do.call("cbind", out) #merge a list column-wise
# before writing, you can make column names pretty with colnames()
# e.g. colnames(result) <- c("x01", "x02")
#write.table(result, file = "means.txt")