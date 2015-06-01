library("XML")
library("httr")
library("stringr")
#library("igraph")
library("dplyr")

# Get the urls
x = url("http://myweb.ncku.edu.tw/~cpcheng/Rbook/index.htm")
htmlCode = readLines(x)
close(x)

URL1 <- htmlCode[grep("><a href=",htmlCode)][c(4:12,14:31)]

p <- c("style='font-size:16.0pt'>","<span lang=EN-US>", "<span lang=EN-US", "><span", "<a href=","lang=EN-US style='font-size:16.0pt'><a href=","lang=EN-US", " ", "\"", ">")

for(i in 1:length(p)){
    URL1 = gsub(p[i],"", URL1)
}

# Make directories
CH_DIR <- c('01','02','03','04','05','06','07','08','09','10','11','12')
for(i in 1:length(CH_DIR))
    if (!file.exists(CH_DIR[i])) {
        dir.create(CH_DIR[i])
        dir.create(paste0(CH_DIR[i],"/data") )
    }


# Download files
HomeURL <- "http://myweb.ncku.edu.tw/~cpcheng/Rbook/"
fileURL <- paste0(HomeURL, URL1)
for(i in 1:length(URL1)){
    download.file(fileURL[i], destfile = paste0("./",URL1[i]) )
}