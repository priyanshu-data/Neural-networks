##installing the libraries




##loading the data in Rstudio
company<-read.csv(choose.files())
summary(company)
str(company)
company$State <- as.numeric(revalue(company$State,c("New York"="0", "California"="1","Florida"="2")))####as neural network works best with numbers system,so need to convert state into numeric form
View(company)

