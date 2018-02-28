### Script PTA-Umgang ###

# 1. Load Data ####
pta <- readRDS("text_ptas2017-07-20.rds")


# 2. Wordcount ####
library(stringr)
readtext.wc <- function(x) {
  wordcount <- NULL
  for (i in 1:length(x$text)){
    words <- str_extract_all(x$text[i], boundary("word"))
    words <- length(unlist(words))
    wordcount <- append(wordcount, words)
  }
  return(wordcount)
}


wc <- readtext.wc(pta)
id <- as.numeric(gsub(".pdf","", pta$doc_id))

dfm <- data.frame(id = id,
                  wc = wc)
dfm <- dfm[order(dfm$id), ]
row.names(dfm) <- NULL


## Durchsuchen ##
library(stringr)
length(grep("trade", pta$text[1]))

monetary <- grepl("monetary", pta$text, ignore.case = T)

grepl("monetary", pta$text[1:4], ignore.case = T)

library(quanteda)
tkspta <- tokens(pta$text, what = "word", remove_punct = T)
tksptalow <- tokens_tolower(tkspta)
tksptalow <- tokens_select(tksptalow, stopwords(), selection = "remove")
tksptalow <- tokens_wordstem(tksptalow, language = "english") #run=running

tksptalow.dfm <- dfm(tksptalow, tolower = F)
tksptalow.dfm[1:10, 1:20] %>% as.matrix() %>% utils::View()
#tksptalow.dfm %>% as.matrix() %>% dim() #brings the pc down!

# Graphics ####
i <- seq(1:length(pta$text))
countpta <- data.frame(id = i, xrat = i, cur = i, mon = i, cb = i)
for (i in 1:length(pta$text)){
  #x <- qdap::word_count(pta$text[i])
  x <- 1
  countpta$id[i] <- pta$docvar1[i]
  countpta$xrat[i] <- stringr::str_count(pta$text[i], "exchange rate") / x
  countpta$cur[i] <- stringr::str_count(pta$text[i], "currency") / x
  countpta$mon[i] <- stringr::str_count(pta$text[i], "monetary measures")
  countpta$cb[i] <- stringr::str_count(pta$text[i], "central bank") / x
  print(i)
}
countpta <- countpta[order(countpta$id),]
row.names(countpta) <- NULL

temppta <- read.csv("DatPta.csv", header = T, 
                    colClasses=c('NULL', NA, rep('NULL', 4),
                                 NA, rep('NULL', 188)))
colnames(temppta)[1] <- "id"
for (i in 1:nrow(temppta)){
  if (isTRUE(temppta$id[i] == temppta$id[i+1])){
    temppta <- temppta[-i,]
  }
}

temp <- merge(temppta, countpta)
temp <- temp[order(temp$year),]
temp <- subset(temp,!is.na(temp$year))

temp2 <- subset(temp, select = -c(id))
temp2 <- reshape2::melt(temp2, id.var="year")
for (i in nrow(temp2):1){
  if (isTRUE(temp2$value[i] == 0)){
    temp2 <- temp2[-i,]
  }
}

#temp2$id <- row.names(temp2) #to check results

for (i in 1:nrow(temp2)){
  if (isTRUE(temp2$value[i] > 1)){
    k <- temp2$value[i] - 1
    for (j in 1:k){
      temp2 <- rbind(temp2, temp2[i,]) 
      }
  }
}  


library(ggplot2)
ggplot(data=temp2, aes(year, fill = variable)) +
  geom_bar(position='dodge') +
  scale_fill_manual(values=c("#EF1D35", "#09B92F", "#FEB01E"),
                      name="Occured Words",
                      labels=c('"exchange rate"','"currency"',
                               '"central bank"')) +
  scale_x_continuous(name = "Year",
                     labels = seq(1990,2016),breaks = seq(1990,2016)) +
  scale_y_continuous(name = "Word Count", breaks = seq(0,275,50)) +
  theme(axis.text.x = element_text(angle=45),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "#F2F2F2"),
        panel.grid.major = element_line("#D8D8D8"))


ggplot(data=temp, aes(year,cur)) +
  geom_col()

################################### other ##################################
# Review Data
colnames(x)
writeLines(x$text[1], "testext1.txt")

# Pipe Example
x <- as.factor(c(2,3,4)) %>% as.character() %>% as.numeric()