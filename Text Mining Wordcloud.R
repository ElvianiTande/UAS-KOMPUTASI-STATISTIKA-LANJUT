#Input Library dan Dataset
library(wordcloud)
library(tm)
library(textclean)
library(tidytext)

reviews <- read.csv("C:/Users/LENOVO/Pictures/reviews.csv")
head(reviews)

#merubah file .csv tadi kedalam corpus
corpusdata <- Corpus(VectorSource(reviews$Review))
inspect(corpusdata[1:5])

#mengubah semua huruf kapital menjadi huruf kecil
data_casefolding <- tm_map(corpusdata,
                           content_transformer(tolower))

#menghapus url pada dokumen
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
dataURL <- tm_map(data_casefolding,
                  content_transformer(removeURL))

#menghapus mention pada dokumen
remove.mention <- function(x) gsub("Q\\S+","",x)
data_mention <- tm_map(dataURL, remove.mention)

#menghapus hashtag
remove.hashtag <- function(X) gsub ("#\\S+","", X)
data_hashtag <- tm_map(data_mention, remove.hashtag)

#menghapus tanda baca
data_punctuation <- tm_map(data_hashtag, content_transformer(removePunctuation))

#menghapus nomor
data_nonumber <- tm_map(data_punctuation,
                        content_transformer(removeNumbers))

#stemmming atau menghapus imbuhan sehingga kata menjadi kata dasar
data_stemming <- tm_map(data_nonumber, stemDocument,
                        language = "english")

#simpan stemming
databersih <- data.frame(text=unlist(sapply(data_stemming, "[")), stringsAsFactors = FALSE)

#Menghapus Huruf yang Bukan Kata
databersih <- strip(databersih)
head(databersih)

#Menghapus Kata yang tidak penting
databersih <-removeWords(databersih,c("a","i","at","my","is","it","of","so","to","be","or","in" ,"the","our","you","and","was","this","with","not","too","but","for","its","who","has","what","yet","your","out","else","more","own","are","abaut","just","because","all","etc","works","these","did","say","use","end","she","lot","can","not","for","saw","also","have","that","after","and","dont","than","cant",","))
head(databersih)

##Membuat Word Cloud
#Mengubah Data Frame Menjadi Data Faktor
tdm <- TermDocumentMatrix(databersih)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
#Mengubah Data Faktor Menjadi Data Frame
d <- data.frame(word = names(v), freq = v)

wordcloud(d$word, d$freq,
          random.order = FALSE,
          max.words = 200,
          colors = brewer.pal(name = "Dark2",8 )) 

