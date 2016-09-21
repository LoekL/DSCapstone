library(ggplot2)
library(tm)
library(slam)
library(sqldf)

twitter <- readLines("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/Data/final/en_US/en_US.twitter.txt", encoding = "utf-8", skipNul = TRUE)
blogs <- readLines("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/Data/final/en_US/en_US.blogs.txt", encoding = "utf-8", skipNul = TRUE)
news <- readLines("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/Data/final/en_US/en_US.news.txt", encoding = "utf-8", skipNul = TRUE)

# Generating smaller samples for future processing
n = 200000

twitterSample <- twitter[sample(1:length(twitter), n)]
blogsSample <- blogs[sample(1:length(blogs), n)]
newsSample <- news[sample(1:length(news), n)]

sampleData <- c(twitterSample, blogsSample, newsSample)

# sampleData <- c(twitter, blogs, news)

rm(twitter)
rm(blogs)
rm(news)
rm(twitterSample)
rm(blogsSample)
rm(newsSample)
rm(n)

## Profanity Filtering

profanityEnglish <- c('anal','anus','arse','ass','ballsack','balls','bastard','bitch','biatch','bloody','blowjob','blowjob','bollock','bollok','boner','boob','bugger','bum','butt','buttplug','clitoris','cock','coon','crap','cunt','damn','dick','dildo','dyke','fag','feck','fellate','fellatio','felching','fuck','f u c k','fudgepacker','fudgepacker','flange','Goddamn','Goddamn','hell','homo','jerk','jizz','knobend','labia','lmao','lmfao','muff','nigger','nigga','omg','penis','piss','poop','prick','pube','pussy','queer','scrotum','sex','shit','shit','sh1t','slut','smegma','spunk','tit','tosser','turd','twat','vagina','wank','whore','wtf')
# stopWordsEnglish <- c('a','able','about','across','after','all','almost','also','am','among','an','and','any','are','as','at','be','because','been','but','by','can','cannot','could','dear','did','do','does','either','else','ever','every','for','from','get','got','had','has','have','he','her','hers','him','his','how','however','i','if','in','into','is','it','its','just','least','let','like','likely','may','me','might','most','must','my','neither','no','nor','not','of','off','often','on','only','or','other','our','own','rather','said','say','says','she','should','since','so','some','than','that','the','their','them','then','there','these','they','this','tis','to','too','twas','us','wants','was','we','were','what','when','where','which','while','who','whom','why','will','with','would','yet','you','your')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

filter <- function(sampleText, filterWords){
  filteredDoc <- vector(length = length(sampleText))
  j = 1
  for (i in sampleText) {
    words <- unlist(strsplit(i, split = " "))
    filteredWords <- gsub(pattern = paste(paste(paste0('^', filterWords, '$'), collapse = "|"), collapse = "|"), x = words, replacement = "")
    filteredText <- paste(as.vector(filteredWords[filteredWords != ""]), collapse = " ")
    filteredDoc[j] <- trim(filteredText)
    j = j + 1
  }
  return(filteredDoc)
}

sampleDataClean <- filter(sampleData, profanityEnglish)
head(sampleDataClean)
# sampleDataClean <- filter(sampleDataClean, stopWordsEnglish)
# head(sampleDataClean)
rm(sampleData)
rm(profanityEnglish)

## Word Frequencies

#### 1-Grams

y <- gsub('[^a-zA-Z]+', '', tolower(unlist(strsplit(sampleDataClean, split = " "))))
y <- y[y != ""]
wordDist <- table(y)
dfUni <- as.data.frame(sort(wordDist, decreasing = TRUE))
names(dfUni)[1] <- 'ngram'
dfUni <- dfUni[(dfUni$Freq >= 4),] # removing/pruning low frequency ngrams
dfUni$order <- 1
rm(wordDist)

setwd("/Users/loeklandzaat/Desktop")
write.csv(dfUni, "dfUni600k.csv") 

#### Using tm to manage the corpora

getCorpus <- function(v) {
  corpus <- VCorpus(VectorSource(v))
  corpus <- tm_map(corpus, stripWhitespace)  # remove whitespace
  corpus <- tm_map(corpus, content_transformer(tolower))  # lowercase all
  corpus <- tm_map(corpus, removePunctuation) # remove all punctuation
  corpus <- tm_map(corpus, removeNumbers) # remove all numbers
  # corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
  corpus 
}

x <- getCorpus(sampleDataClean)

#### 2-Grams

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

tdmBi <- TermDocumentMatrix(x, control = list(tokenize = BigramTokenizer))

biGramDist <- row_sums(tdmBi, na.rm = T) # col_sums(tdm, na.rm = T)
dfBi <- as.data.frame(sort(biGramDist, decreasing = TRUE))
names(dfBi)[1] <- 'Freq'
dfBi$ngram <- rownames(dfBi)
rownames(dfBi) <- NULL
dfBi <- dfBi[c(2,1)]
dfBi <- dfBi[(dfBi$Freq >= 4),] # removing/pruning low frequency ngrams
dfBi$order <- 2
rm(biGramDist)
rm(tdmBi)

write.csv(dfBi, "dfBi600k.csv")

#### 3-Grams

TrigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

tdmTri <- TermDocumentMatrix(x, control = list(tokenize = TrigramTokenizer))

triGramDist <- row_sums(tdmTri, na.rm = T) # col_sums(tdm, na.rm = T)
dfTri <- as.data.frame(sort(triGramDist, decreasing = TRUE))
names(dfTri)[1] <- 'Freq'
dfTri$ngram <- rownames(dfTri)
rownames(dfTri) <- NULL
dfTri <- dfTri[c(2,1)]
dfTri <- dfTri[(dfTri$Freq >= 4),] # removing/pruning low frequency ngrams
dfTri$order <- 3
rm(triGramDist)
rm(tdmTri)

write.csv(dfTri, "dfTri600k.csv")

#### 4-Grams

fourGramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

tdmFour <- TermDocumentMatrix(x, control = list(tokenize = fourGramTokenizer))

fourGramDist <- row_sums(tdmFour, na.rm = T) # col_sums(tdm, na.rm = T)
dfFour <- as.data.frame(sort(fourGramDist, decreasing = TRUE))
names(dfFour)[1] <- 'Freq'
dfFour$ngram <- rownames(dfFour)
rownames(dfFour) <- NULL
dfFour <- dfFour[c(2,1)]
dfFour <- dfFour[(dfFour$Freq >= 4),] # removing/pruning low frequency ngrams
dfFour$order <- 4
rm(fourGramDist)
rm(tdmFour)

write.csv(dfFour, "dfFour600k.csv")

#### 5-Grams

fiveGramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)

tdmFive <- TermDocumentMatrix(x, control = list(tokenize = fiveGramTokenizer))

fiveGramDist <- row_sums(tdmFive, na.rm = T) # col_sums(tdm, na.rm = T)
dfFive <- as.data.frame(sort(fiveGramDist, decreasing = TRUE))
names(dfFive)[1] <- 'Freq'
dfFive$ngram <- rownames(dfFive)
rownames(dfFive) <- NULL
dfFive <- dfFive[c(2,1)]
dfFive <- dfFive[(dfFive$Freq >= 4),] # removing/pruning low frequency ngrams
dfFive$order <- 5 
rm(fiveGramDist)
rm(tdmFive)

write.csv(dfFive, "dfFive600k.csv")

setwd("/Users/loeklandzaat/Desktop")
dfUni <- read.csv("dfUni600k.csv")
dfUni <- dfUni[, 2:4]
dfBi <- read.csv("dfBi600k.csv")
dfBi <- dfBi[, 2:4]
dfTri <- read.csv("dfTri600k.csv")
dfTri <- dfTri[, 2:4]
dfFour <- read.csv("dfFour600k.csv")
dfFour <- dfFour[, 2:4]
dfFive <- read.csv("dfFive600k.csv")
dfFive <- dfFive[, 2:4]

sentence <- "hello how"
sentence <- gsub('[^a-zA-Z ]', '', tolower(sentence))
sentence <- filter(sentence, profanityEnglish)
# sentence <- filter(sentence, stopWordsEnglish)
wordVector <- unlist(strsplit(sentence, " "))
size <- length(wordVector)

if (size >= 4) {
  start <- size - 3
  stop <- size
  fourGram <- paste0(wordVector[start:stop], collapse = " ")
  threeGram <- paste0(wordVector[(start+1):stop], collapse = " ")
  twoGram <- paste0(wordVector[(start+2):stop], collapse = " ")
  oneGram <- paste0(wordVector[(start+3):stop], collapse = " ")
} else if (size == 3) {
  threeGram <- sentence
  twoGram <- paste0(wordVector[2:size], collapse = " ")
  oneGram <- paste0(wordVector[3:size], collapse = " ")
} else if (size == 2) {
  twoGram <- sentence
  oneGram <- paste0(wordVector[2:size], collapse = " ")
} else {
  oneGram <- sentence
}

# fourGram
# threeGram
# twoGram
# oneGram

# rm(fourGram)
# rm(threeGram)
# rm(twoGram)
# rm(oneGram)
# df <- df[order(-df$score),]
# df <- rbind(
#   dfFive[grepl(regexFourGram, as.vector(dfFive[,1])),],
#   dfFour[grepl(regexTriGram, as.vector(dfFour[,1])),],
#   dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),],
#   dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),],
#   dfUni[grepl(regexFinal, as.vector(dfUni[,1])),],
#   dfUni[1:5,]
# )

if (size >= 4) {
  regexFourGram <- paste0('^', fourGram, ' .*')
  regexTriGram <- paste0('^', threeGram, ' .*')
  regexBiGram <- paste0('^', twoGram, ' .*')
  regexUniGram <- paste0('^', oneGram, ' .*')
  regexFinal <- paste(paste0('^', tail(wordVector, 5), '$'), collapse = "|")
  subGram <- paste0()
  df5 <- dfFive[grepl(regexFourGram, as.vector(dfFive[,1])),]
  df5 <- head(df5[order(-df5$Freq),], 10)
  df4 <- dfFour[grepl(regexTriGram, as.vector(dfFour[,1])),]
  df4Sub <- df4[order(-df4$Freq),]
  df4 <- rbind(head(df4Sub, 10), dfFour[(dfFour$ngram == fourGram),])
  df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
  df3Sub <- df3[order(-df3$Freq),]
  df3 <- rbind(head(df3Sub, 10), dfTri[(dfTri$ngram == threeGram),])
  df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
  df2Sub <- df2[order(-df2$Freq),]
  df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
  df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
  df1 <- df1[order(-df1$Freq),]
  df0 <- dfUni[1:5,]
  df <- rbind(df5, df4, df3, df2, df1, df0)
} else if (size == 3) {
  regexTriGram <- paste0('^', threeGram, '.*')
  regexBiGram <- paste0('^', twoGram, '.*')
  regexUniGram <- paste0('^', oneGram, '.*')
  regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
  df4 <- dfFour[grepl(regexTriGram, as.vector(dfFour[,1])),]
  df4 <- head(df4[order(-df4$Freq),], 10)
  df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
  df3Sub <- df3[order(-df3$Freq),]
  df3 <- rbind(head(df3Sub, 10), dfTri[(dfTri$ngram == threeGram),])
  df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
  df2Sub <- df2[order(-df2$Freq),]
  df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
  df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
  df1 <- df1[order(-df1$Freq),]
  df0 <- dfUni[1:5,]
  df <- rbind(df4, df3, df2, df1, df0)
} else if (size == 2) {
  regexBiGram <- paste0('^', twoGram, '.*')
  regexUniGram <- paste0('^', oneGram, '.*')
  regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
  df3 <- dfTri[grepl(regexBiGram, as.vector(dfTri[,1])),]
  df3 <- head(df3[order(-df3$Freq),], 10)
  df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
  df2Sub <- df2[order(-df2$Freq),]
  df2 <- rbind(head(df2Sub, 10), dfBi[(dfBi$ngram == twoGram),])
  df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
  df1 <- df1[order(-df1$Freq),]
  df0 <- dfUni[1:5,]
  df <- rbind(df3, df2, df1, df0)
} else {
  regexUniGram <- paste0('^', oneGram, '.*')
  regexFinal <- paste(paste0('^', wordVector, '$'), collapse = "|")
  df2 <- dfBi[grepl(regexUniGram, as.vector(dfBi[,1])),]
  df2 <- head(df2[order(-df2$Freq),], 10)
  df1 <- dfUni[grepl(regexFinal, as.vector(dfUni[,1])),]
  df1 <- df1[order(-df1$Freq),]
  df0 <- dfUni[1:5,]
  df <- rbind(df2, df1, df0)
}

df[,1] <- as.character(df[,1])

for (i in 1:dim(df)[1]) {
  df$recommendation[i] <- tail(unlist(strsplit(df[i,1], " ")), 1)
  df$subgram[i] <- paste(unlist(strsplit(df[i,1], " "))[1:(df[i,3] - 1)], collapse = " ")
}

df <- sqldf("
SELECT DISTINCT 
  a.*,
  b.Freq AS subgramFreq
FROM df a
LEFT JOIN df b
ON a.subgram = b.ngram
WHERE subgramFreq IS NOT NULL
")

lambda <- 0.4
df$score <- (lambda^(5 - df$order)) * (df$Freq / df$subgramFreq)
df[(df$order == 1), 'score'] <- 0
df <- df[order(-df$score),]
df <- df[!grepl(regexFinal, as.vector(df[,1])),]
df <- df[!grepl(regexFinal, as.vector(df[,4])),]
topFive <- head(unique(df$recommendation), 5)
topFive

object.size(dfUni)[1] / 1024 / 1024 # MB Size
object.size(dfBi)[1] / 1024 / 1024 # MB Size
object.size(dfTri)[1] / 1024 / 1024 # MB Size
object.size(dfFour)[1] / 1024 / 1024 # MB Size
object.size(dfFive)[1] / 1024 / 1024 # MB Size

# rm(BigramTokenizer)
# rm(fourGramTokenizer)
# rm(fiveGramTokenizer)
# rm(filter)
# rm(getCorpus)
# rm(trim)

## Creative Exploration -- Supplementing Model with External Data?
# - Lacking frequency list of single words! Extract from Bigram?

ext5Gram <- read.csv("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/external_ngram_data/5GramExt.csv", header=FALSE)
# object.size(ext5Gram)[1] / 1024 / 1024 # MB Size
# head(ext5Gram)
ext5Gram$ngram <- paste(ext5Gram$V2, ext5Gram$V3, ext5Gram$V4, ext5Gram$V5 ,ext5Gram$V6, sep = " ")
ext5Gram <- ext5Gram[,c(7,1)]
colnames(ext5Gram)[2] <- "Freq"
ext5Gram$order <- 5
ext5Gram$ngram <- gsub('[^a-zA-Z| ]+', '', ext5Gram$ngram) # remove punctuation

ext4Gram <- read.csv("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/external_ngram_data/4GramExt.csv", header=FALSE)
ext4Gram$ngram <- paste(ext4Gram$V2, ext4Gram$V3, ext4Gram$V4, ext4Gram$V5, sep = " ")
ext4Gram <- ext4Gram[,c(6,1)]
colnames(ext4Gram)[2] <- "Freq"
ext4Gram$order <- 4
ext4Gram$ngram <- gsub('[^a-zA-Z| ]+', '', ext4Gram$ngram) # remove punctuation

ext3Gram <- read.csv("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/external_ngram_data/3GramExt.csv", header=FALSE)
ext3Gram$ngram <- paste(ext3Gram$V2, ext3Gram$V3, ext3Gram$V4, sep = " ")
ext3Gram <- ext3Gram[,c(5,1)]
colnames(ext3Gram)[2] <- "Freq"
ext3Gram$order <- 3
ext3Gram$ngram <- gsub('[^a-zA-Z| ]+', '', ext3Gram$ngram) # remove punctuation

ext2Gram <- read.csv("/Users/loeklandzaat/Dropbox (Personal)/1 - Coursera/R/10 - Capstone/external_ngram_data/2GramExt.csv", header=FALSE)
ext2Gram$ngram <- paste(ext2Gram$V2, ext2Gram$V3, sep = " ")
ext2Gram <- ext2Gram[,c(4,1)]
colnames(ext2Gram)[2] <- "Freq"
ext2Gram$order <- 2
ext2Gram$ngram <- gsub('[^a-zA-Z| ]+', '', ext2Gram$ngram) # remove punctuation

dfBi <- rbind(dfBi, ext2Gram)
dfBi <- aggregate(dfBi['Freq'], by=dfBi['ngram'], sum) # dedupe via aggregation
dfBi$order <- 2
dfTri <- rbind(dfTri, ext3Gram)
dfTri <- aggregate(dfTri['Freq'], by=dfTri['ngram'], sum) # dedupe via aggregation
dfTri$order <- 3
dfFour <- rbind(dfFour, ext4Gram)
dfFour <- aggregate(dfFour['Freq'], by=dfFour['ngram'], sum) # dedupe via aggregation
dfFour$order <- 4
dfFive <- rbind(dfFive, ext5Gram)
dfFive <- aggregate(dfFive['Freq'], by=dfFive['ngram'], sum) # dedupe via aggregation
dfFive$order <- 5

dim(dfBi[(dfBi$Freq <= 30),])[1]
dim(dfTri[(dfTri$Freq <= 30),])[1]
dim(dfFour[(dfFour$Freq <= 15),])[1]
dim(dfFive[(dfFive$Freq <= 6),])[1]

# Pruning

dfBi <- dfBi[(dfBi$Freq > 30),]
dfTri <- dfTri[(dfTri$Freq > 30),]
dfFour <- dfFour[(dfFour$Freq > 15),]
dfFive <- dfFive[(dfFive$Freq > 6),]