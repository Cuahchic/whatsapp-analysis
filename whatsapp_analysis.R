library(tm)
library(stringr)
library(wordcloud)
library(plotly)
library(stringi)
library(memisc)
library(lubridate)
library(syuzhet)
library(scales)
library(reshape2)
library(dplyr)
library(gplots)
library(reshape)


# Read raw txt file for manipulation
msgs <- read.table(file = "data/francis-family.txt", quote="", sep="\n", stringsAsFactors = FALSE)
colnames(msgs) <- c("origmsg")


# Function to clean up messages
msg.cleanup <- function(msg) {
  outputmsg <- gsub(pattern = "[^\x01-\x7F]", replacement = "", msg)  # Remove emojies
  outputmsg <- gsub(pattern = "[[:punct:]]+", replacement = "", x = outputmsg)  # Remove punctuation
  outputmsg <- trimws(outputmsg)  # Remove remaining whitespace
  
  return (outputmsg)
}


# Concatenate multi-line messages
msgs$timestamp <- ""
msgs$sender <- ""
msgs$msg <- ""
msgs$ismedia <- 0
msgs$numemojies <- 0
msgs$ishyperlink <- 0
msgs$markfordelete <- 0

prevmsg <- ""

for (i in nrow(msgs):1) {
  msg <- paste(msgs[i,"origmsg"], prevmsg, sep = " ")
  
  timestamp <- unlist(regmatches(msg, gregexpr(pattern = "^(0?[1-9]|[12][0-9]|3[01])[\\/\\-](0?[1-9]|1[012])[\\/\\-]\\d{4}, \\d{2}:\\d{2}", text = msg)))
  
  if (length(timestamp) > 0) {  # Message contains timestamp so is start of multi- or single-line message
    prevmsg <- ""
    msg <- gsub(pattern = timestamp, replacement = "", x = msg)
    colonLoc <- unlist(gregexpr(pattern = ":", text = msg))[1]
    
    if (colonLoc >= 0) { # Other than timestamp message contains colon therefore a username
      msgs$timestamp[i] <- timestamp
      msgs$sender[i] <- trimws(gsub(pattern = "[-:]", replacement = "", substr(x = msg, start = 1, stop = colonLoc)))
      
      isMedia <- 0
      numEmojies <- 0
      isHyperlink <- 0
      if (unlist(gregexpr(pattern = "<Media omitted>", text = msg))[1] >= 0) {
        isMedia <- 1
        msg <- ""
      } else if (unlist(gregexpr(pattern = "https?:\\/\\/[\\w\\d][\\w\\d\\-]*(\\.[\\w\\d\\-]+)*\\.[\\w]+(\\/[\\w\\-\\_\\(\\)\\.\\?\\=\\&]*)*", text = msg, perl = TRUE))[1] >= 0) {
        isHyperlink <- 1
      }
      
      numEmojies <- length(which(unlist(gregexpr(pattern = "ðY", text = msg)) >= 0))
      
      msgs$msg[i] <- msg.cleanup(substr(x = msg, start = (colonLoc + 1), stop = nchar(msg)))
      msgs$ismedia[i] <- isMedia
      msgs$numemojies[i] <- numEmojies
      msgs$ishyperlink[i] <- isHyperlink
      
    } else {  # Not a real message, e.g. information line
      msgs$markfordelete[i] <- 1
      next
    }
  } else {  # Line doesn't contain a timestamp and therefore add it next time
    msgs$markfordelete[i] <- 1
    prevmsg <- msg
  }
}

#View(msgs[msgs$markfordelete == 1,])
# Remove any lines marked for delete and remove this column
msgs <- msgs[msgs$markfordelete != 1,]
msgs$markfordelete <- NULL


# Create function for similarity comparison
fun.cosinesimilarity <- function(df) {
  df_copy <- df / colSums(df)
  
  sumsq <- colSums(df_copy^2)
  
  df_out <- data.frame(matrix(NA, nrow=length(sumsq), ncol=length(sumsq)))
  colnames(df_out) <- colnames(df_copy)
  rownames(df_out) <- colnames(df_copy)
  
  for (n1 in colnames(df_copy))
  {
    for (n2 in colnames(df_copy))
    {
      df_out[n2, n1] <- sum(df_copy[,n1] * df_copy[,n2]) / (sqrt(sumsq[n1]) * sqrt(sumsq[n2]))
    }
  }
  
  return (df_out)
}


# Convert timestamp to date format (do it once for ones with hours and once for those which only have a date as they were sent at midnight)
msgs$origtimestamp <- msgs$timestamp
msgs$timestamp <- as.POSIXct(msgs$origtimestamp, format="%d/%m/%Y, %H:%M")
msgs$timestamp[is.na(msgs$timestamp)] <- as.POSIXct(msgs$origtimestamp[is.na(msgs$timestamp)], format="%d/%m/%Y")
msgs$day <- format(msgs$timestamp,format='%d/%m/%Y')



# Get sender short names for consistent plotting (does not consider if more than one person in group has the same first name)
fun.longtoshortname <- function(longnames) {
  # Get unique entries of names
  uniqueNames <- unique(longnames)
  
  firstNames <- unlist(regmatches(uniqueNames, gregexpr(pattern = "^[a-zA-Z]+", text = uniqueNames)))
  
  namesList <- as.list(firstNames)
  names(namesList) <- uniqueNames
  
  return (as.character(namesList[longnames]))
}

msgs$sendershortname <- fun.longtoshortname(msgs$sender)



# Get message type for plotting
msgs$msgtype <- cases(
  "media" <- msgs$ismedia==1,
  "hyperlink" <- msgs$ishyperlink==1,
  "text" <- msgs$ismedia==0 & msgs$ishyperlink==0
)


# Plot messages sent by day
msgsbyday <- data.frame(table(msgs$day))
colnames(msgsbyday) <- c("day", "msgssent")
msgsbyday$day <- as.POSIXct(msgsbyday$day, format="%d/%m/%Y")
msgsbyday <- msgsbyday[order(msgsbyday$day),]

line_msgs_xaxis <- list(title = "")
line_msgs_yaxis <- list(title = "Messages Sent Per Day")

plot_ly(x=msgsbyday$day, y=msgsbyday$msgssent, type="scatter", mode = "lines") %>% layout(title = "Daily Messages Sent", xaxis = line_msgs_xaxis, yaxis = line_msgs_yaxis)



# Histogram who sent most messages by message type
senderfreq <- as.data.frame(table(msgs$sendershortname, msgs$msgtype))         # Create sender frequency table
colnames(senderfreq) <- c("sendershortname", "msgtype", "msgssent")
senderfreq$propmsgssent <- 100 * senderfreq$msgssent / sum(senderfreq$msgssent)

hist_msgs_xaxis <- list(title = "", categoryorder = "array", categoryarray = names(sort(table(msgs$sendershortname), decreasing=TRUE)))
hist_msgs_yaxis <- list(title = "Messages Sent (% Of Total)")

plot_ly(x=senderfreq$sendershortname, y=senderfreq$propmsgssent, color=senderfreq$msgtype, type="bar") %>% layout(title = paste("Messages By Sender (", nrow(msgs), " messages)", sep = ""), xaxis = hist_msgs_xaxis, yaxis = hist_msgs_yaxis, barmode="stack")



# Start preparing for and plot word cloud
wordCorpus <- Corpus(VectorSource(msgs$msg))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, c("get", "can", "will", "got", "thats" ,stopwords("english")))  # Remove standard stopwords and some more than were appearing a lot
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")  # See http://www.datavis.ca/sasmac/brewerpal.html for colours list
pal <- pal[-(1:4)]             # Removes lightest colours to make it easier to read
set.seed(123)
wordcloud(words = wordCorpus, scale=c(3,0.5), max.words=20, random.order=TRUE, rot.per=0.35, use.r.layout=FALSE, colors=pal)   # See ?wordcloud for more info on settings



# Plot word cloud for a given person
filter.sendershortname <- "Colin"

wordCorpus.filter <- Corpus(VectorSource(msgs[msgs$sendershortname==filter.sendershortname, "msg"]))
wordCorpus.filter <- tm_map(wordCorpus.filter, removePunctuation)
wordCorpus.filter <- tm_map(wordCorpus.filter, content_transformer(tolower))
wordCorpus.filter <- tm_map(wordCorpus.filter, removeWords, c("get", "can", "will", "got", "thats", stopwords("english")))  # Remove standard stopwords and some more than were appearing a lot
wordCorpus.filter <- tm_map(wordCorpus.filter, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")  # See http://www.datavis.ca/sasmac/brewerpal.html for colours list
pal <- pal[-(1:4)]             # Removes lightest colours to make it easier to read
set.seed(123)
wordcloud(words = wordCorpus.filter, scale=c(3,0.5), max.words=20, random.order=TRUE, rot.per=0.35, use.r.layout=FALSE, colors=pal)



# Plot comparison cloud for all group members
namelist <- levels(factor(msgs$sendershortname))
numnames <- length(namelist)        # Get number of names
names.doc <- rep("", numnames)                                  # Create empty vector of same length as names to store all words used by each person
for (i in 1:numnames)
{
  names.doc[i] <- paste(msgs[msgs$sendershortname==namelist[i],"msg"], collapse=" ")  # This creates one document per group member that we can generate a word cloud for
}

wordCorpus.compcloud.filter <- Corpus(VectorSource(names.doc))
wordCorpus.compcloud.filter <- tm_map(wordCorpus.compcloud.filter, removePunctuation)
wordCorpus.compcloud.filter <- tm_map(wordCorpus.compcloud.filter, content_transformer(tolower))
wordCorpus.compcloud.filter <- tm_map(wordCorpus.compcloud.filter, removeWords, c("get", "can", "will", "got", "thats", stopwords("english")))  # Remove standard stopwords and some more than were appearing a lot
wordCorpus.compcloud.filter <- tm_map(wordCorpus.compcloud.filter, stripWhitespace)
termDocMat.compcloud <- TermDocumentMatrix(wordCorpus.compcloud.filter)
termDocMat.compcloud <- as.matrix(termDocMat.compcloud)
colnames(termDocMat.compcloud) <- namelist

comparison.cloud(termDocMat.compcloud, scale=c(3,0.8), random.order=FALSE, title.size=2, colors=brewer.pal(numnames+1,"Set3")[-2], max.words=600)
commonality.cloud(termDocMat.compcloud, scale=c(6,0.2), random.order=TRUE, rot.per=0.35, use.r.layout=FALSE, colors=pal)



# Plot frequencies of words
termDocMat <- TermDocumentMatrix(wordCorpus)
termDocMat <- as.matrix(termDocMat)



# Analyse message sentiment
msgsentiment <- get_nrc_sentiment(msgs$msg)
summsgsentiment <- colSums(msgsentiment)
summsgsentiment <- summsgsentiment[!(names(summsgsentiment) %in% c("negative", "positive"))]    # Remove positive and negative as they are separate counts
summsgsentiment <- as.data.frame(summsgsentiment)
colnames(summsgsentiment) <- c("freq")         # Rename column
summsgsentiment$propfreq <- 100 * summsgsentiment$freq / sum(summsgsentiment$freq)    # Get proportional freq
summsgsentiment$sentiment <- rownames(summsgsentiment)

hist_sentiment_xaxis <- list(title = "")
hist_sentiment_yaxis <- list(title = "Proportion of Messages (%)")

plot_ly(x=summsgsentiment[order(summsgsentiment$propfreq, decreasing=TRUE),"sentiment"], y=summsgsentiment[order(summsgsentiment$propfreq, decreasing=TRUE),"propfreq"], type="bar") %>% layout(title = paste("Message Sentiment", sep = ""), xaxis = hist_sentiment_xaxis, yaxis = hist_sentiment_yaxis)



# Append message sentiment to original data so we can plot sentiment per person
msgs <- cbind(msgs, msgsentiment[,!(names(msgsentiment) %in% c("negative", "positive"))])

msgsentimentpp <- aggregate(cbind(anger, anticipation, disgust, fear, joy, sadness, surprise, trust) ~ sendershortname, msgs[msgs$msgtype=="text",], sum) %>% melt
colnames(msgsentimentpp) <- c("sendershortname", "sentiment", "freqsentiment")         # Give better column names
msgsentimentpp <- merge(x=msgsentimentpp, y=aggregate(msgssent ~ sendershortname, senderfreq, sum), all.x = TRUE)                   # Attach total messages sent so we can calculate proportion
msgsentimentpp$propsentiment <- 100 * msgsentimentpp$freqsentiment / msgsentimentpp$msgssent    # Proportion of messages sent by sentiment (remember that multiple sentiments can exist per message)

hist_sentimentpp_xaxis <- list(title = "")
hist_sentimentpp_yaxis <- list(title = "Proportion Of Messages With Emotion Per Person (%)")

filter.sentiment <- "trust"
#anger, anticipation, disgust, fear, joy, sadness, surprise, trust

plot_ly(x=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "sendershortname"], y=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "propsentiment"], color=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "sentiment"], type="bar", colors=sample(colours(), 1)) %>% layout(title = paste("Sentiment Of ", filter.sentiment, " Per Person", sep = ""), xaxis = hist_sentimentpp_xaxis, yaxis = hist_sentimentpp_yaxis)

plot_ly(x=msgsentimentpp$sendershortname, y=msgsentimentpp$propsentiment, color=msgsentimentpp$sentiment, type="bar") %>% layout(title = paste("Sentiment Per Person", sep = ""), xaxis = hist_sentimentpp_xaxis, yaxis = hist_sentimentpp_yaxis)


# Quickest responders
binsize <- 30
maxresponseduration <- 1000
msg.response <- msgs[,c("timestamp", "sendershortname")] %>% mutate(replier=lead(sendershortname)) %>% mutate(responsetime=difftime(lead(timestamp), timestamp, units=c("secs")))
msg.response$bin <- cut(as.numeric(msg.response$responsetime), breaks=c(seq(0, min(c(maxresponseduration, max(as.numeric(msg.response$responsetime), na.rm=T))), by=binsize)), labels=F, include.lowest=T)

msg.response.freq <- with(msg.response, as.data.frame(table(bin, replier)))
colnames(msg.response.freq) <- c("bin", "replier", "freq")
msg.response.freq$x <- (as.numeric(levels(msg.response.freq$bin))[msg.response.freq$bin] - 1) * binsize
msg.response.freq <- msg.response.freq %>% group_by(replier) %>% mutate(totalreplies=sum(freq)) %>% data.frame
msg.response.freq$freqnorm <- 100 * msg.response.freq$freq / msg.response.freq$totalreplies

response_xaxis <- list(title = "Response Time (sec)")
response_yaxis <- list(title = "Proportion of Replies (%)")

plot_ly(x=msg.response.freq$x, y=msg.response.freq$freqnorm, color=msg.response.freq$replier, type="scatter", mode="line") %>% layout(title = "Response Time Per Person", xaxis = response_xaxis, yaxis = response_yaxis)

sum(msg.response.freq$freq[msg.response.freq$bin==1]) / sum(msg.response.freq$freq) # 79% of messages replied to within 1 minute



# Most similar vocabularies
tdm.cossim <- fun.cosinesimilarity(data.frame(termDocMat.compcloud))

filter.sendershortname <- "Colin"

y.dat <- sort(tdm.cossim[,c(filter.sendershortname)], decreasing=T)
x.dat <- colnames(tdm.cossim[order(tdm.cossim[,filter.sendershortname], decreasing = T)])

hist_similarity_xaxis <- list(title = "")
hist_similarity_yaxis <- list(title = "Cosine Similarity")

plot_ly(x=x.dat, y=y.dat, type="bar", color=x.dat[3], colors=sample(colours(), 1)) %>% layout(showlegend=F, title = paste("Who talks like", filter.sendershortname, "?", sep = " "), xaxis = hist_similarity_xaxis, yaxis = hist_similarity_yaxis)

legend_settings <- list(tickformat = "%.1f")
plot_ly(z = data.matrix(tdm.cossim), x = names(tdm.cossim), y = names(tdm.cossim), type = "heatmap", colors = colorRamp(c("red", "green"))) %>% layout(title = "Message Similarity Matrix", legend = legend_settings)


