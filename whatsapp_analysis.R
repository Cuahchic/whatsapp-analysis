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


# Set WD
setwd("P:/Personal/WhatsApp Analysis")
#setwd("C:/R Output")



# Read in data from CSV and rename columns
msgs <- read.csv("WhatsAppAnalysis - DataPrep.csv", stringsAsFactors = FALSE, header = FALSE)
colnames(msgs) <- c("origmsg", "timestamp", "sender", "msg", "ismedia", "numemojis", "ishyperlink")



# Create function to map long names to short names
fun.longtoshortname <- function(longname) {
  cases(
    "kirk" <- longname=="Kirk Dennehy",
    "colin" <- longname=="Colin Parry",
    "chrisy" <- longname=="Chris Parker",
    "jim" <- longname=="Jamie Chrystal",
    "hagger" <- longname=="Jonathan Hagger",
    "kev" <- longname=="Kevin McCluskey",
    "saul" <- longname=="Saul Milne",
    "stef" <- longname=="Steven Mallinson",
    "broome" <- longname=="Steven Wilson",
    "stu" <- longname=="Stuart Davidson"
  )
}

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
  
  df_out
}



# Get aliases for names so we can analyse these later
namesfor.colin <- c("coco", "colin", "colins", "colon", "pazza", "parry", "parrys")
namesfor.chrisy <- c("chris", "chrisi", "chrissy", "christiphwoar", "christopher", "chrisy", "chrisys", "parker", "parkers", "christiphylis")
namesfor.saul <- c("saul", "sauls", "sauloman", "saulomon", "milne", "milnes")
namesfor.jim <- c("jim", "jimbob", "jims", "chrystal", "jamie", "jamies", "jiiiiiiiiiim", "jimbim", "jimbomb", "jiminy")
namesfor.hagger <- c("hag", "hagg", "hagger", "haggers", "haggerss", "hagroid", "jonathan", "hangler", "hanglers", "hagar", "haggart", "haggerston", "haggertron")
namesfor.kev <- c("kev", "kevs", "kevin", "mckluskey", "mccluskey", "keeeeeeeev")
namesfor.broome <- c("brom", "broome", "broomes", "brooooome", "broooooome", "broooooomeeeeeeeee", "broooooooooome", "wilson")
namesfor.stef <- c("stef", "stefs", "steff", "stefron", "steven", "stevens", "mallinson", "mallinsons", "stevie", "steeeeeeeeeeveeeeeeeen", "steeeeeeeeeeveeeeeeen", "steeeeef")
namesfor.stu <- c("stu", "stuart", "davidson", "stus", "stuarts")
namesfor.kirk <- c("denehey", "dennehy", "dennehys", "derney", "derny", "kiokis", "kiokus", "kirk", "kirkimus", "kirkis", "kirks", "kirky", "kirkby", "kirklands", "kirm", "kirmos", "kiiiiiiiiiiiiiiiiiiiiiiiirm", "kiiiiiiiiiiiiiiirm", "koik", "kuiok")



# Convert timestamp to date format (do it once for ones with hours and once for those which only have a date as they were sent at midnight)
msgs$origtimestamp <- msgs$timestamp
msgs$timestamp <- as.POSIXct(msgs$origtimestamp, format="%d/%m/%Y %H:%M")
msgs$timestamp[is.na(msgs$timestamp)] <- as.POSIXct(msgs$origtimestamp[is.na(msgs$timestamp)], format="%d/%m/%Y")
msgs$day <- format(msgs$timestamp,format='%d/%m/%Y')



# Get sender short names for consistent plotting
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

plot_ly(x=msgsbyday$day, y=msgsbyday$msgssent, type="scatter", mode = "lines") %>% layout(title = "Messages Sent By Day Over Time", xaxis = line_msgs_xaxis, yaxis = line_msgs_yaxis)



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
wordcloud(words = wordCorpus, scale=c(4,0.2), max.words=30, random.order=TRUE, rot.per=0.35, use.r.layout=FALSE, colors=pal)   # See ?wordcloud for more info on settings



# Plot word cloud for a given person
filter.sendershortname <- "kirk"

wordCorpus.filter <- Corpus(VectorSource(msgs[msgs$sendershortname==filter.sendershortname, "msg"]))
wordCorpus.filter <- tm_map(wordCorpus.filter, removePunctuation)
wordCorpus.filter <- tm_map(wordCorpus.filter, content_transformer(tolower))
wordCorpus.filter <- tm_map(wordCorpus.filter, removeWords, c("get", "can", "will", "got", "thats", stopwords("english")))  # Remove standard stopwords and some more than were appearing a lot
wordCorpus.filter <- tm_map(wordCorpus.filter, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")  # See http://www.datavis.ca/sasmac/brewerpal.html for colours list
pal <- pal[-(1:4)]             # Removes lightest colours to make it easier to read
set.seed(123)
wordcloud(words = wordCorpus.filter, scale=c(4,0.2), max.words=30, random.order=TRUE, rot.per=0.35, use.r.layout=FALSE, colors=pal)



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



# See how many times each name has been mentioned
termDocMat.RowSums <- rowSums(termDocMat)
termDocMat.RowSums <- as.data.frame(termDocMat.RowSums)       # Convert to data frame
colnames(termDocMat.RowSums) <- c("wordfreq")                 # Rename column to something better
termDocMat.RowSums$shortname <- factor(rownames(termDocMat.RowSums))
levels(termDocMat.RowSums$shortname) <- mget(ls(pattern=glob2rx("namesfor.*")))    # Create levels based on groups, see http://stackoverflow.com/questions/20176656/group-variables-in-a-dataframe-r-using-a-specific-list

namefreq <- aggregate(wordfreq ~ shortname, sum, data=termDocMat.RowSums)       # Sum
namefreq$shortname <- gsub("namesfor.", "", namefreq$shortname)                 # Remove "namesfor." from each group name to get the actual name
namefreq$propwordfreq <- 100 * namefreq$wordfreq / sum(namefreq$wordfreq)       # Normalise to plot percentage

hist_names_xaxis <- list(title = "")
hist_names_yaxis <- list(title = "Name Mention (% Of Total Names Mentioned)")

plot_ly(x=namefreq[order(namefreq$propwordfreq, decreasing=TRUE),"shortname"], y=namefreq[order(namefreq$propwordfreq, decreasing=TRUE),"propwordfreq"], type="bar") %>% layout(title = paste("Who Gets Talked About?", sep = ""),xaxis = hist_names_xaxis, yaxis = hist_names_yaxis)

sum(namefreq$wordfreq[namefreq$shortname %in% c("kirk","hagger","colin")])    # Kirk, Hagger and Colin make up 60% of the names mentioned



# See relative usage of each name based on messages sent
nameandsender <- merge(x=namefreq, y=aggregate(propmsgssent ~ sendershortname, senderfreq, sum), by.x="shortname", by.y="sendershortname")       # Get table of both variables
nameandsender$relnameuse <- 100 * (nameandsender$propwordfreq / nameandsender$propmsgssent - 1)  # Get relative name usage

hist_relnames_xaxis <- list(title = "")
hist_relnames_yaxis <- list(title = "Name Mention Per Message Sent (%)", dtick=50)

plot_ly(x=namefreq[order(nameandsender$relnameuse, decreasing=TRUE),"shortname"], y=nameandsender[order(nameandsender$relnameuse, decreasing=TRUE),"relnameuse"], type="bar") %>% layout(title = paste("Who Do We Think About?", sep = ""), xaxis = hist_relnames_xaxis, yaxis = hist_relnames_yaxis)



# See who talks about who (create heatmap showing percentage of messages mentioning another person)
msgsentnorm <- function(x, sndr) {
  x / sum(senderfreq$msgssent[senderfreq$sendershortname==sndr])
}

termDocMat.compcloud.names <- as.data.frame(termDocMat.compcloud)
termDocMat.compcloud.names <- termDocMat.compcloud.names[rownames(termDocMat.compcloud.names) %in% c(namesfor.broome, namesfor.chrisy, namesfor.colin, namesfor.hagger, namesfor.jim, namesfor.kev, namesfor.kirk, namesfor.saul, namesfor.stef, namesfor.stu),]
termDocMat.compcloud.names$namementioned <- factor(rownames(termDocMat.compcloud.names))
levels(termDocMat.compcloud.names$namementioned) <- mget(ls(pattern=glob2rx("namesfor.*")))   # Convert names to common name
namefreq.bysender <- termDocMat.compcloud.names %>% group_by(namementioned) %>% summarise(broome=sum(broome), chrisy=sum(chrisy), colin=sum(colin), hagger=sum(hagger), jim=sum(jim), kev=sum(kev), kirk=sum(kirk), saul=sum(saul), stef=sum(stef), stu=sum(stu)) %>% data.frame %>% melt
colnames(namefreq.bysender) <- c("namementioned", "sendershortname", "freq")
namefreq.bysender$namementioned <- gsub("namesfor.", "", namefreq.bysender$namementioned)
namefreq.bysender <- merge(x=namefreq.bysender, y=aggregate(msgssent ~ sendershortname, sum, data=senderfreq), by.x="sendershortname", by.y="sendershortname")
namefreq.bysender$freqnorm <- namefreq.bysender$freq / namefreq.bysender$msgssent

htmap.data <- as.matrix(cast(namefreq.bysender, namementioned~sendershortname, sum, value="freqnorm"))

htmap <- heatmap.2(htmap.data, Rowv=NA, Colv=NA, main="Who Mentions Who?", xlab="Sender", ylab="Name Mentioned")



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

filter.sentiment <- "disgust"
#anger, anticipation, disgust, fear, joy, sadness, surprise, trust

plot_ly(x=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "sendershortname"], y=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "propsentiment"], color=msgsentimentpp[msgsentimentpp$sentiment==filter.sentiment, "sentiment"], type="bar", colors=sample(colours(), 1)) %>% layout(title = paste("Sentiment Of ", filter.sentiment, " Per Person", sep = ""), xaxis = hist_sentimentpp_xaxis, yaxis = hist_sentimentpp_yaxis)

plot_ly(x=msgsentimentpp$sendershortname, y=msgsentimentpp$propsentiment, color=msgsentimentpp$sentiment, type="bar") %>% layout(title = paste("Sentiment Per Person", sep = ""), xaxis = hist_sentimentpp_xaxis, yaxis = hist_sentimentpp_yaxis)



# Check for emotionless messages
msgs[msgs$sendershortname=="stu" & msgs$anger==0 & msgs$anticipation==0 & msgs$disgust==0 & msgs$fear==0 & msgs$joy==0 & msgs$sadness==0 & msgs$surprise==0 & msgs$trust==0,]



# Quickest responders
binsize <- 60
maxresponseduration <- 3600
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

filter.sendershortname <- "colin"

y.dat <- sort(tdm.cossim[,c(filter.sendershortname)], decreasing=T)
x.dat <- colnames(tdm.cossim[order(tdm.cossim[,filter.sendershortname], decreasing = T)])

hist_similarity_xaxis <- list(title = "")
hist_similarity_yaxis <- list(title = "Cosine Similarity")

plot_ly(x=x.dat, y=y.dat, type="bar", color=x.dat[3], colors=sample(colours(), 1)) %>% layout(showlegend=F, title = paste("Who talks like", filter.sendershortname, "?", sep = " "), xaxis = hist_similarity_xaxis, yaxis = hist_similarity_yaxis)



# Exonerate or condemn Kirk from falling in love and ignoring the group
# Get standard colours for plots
inlove.cols <- c("Alone and unloved" = "black", "Deeply infatuated" = "red")

# Get date of Carly coming on scene and calculate date ranges to search
carly.date <- as.POSIXct("01/08/2017 00:00", format="%d/%m/%Y %H:%M", tz="UTC")

carly.til.now.days <- floor(as.numeric(difftime(time1=Sys.time(), time2=carly.date, units="days")))

pre.carly.date <- carly.date - (carly.til.now.days * 86400)

# Get only relevant messages and add in love status
msgs.kirk <- msgs[msgs$sendershortname == "kirk" & msgs$timestamp >= pre.carly.date,]
msgs.kirk$inlove <- ifelse(msgs.kirk$timestamp < carly.date, "Alone and unloved", "Deeply infatuated")

# Plot raw number of messages sent
chart.cols <- c(text="cornflowerblue", media="coral1", hyperlink="cyan3")
ggplot() + geom_bar(data=msgs.kirk, aes(x=inlove, fill=msgtype)) +
  labs(x="Relationship Status",
       y="Number of Messages Sent",
       title=paste0("Kirk's messages ", carly.til.now.days, " days before and after meeting Carly"),
       fill="Message Type") +
  scale_fill_manual(values=chart.cols)


# Let's look at how many messages were sent each day and the distributions
msgsbyday.kirk <- data.frame(table(msgs$day[msgs$sendershortname=="kirk" & msgs$timestamp >= pre.carly.date]))
colnames(msgsbyday.kirk) <- c("day", "msgssent")
msgsbyday.kirk$day <- as.POSIXct(msgsbyday.kirk$day, format="%d/%m/%Y")
msgsbyday.kirk <- msgsbyday.kirk[order(msgsbyday.kirk$day),]
msgsbyday.kirk$inlove <- ifelse(msgsbyday.kirk$day < carly.date, "Alone and unloved", "Deeply infatuated")

ggplot() + geom_density(data = msgsbyday.kirk, aes(x=msgssent, colour=inlove)) +
  labs(x="Messages Sent Per Day",
       y="Density",
       title="Distribution Of Kirk's Messages Sent Per Day Per Relationship Status",
       colour="Relationship Status") +
  scale_colour_manual(values=inlove.cols)


# Let's see how quickly Kirk responded to other messages
msg.response.kirk <- msg.response[msg.response$replier=="kirk" &
                                    msg.response$sendershortname!="kirk" &
                                    msgs$timestamp >= pre.carly.date,]
msg.response.kirk$responsetime <- as.numeric(msg.response.kirk$responsetime)
msg.response.kirk <- msg.response.kirk[!is.na(msg.response.kirk$bin),]  # Only keep messages where Kirk could have been reasonably expected to reply within the same conversation
msg.response.kirk$inlove <- ifelse(msg.response.kirk$timestamp < carly.date, "Alone and unloved", "Deeply infatuated")

ggplot() + geom_density(data = msg.response.kirk, aes(x=responsetime, colour=inlove)) +
  labs(x="Time To Reply (secs)",
       y="Density",
       title="Distribution Of Kirk's Time To Reply Per Relationship Status",
       colour="Relationship Status") +
  scale_colour_manual(values=inlove.cols) +
  xlim(0,1000)

msgs.replied.60secs.kirk.single <- nrow(msg.response.kirk[msg.response.kirk$inlove=="Alone and unloved" & msg.response.kirk$responsetime < 60,]) / nrow(msg.response.kirk[msg.response.kirk$inlove=="Alone and unloved",])
msgs.replied.60secs.kirk.notsingle <- nrow(msg.response.kirk[msg.response.kirk$inlove=="Deeply infatuated" & msg.response.kirk$responsetime < 60,]) / nrow(msg.response.kirk[msg.response.kirk$inlove=="Deeply infatuated",])









