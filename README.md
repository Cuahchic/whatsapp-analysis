# WhatsApp'ening!
Analyse messages from a group on WhatsApp.

Built in RStudio version 1.0.153 and using R version 3.3.1.

## Overview
So you're in a WhatsApp group with your friends, family or colleagues:

<img src="https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/WhatsApp%20Example.png" width="250">

And you want to know the answers to the burning questions such as:
* How many messages are sent per day?
* Who sends the most messages?
* What is each contributors favourite words?
* Who sends the most joyful or angriest messages?
* Who responds quickest?
* Who's messages are the most similar?

Fear not wearily traveller, this R code will answer these questions and more!

## Messages Sent Per Day
![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/01%20Messages%20per%20day.png)

## Messages By Sender
Split out by text, media (image or videos) or hyperlink posts.

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/02%20Messages%20by%20sender.png)

## Word Cloud
A word cloud shows the relative prevalence of a word by making the word larger or smaller. For example, in the word cloud below `xxx` is dis-proportionally used since it is much bigger than the other words.

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/03%20Word%20cloud%20-%20everyone.png)

## Comparison Cloud
A comparison cloud shows which words each person used more than other people, i.e. their unique vocabulary. In the example below `Colin` uses `thanks` much more than any other contributors.

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/05%20Comparison%20Cloud.png)

## Sentiment
The sentiment analysis looks for words that exhibit one of eight possible emotions - anger, anticipation, disgust, fear, joy, sadness, surprise and trust. Note that in the example below, since a message can contain more than one category, the bars will add to more than 100%.

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/07%20Sentiment%20per%20person%20-%20all.png)

## Response Time
Who's got the fastest fingers, calculated using 30 second bins.

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/08%20Response%20Time%20Per%20Person.png)

## Message Similarity
By calculating the cosine similarity of contributors word vector this shows how similarity each persons vocabulary is. Green shows similar vocabulary, whereas red shows dissimilar. Obviously comparing each person to themself shows green!

![](https://github.com/Cuahchic/whatsapp-analysis/blob/master/img/09%20Message%20Similarity.png)





