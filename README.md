# Twitter-analysis-on-Tesla
ABSTRACT:</br>
    With the rapid development of Internet technology, people are increasingly accustomed to obtaining information and expressing their sentiments on the Internet. In the financial market, the sentiments in the financial news are supposed to have some correlation with the movement of the stock market. This paper aims to explore such correlation by conducting an empirical and data analysis.</br>
</br>
INTRODUCTION:</br>
    Sentiment analysis is the process of identifying the opinion expressed in text. Recently it has been used to study behavioural finance, and in particular the effect of opinions and emotions on economic or financial decisions. Sentiment analysis refers to the general method to extract polarity and subjectivity from semantic orientation which refers to the strength of words and polarity text or phrases. This project focuses on the financial market as the domain for sentiment analysis of text.</br>
</br>
MODULES:</br>
•	Streaming data from Twitter</br>
•	Pre-processing data</br>
•	Analysing the data</br>
•	Summarized text</br>
•	Training the model</br>
•	Visualization</br>
</br>
METHOD:
</br>
Machine-learning-based Approach:
    Machine learning methods often rely on supervised classification approaches where sentiment detection is framed as a binary which are positive and negative. This approach requires labelled data to train classifiers. This approach, it becomes apparent that aspects of the local context of a word need to be considered such as negative (e.g., Fall) and positive (e.g., Rise).
</br>
However, showed a basic paradigm for create a feature vector is:</br> 
•	Apply a part of speech tagger to each tweet post</br> 
•	Collect all the adjective for entire tweet posts </br>
•	Make a popular word set composed of the top N adjectives</br>
•	Navigate all the tweets in the experimental set to create the following:</br> 
1.	Number of positive words 
2.	Number of negative words 
3.	Presence, absence, or frequency of each word
</br>
•	MODEL DEVELOPMENT:</br>

1.	we decided to proceed with topic modelling as our analytical approach for model development. The idea is for us to identify topics as set of documents, select the right topic and create a final stock market Dataframe for prediction. In terms of topic modelling, we have selected Latent Dirichlet Allocation (LDA).</br>

2.	LDA is an unsupervised learning that views the documents as bag of words. In each topic that is generated, picks a set of words against it. Below outlines the each step the LDA does.</br>
	Assume there are k topics across all the documents.
	Distribute these topics across a document by assigning each word a topic.
	For each word in the document, assume its topic is wrong but every other word is assigned the topic is correct.
	Assign a word for each topic based on what topics are in the document and how many times a word has been assigned to a particular topic across all the documents.
	Repeat this process several times for each document.</br>

3.	Per-Document Classification: The tidy text package provides this method for extracting the per-topic-per-word probabilities, called β (“beta”), from the LDA model.</br>

4.	Document-topic probabilities: Besides estimating each topic as a mixture of words, LDA also models each document as a mixture of topics. We can examine the per-document-per-topic probabilities, called γ (“gamma”), with the matrix = "gamma" argument to tidy ().

</br>
•	SENTIMENT ANALYSIS: </br>

In the sentiment analysis, each tweet will get an emotion score. The ‘Syuzhet’ package breaks the emotion into 10 different emotions: anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive. Each tweet will be evaluated by these 10 emotions and then assigned a sum score.
</br>
•	We have defined the topics in sets of documents using LDA topics modelling, we have also assigned a tweet score with our sentiment analysis. Our next step is to map the sentiment scores against the stock price change.
 
![image](https://user-images.githubusercontent.com/66820548/117193290-4e310300-ae00-11eb-99cd-94d7f16630b5.png)

  IMPLEMENTATION:</br>

•	Separating date and hour from timestamp</br>
•	Remove meaningless characters and symbols</br>
•	Remove all non-ASCII characters from text</br>
•	Delete empty text column if present and Tweets that contained less than 20 characters (noise data).</br>
•	Add id column to consider each text row as a document.</br>
•	Creating Document Term Matrix:</br>
•	Select text and id column</br>
•	Create a corpus for document term matrix</br>
•	Remove all punctuation from the corpus</br>
•	Remove all English stop words and number from the corpus.</br>
