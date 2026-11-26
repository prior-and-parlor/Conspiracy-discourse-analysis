---
title: "Analysis of Conspiracy comments: LDA and Sentiment Analysis"
output: word_document
---
  
  # Introduction 
  
This document details the data analysis for the project *A real-time corpus-based analysis of conspiracy discourse on Reddit*. 

This part of the analysis aims to detect words and bigrams that differ in frequency between 1 June 2019 and 30 November 2020.

The analysis is initiated by clearing the workspace, setting options, and activating packages, as well as functions.
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,eval = T, message=FALSE, warning=FALSE)
```


# Loading all required libraries 
```{r}
library(tidyverse)
library(tm)
library(readxl)
library(textstem)
library(tidytext)

```

# Reading all Excel files from "~/Desktop/Conspiracy" folder

```{r}
file_names <- list.files("~/Desktop/textanalysis/Conspiracy", pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
Conspiracydata <- data.frame()

# Loop through each Excel file
for (file in file_names) {
  # Read the Excel file into a data frame
  data <- read_excel(file)
  
  # Append the data to the combined data frame
  Conspiracydata <- bind_rows(Conspiracydata, data)
}
```


```{r}
#download custom stopwords from the github
custom_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")
```

# Define function to clean conspiracy comments

```{r}
clean_reddit_comments <- function(data_frame) {
  # Remove rows where Username contains specific values
  usernames_to_drop <- c("t2_6l4z3", "t2_ppq7qo4", "t2_6tjuu")
  data_frame <- data_frame[!data_frame$Username %in% usernames_to_drop, ]
  
  #change to lower case
  data_frame$Body <- tolower(data_frame$Body)
  
  # Remove HTML tags and URLs
  data_frame$Body <- gsub("<.*?>", "", data_frame$Body) # Remove HTML tags
  data_frame$Body <- gsub("http\\S+\\s*", "", data_frame$Body) # Remove URLs
  
  # Remove special characters, punctuation, and numbers
  data_frame$Body <- gsub("[^a-zA-Z]", " ", data_frame$Body) # Remove non-alphabetic characters
  data_frame$Body <- gsub("\\b\\d+\\b", "", data_frame$Body) # Remove numbers
  
  # Replace double space with empty string
  data_frame$Body <- gsub("\\s+", " ", data_frame$Body)
  
  # Remove bullet points
  data_frame$Body <- gsub("^\\s*\\*\\s*", "", data_frame$Body)
  
  # Remove replies with parent comments within
  data_frame$Body <- gsub("^>.*$", "", data_frame$Body, perl = TRUE)
  
  # Remove rows where Username contains "bot"
  data_frame <- data_frame[!grepl("bot", data_frame$Username, ignore.case = TRUE), ]
  
  # Remove leading and trailing whitespace
  data_frame$Body <- trimws(data_frame$Body)
  
  # Remove stop words
  data_frame$Body <- removeWords(data_frame$Body, c(stopwords("english"), custom_stopwords))
  
  # Remove punctuation
  data_frame$Body <- removePunctuation(data_frame$Body)
  
  # Remove leading and trailing whitespace
  data_frame$Body <- trimws(data_frame$Body)
  
  # Remove rows where Body is empty
  data_frame <- data_frame[!data_frame$Body == "", ]
  
  # Lemmatize words
  data_frame$Body <- textstem::lemmatize_strings(data_frame$Body)
  data_frame$Date <- as.Date(substr(data_frame$Date,1,10))
  
  return(data_frame)
}

```

Clean conspiracy data 

```{r}
Conspiracydata <- clean_reddit_comments(Conspiracydata)
write.csv(Conspiracydata,"CleanedConspiracyfulldataset.csv")
```

Add periodisation

```{r}
Conspiracydata <- Conspiracydata %>% 
  mutate(doc_id = as.character(row_number())) %>% 
  dplyr::rename(text = Body) %>% 
  select(doc_id,text , Date) %>% 
  dplyr::mutate(NumDate = str_replace_all(Date, "-", ""),
                NumDate = as.numeric(NumDate),
                Period_Keyword = ifelse(NumDate <= 20191231, "Phase_01",
                                        "Phase_02")) %>%
  select(-NumDate)
# inspect data
head(Conspiracydata); nrow(Conspiracydata)

write.csv(Conspiracydata,"ConspiracywPhases.csv")

```

Create corpus and DTM

```{r }
# create corpus
corpus <- Corpus(DataframeSource(Conspiracydata))
# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 10
DTM <- DocumentTermMatrix(corpus, 
                          control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM)
```

Due to vocabulary pruning, DTM may have empty rows (problematic!): remove empty docs from DTM and metadata

```{r }
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
Conspiracydata <- Conspiracydata[sel_idx, ]
```

OPTIONAL TO RUN. NOT FOLLOWING THE TOPIC NUMBER RECOMMENDATION ANYWAY. 

# Determine optimal number of topics

The determination of the optimal number of topics follows @murzintcev2020idealtopics.

```{r }
# load packages
library(topicmodels)
library(ldatuning)
# create sample data set
ldatuneset <- Conspiracydata
ldatuneCorpus <- corpus <- Corpus(DataframeSource(ldatuneset))
ldatuneDTM <- DocumentTermMatrix(ldatuneCorpus,
                                 control = list(bounds = list(global = c(minimumFrequency, Inf))))
sel_idx <- slam::row_sums(ldatuneDTM) > 0
ldatuneDTM <- ldatuneDTM[sel_idx, ]
# create models with different number of topics
result <- FindTopicsNumber(
  ldatuneDTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
```
OPTIONAL CHUNK TO RUN.
Plot results (best number of topics: lowest CaoJuan2009, highest Griffith2004). 

```{r covtwit_02_06, echo=T, eval = T, message=FALSE, warning=FALSE}
FindTopicsNumber_plot(result)
```

Inspect the graph for a dip in minimisers and a peak in maximisers. Then, select K = n topics. Need to play around with the no. of topics.

```{r }

# load packages
library(topicmodels)
library(ldatuning)
# number of topics
K <- 200
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
```

Extract LDA results.

```{r }
# have a look at some of the results (posterior distributions)
LDA_Result <- posterior(topicModel)
# topics are probability distributions over the entire vocabulary
beta <- LDA_Result$terms   # get beta from results
# for every document we have a probability distribution of its contained topics
theta <- as.data.frame(LDA_Result$topics)
# extract dominant topic for each comment
Conspiracydata$DominantTopic <- colnames(theta)[apply(theta,1,which.max)]
head(Conspiracydata)
```
```{r}
write.csv(Conspiracydata, "TopicswComments.csv")
```

Write function for extracting the top ten distinctive terms per topic. This chunk works! Run it!
  
  ```{r }
ExtractTopicKeywords <- function(Conspiracydata, Topic = 1){
  source(here::here("/Users/abigailohara/Desktop/Abigail_textanalysis", "CoocStatzFisher1.R"))
  TopicTerms <- Conspiracydata %>%
    unnest_tokens(Word, text) %>%
    dplyr::mutate(DominantTopic = ifelse(DominantTopic == Topic, "Target", "NonTarget")) %>%
    group_by(DominantTopic) %>% 
    count(Word, sort = TRUE) %>% 
    spread(DominantTopic, n) %>% 
    replace(is.na(.), 0) %>%
    mutate(Total = Target+NonTarget) %>%
    dplyr::filter(Total > 10) %>%
    dplyr::mutate(TotalTarget = sum(Target),
                  TotalNonTarget = sum(NonTarget),
                  NRows = length(Word)) %>%
    dplyr::select(-Total)
  # extract keywords
  keywords <- CoocStatzFisher(TopicTerms)
  sigkeywords <- keywords %>%
    dplyr::filter(CorrSignificance != "n.s.",
                  Type == "Overuse") %>%
    dplyr::arrange(-phi)
  return(paste(sigkeywords$Word[1:10], collapse = "|"))
}
```

Extract key terms for topics.

```{r }
# apply function
SigKeywordsTopic01 <- ExtractTopicKeywords(Conspiracydata, 1)
SigKeywordsTopic02 <- ExtractTopicKeywords(Conspiracydata, 2)
SigKeywordsTopic03 <- ExtractTopicKeywords(Conspiracydata, 3)
SigKeywordsTopic04 <- ExtractTopicKeywords(Conspiracydata, 4)
SigKeywordsTopic05 <- ExtractTopicKeywords(Conspiracydata, 5)
SigKeywordsTopic06 <- ExtractTopicKeywords(Conspiracydata, 6)
SigKeywordsTopic07 <- ExtractTopicKeywords(Conspiracydata, 7)
SigKeywordsTopic08 <- ExtractTopicKeywords(Conspiracydata, 8)
SigKeywordsTopic09 <- ExtractTopicKeywords(Conspiracydata, 9)
SigKeywordsTopic10 <- ExtractTopicKeywords(Conspiracydata, 10)
SigKeywordsTopic11 <- ExtractTopicKeywords(Conspiracydata, 11)
SigKeywordsTopic12 <- ExtractTopicKeywords(Conspiracydata, 12)
SigKeywordsTopic13 <- ExtractTopicKeywords(Conspiracydata, 13)
SigKeywordsTopic14 <- ExtractTopicKeywords(Conspiracydata, 14)
SigKeywordsTopic15 <- ExtractTopicKeywords(Conspiracydata, 15)
SigKeywordsTopic16 <- ExtractTopicKeywords(Conspiracydata, 16)
SigKeywordsTopic17 <- ExtractTopicKeywords(Conspiracydata, 17)
SigKeywordsTopic18 <- ExtractTopicKeywords(Conspiracydata, 18)
SigKeywordsTopic19 <- ExtractTopicKeywords(Conspiracydata, 19)
SigKeywordsTopic20 <- ExtractTopicKeywords(Conspiracydata, 20)
SigKeywordsTopic21 <- ExtractTopicKeywords(Conspiracydata, 21)
SigKeywordsTopic22 <- ExtractTopicKeywords(Conspiracydata, 22)
SigKeywordsTopic23 <- ExtractTopicKeywords(Conspiracydata, 23)
SigKeywordsTopic24 <- ExtractTopicKeywords(Conspiracydata, 24)
SigKeywordsTopic25 <- ExtractTopicKeywords(Conspiracydata, 25)
SigKeywordsTopic26 <- ExtractTopicKeywords(Conspiracydata, 26)
SigKeywordsTopic27 <- ExtractTopicKeywords(Conspiracydata, 27)
SigKeywordsTopic28 <- ExtractTopicKeywords(Conspiracydata, 28)
SigKeywordsTopic29 <- ExtractTopicKeywords(Conspiracydata, 29)
SigKeywordsTopic30 <- ExtractTopicKeywords(Conspiracydata, 30)
SigKeywordsTopic31 <- ExtractTopicKeywords(Conspiracydata, 31)
SigKeywordsTopic32 <- ExtractTopicKeywords(Conspiracydata, 32)
SigKeywordsTopic33 <- ExtractTopicKeywords(Conspiracydata, 33)
SigKeywordsTopic34 <- ExtractTopicKeywords(Conspiracydata, 34)
SigKeywordsTopic35 <- ExtractTopicKeywords(Conspiracydata, 35)
SigKeywordsTopic36 <- ExtractTopicKeywords(Conspiracydata, 36)
SigKeywordsTopic37 <- ExtractTopicKeywords(Conspiracydata, 37)
SigKeywordsTopic38 <- ExtractTopicKeywords(Conspiracydata, 38)
SigKeywordsTopic39 <- ExtractTopicKeywords(Conspiracydata, 39)
SigKeywordsTopic40 <- ExtractTopicKeywords(Conspiracydata, 40)
SigKeywordsTopic41 <- ExtractTopicKeywords(Conspiracydata, 41)
SigKeywordsTopic42 <- ExtractTopicKeywords(Conspiracydata, 42)
SigKeywordsTopic43 <- ExtractTopicKeywords(Conspiracydata, 43)
SigKeywordsTopic44 <- ExtractTopicKeywords(Conspiracydata, 44)
SigKeywordsTopic45 <- ExtractTopicKeywords(Conspiracydata, 45)
SigKeywordsTopic46 <- ExtractTopicKeywords(Conspiracydata, 46)
SigKeywordsTopic47 <- ExtractTopicKeywords(Conspiracydata, 47)
SigKeywordsTopic48 <- ExtractTopicKeywords(Conspiracydata, 48)
SigKeywordsTopic49 <- ExtractTopicKeywords(Conspiracydata, 49)
SigKeywordsTopic50 <- ExtractTopicKeywords(Conspiracydata, 50)
SigKeywordsTopic51 <- ExtractTopicKeywords(Conspiracydata, 51)
SigKeywordsTopic52 <- ExtractTopicKeywords(Conspiracydata, 52)
SigKeywordsTopic53 <- ExtractTopicKeywords(Conspiracydata, 53)
SigKeywordsTopic54 <- ExtractTopicKeywords(Conspiracydata, 54)
SigKeywordsTopic55 <- ExtractTopicKeywords(Conspiracydata, 55)
SigKeywordsTopic56 <- ExtractTopicKeywords(Conspiracydata, 56)
SigKeywordsTopic57 <- ExtractTopicKeywords(Conspiracydata, 57)
SigKeywordsTopic58 <- ExtractTopicKeywords(Conspiracydata, 58)
SigKeywordsTopic59 <- ExtractTopicKeywords(Conspiracydata, 59)
SigKeywordsTopic60 <- ExtractTopicKeywords(Conspiracydata, 60)
SigKeywordsTopic61 <- ExtractTopicKeywords(Conspiracydata, 61)
SigKeywordsTopic62 <- ExtractTopicKeywords(Conspiracydata, 62)
SigKeywordsTopic63 <- ExtractTopicKeywords(Conspiracydata, 63)
SigKeywordsTopic64 <- ExtractTopicKeywords(Conspiracydata, 64)
SigKeywordsTopic65 <- ExtractTopicKeywords(Conspiracydata, 65)
SigKeywordsTopic66 <- ExtractTopicKeywords(Conspiracydata, 66)
SigKeywordsTopic67 <- ExtractTopicKeywords(Conspiracydata, 67)
SigKeywordsTopic68 <- ExtractTopicKeywords(Conspiracydata, 68)
SigKeywordsTopic69 <- ExtractTopicKeywords(Conspiracydata, 69)
SigKeywordsTopic70 <- ExtractTopicKeywords(Conspiracydata, 70)
SigKeywordsTopic71 <- ExtractTopicKeywords(Conspiracydata, 71)
SigKeywordsTopic72 <- ExtractTopicKeywords(Conspiracydata, 72)
SigKeywordsTopic73 <- ExtractTopicKeywords(Conspiracydata, 73)
SigKeywordsTopic74 <- ExtractTopicKeywords(Conspiracydata, 74)
SigKeywordsTopic75 <- ExtractTopicKeywords(Conspiracydata, 75)
SigKeywordsTopic76 <- ExtractTopicKeywords(Conspiracydata, 76)
SigKeywordsTopic77 <- ExtractTopicKeywords(Conspiracydata, 77)
SigKeywordsTopic78 <- ExtractTopicKeywords(Conspiracydata, 78)
SigKeywordsTopic79 <- ExtractTopicKeywords(Conspiracydata, 79)
SigKeywordsTopic80 <- ExtractTopicKeywords(Conspiracydata, 80)
SigKeywordsTopic81 <- ExtractTopicKeywords(Conspiracydata, 81)
SigKeywordsTopic82 <- ExtractTopicKeywords(Conspiracydata, 82)
SigKeywordsTopic83 <- ExtractTopicKeywords(Conspiracydata, 83)
SigKeywordsTopic84 <- ExtractTopicKeywords(Conspiracydata, 84)
SigKeywordsTopic85 <- ExtractTopicKeywords(Conspiracydata, 85)
SigKeywordsTopic86 <- ExtractTopicKeywords(Conspiracydata, 86)
SigKeywordsTopic87 <- ExtractTopicKeywords(Conspiracydata, 87)
SigKeywordsTopic88 <- ExtractTopicKeywords(Conspiracydata, 88)
SigKeywordsTopic89 <- ExtractTopicKeywords(Conspiracydata, 89)
SigKeywordsTopic90 <- ExtractTopicKeywords(Conspiracydata, 90)
SigKeywordsTopic91 <- ExtractTopicKeywords(Conspiracydata, 91)
SigKeywordsTopic92 <- ExtractTopicKeywords(Conspiracydata, 92)
SigKeywordsTopic93 <- ExtractTopicKeywords(Conspiracydata, 93)
SigKeywordsTopic94 <- ExtractTopicKeywords(Conspiracydata, 94)
SigKeywordsTopic95 <- ExtractTopicKeywords(Conspiracydata, 95)
SigKeywordsTopic96 <- ExtractTopicKeywords(Conspiracydata, 96)
SigKeywordsTopic97 <- ExtractTopicKeywords(Conspiracydata, 97)
SigKeywordsTopic98 <- ExtractTopicKeywords(Conspiracydata, 98)
SigKeywordsTopic99 <- ExtractTopicKeywords(Conspiracydata, 99)
SigKeywordsTopic100 <- ExtractTopicKeywords(Conspiracydata, 100)
SigKeywordsTopic101 <- ExtractTopicKeywords(Conspiracydata, 101)
SigKeywordsTopic102 <- ExtractTopicKeywords(Conspiracydata, 102)
SigKeywordsTopic103 <- ExtractTopicKeywords(Conspiracydata, 103)
SigKeywordsTopic104 <- ExtractTopicKeywords(Conspiracydata, 104)
SigKeywordsTopic105 <- ExtractTopicKeywords(Conspiracydata, 105)
SigKeywordsTopic106 <- ExtractTopicKeywords(Conspiracydata, 106)
SigKeywordsTopic107 <- ExtractTopicKeywords(Conspiracydata, 107)
SigKeywordsTopic108 <- ExtractTopicKeywords(Conspiracydata, 108)
SigKeywordsTopic109 <- ExtractTopicKeywords(Conspiracydata, 109)
SigKeywordsTopic110 <- ExtractTopicKeywords(Conspiracydata, 110)
SigKeywordsTopic111 <- ExtractTopicKeywords(Conspiracydata, 111)
SigKeywordsTopic112 <- ExtractTopicKeywords(Conspiracydata, 112)
SigKeywordsTopic113 <- ExtractTopicKeywords(Conspiracydata, 113)
SigKeywordsTopic114 <- ExtractTopicKeywords(Conspiracydata, 114)
SigKeywordsTopic115 <- ExtractTopicKeywords(Conspiracydata, 115)
SigKeywordsTopic116 <- ExtractTopicKeywords(Conspiracydata, 116)
SigKeywordsTopic117 <- ExtractTopicKeywords(Conspiracydata, 117)
SigKeywordsTopic118 <- ExtractTopicKeywords(Conspiracydata, 118)
SigKeywordsTopic119 <- ExtractTopicKeywords(Conspiracydata, 119)
SigKeywordsTopic120 <- ExtractTopicKeywords(Conspiracydata, 120)
SigKeywordsTopic121 <- ExtractTopicKeywords(Conspiracydata, 121)
SigKeywordsTopic122 <- ExtractTopicKeywords(Conspiracydata, 122)
SigKeywordsTopic123 <- ExtractTopicKeywords(Conspiracydata, 123)
SigKeywordsTopic124 <- ExtractTopicKeywords(Conspiracydata, 124)
SigKeywordsTopic125 <- ExtractTopicKeywords(Conspiracydata, 125)
SigKeywordsTopic126 <- ExtractTopicKeywords(Conspiracydata, 126)
SigKeywordsTopic127 <- ExtractTopicKeywords(Conspiracydata, 127)
SigKeywordsTopic128 <- ExtractTopicKeywords(Conspiracydata, 128)
SigKeywordsTopic129 <- ExtractTopicKeywords(Conspiracydata, 129)
SigKeywordsTopic130 <- ExtractTopicKeywords(Conspiracydata, 130)
SigKeywordsTopic131 <- ExtractTopicKeywords(Conspiracydata, 131)
SigKeywordsTopic132 <- ExtractTopicKeywords(Conspiracydata, 132)
SigKeywordsTopic133 <- ExtractTopicKeywords(Conspiracydata, 133)
SigKeywordsTopic134 <- ExtractTopicKeywords(Conspiracydata, 134)
SigKeywordsTopic135 <- ExtractTopicKeywords(Conspiracydata, 135)
SigKeywordsTopic136 <- ExtractTopicKeywords(Conspiracydata, 136)
SigKeywordsTopic137 <- ExtractTopicKeywords(Conspiracydata, 137)
SigKeywordsTopic138 <- ExtractTopicKeywords(Conspiracydata, 138)
SigKeywordsTopic139 <- ExtractTopicKeywords(Conspiracydata, 139)
SigKeywordsTopic140 <- ExtractTopicKeywords(Conspiracydata, 140)
SigKeywordsTopic141 <- ExtractTopicKeywords(Conspiracydata, 141)
SigKeywordsTopic142 <- ExtractTopicKeywords(Conspiracydata, 142)
SigKeywordsTopic143 <- ExtractTopicKeywords(Conspiracydata, 143)
SigKeywordsTopic144 <- ExtractTopicKeywords(Conspiracydata, 144)
SigKeywordsTopic145 <- ExtractTopicKeywords(Conspiracydata, 145)
SigKeywordsTopic146 <- ExtractTopicKeywords(Conspiracydata, 146)
SigKeywordsTopic147 <- ExtractTopicKeywords(Conspiracydata, 147)
SigKeywordsTopic148 <- ExtractTopicKeywords(Conspiracydata, 148)
SigKeywordsTopic149 <- ExtractTopicKeywords(Conspiracydata, 149)
SigKeywordsTopic150 <- ExtractTopicKeywords(Conspiracydata, 150)
SigKeywordsTopic151 <- ExtractTopicKeywords(Conspiracydata, 151)
SigKeywordsTopic152 <- ExtractTopicKeywords(Conspiracydata, 152)
SigKeywordsTopic153 <- ExtractTopicKeywords(Conspiracydata, 153)
SigKeywordsTopic154 <- ExtractTopicKeywords(Conspiracydata, 154)
SigKeywordsTopic155 <- ExtractTopicKeywords(Conspiracydata, 155)
SigKeywordsTopic156 <- ExtractTopicKeywords(Conspiracydata, 156)
SigKeywordsTopic157 <- ExtractTopicKeywords(Conspiracydata, 157)
SigKeywordsTopic158 <- ExtractTopicKeywords(Conspiracydata, 158)
SigKeywordsTopic159 <- ExtractTopicKeywords(Conspiracydata, 159)
SigKeywordsTopic160 <- ExtractTopicKeywords(Conspiracydata, 160)
SigKeywordsTopic161 <- ExtractTopicKeywords(Conspiracydata, 161)
SigKeywordsTopic162 <- ExtractTopicKeywords(Conspiracydata, 162)
SigKeywordsTopic163 <- ExtractTopicKeywords(Conspiracydata, 163)
SigKeywordsTopic164 <- ExtractTopicKeywords(Conspiracydata, 164)
SigKeywordsTopic165 <- ExtractTopicKeywords(Conspiracydata, 165)
SigKeywordsTopic166 <- ExtractTopicKeywords(Conspiracydata, 166)
SigKeywordsTopic167 <- ExtractTopicKeywords(Conspiracydata, 167)
SigKeywordsTopic168 <- ExtractTopicKeywords(Conspiracydata, 168)
SigKeywordsTopic169 <- ExtractTopicKeywords(Conspiracydata, 169)
SigKeywordsTopic170 <- ExtractTopicKeywords(Conspiracydata, 170)
SigKeywordsTopic171 <- ExtractTopicKeywords(Conspiracydata, 171)
SigKeywordsTopic172 <- ExtractTopicKeywords(Conspiracydata, 172)
SigKeywordsTopic173 <- ExtractTopicKeywords(Conspiracydata, 173)
SigKeywordsTopic174 <- ExtractTopicKeywords(Conspiracydata, 174)
SigKeywordsTopic175 <- ExtractTopicKeywords(Conspiracydata, 175)
SigKeywordsTopic176 <- ExtractTopicKeywords(Conspiracydata, 176)
SigKeywordsTopic177 <- ExtractTopicKeywords(Conspiracydata, 177)
SigKeywordsTopic178 <- ExtractTopicKeywords(Conspiracydata, 178)
SigKeywordsTopic179 <- ExtractTopicKeywords(Conspiracydata, 179)
SigKeywordsTopic180 <- ExtractTopicKeywords(Conspiracydata, 180)
SigKeywordsTopic181 <- ExtractTopicKeywords(Conspiracydata, 181)
SigKeywordsTopic182 <- ExtractTopicKeywords(Conspiracydata, 182)
SigKeywordsTopic183 <- ExtractTopicKeywords(Conspiracydata, 183)
SigKeywordsTopic184 <- ExtractTopicKeywords(Conspiracydata, 184)
SigKeywordsTopic185 <- ExtractTopicKeywords(Conspiracydata, 185)
SigKeywordsTopic186 <- ExtractTopicKeywords(Conspiracydata, 186)
SigKeywordsTopic187 <- ExtractTopicKeywords(Conspiracydata, 187)
SigKeywordsTopic188 <- ExtractTopicKeywords(Conspiracydata, 188)
SigKeywordsTopic189 <- ExtractTopicKeywords(Conspiracydata, 189)
SigKeywordsTopic190 <- ExtractTopicKeywords(Conspiracydata, 190)
SigKeywordsTopic191 <- ExtractTopicKeywords(Conspiracydata, 191)
SigKeywordsTopic192 <- ExtractTopicKeywords(Conspiracydata, 192)
SigKeywordsTopic193 <- ExtractTopicKeywords(Conspiracydata, 193)
SigKeywordsTopic194 <- ExtractTopicKeywords(Conspiracydata, 194)
SigKeywordsTopic195 <- ExtractTopicKeywords(Conspiracydata, 195)
SigKeywordsTopic196 <- ExtractTopicKeywords(Conspiracydata, 196)
SigKeywordsTopic197 <- ExtractTopicKeywords(Conspiracydata, 197)
SigKeywordsTopic198 <- ExtractTopicKeywords(Conspiracydata, 198)
SigKeywordsTopic199 <- ExtractTopicKeywords(Conspiracydata, 199)
SigKeywordsTopic200 <- ExtractTopicKeywords(Conspiracydata, 200)

# inspect data
head(SigKeywordsTopic01)
```

Create topic names based on the most distinctive terms for each topic.

Manually created topic names

```{r }
topicNames_manual <- c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10", "Topic11","Topic12", "Topic13", "Topic14", "Topic15","Topic16", "Topic17", "Topic18", "Topic19", "Topic20", "Topic21", "Topic22", "Topic23", "Topic24", "Topic25", "Topic26", "Topic27", "Topic28","Topic29", "Topic30", "Topic31", "Topic32", "Topic33", "Topic34", "Topic35", "Topic36", "Topic37", "Topic38", "Topic39", "Topic40", "Topic41", "Topic42", "Topic43", "Topic44", "Topic45", "Topic46", "Topic47", "Topic48", "Topic49", "Topic50", "Topic5", "Topic52", "Topic53", "Topic54", "Topic55", "Topic56", "Topic57", "Topic58", "Topic59", "Topic60","Topic61", "Topic62", "Topic63", "Topic64", "Topic65", "Topic66","Topic67", "Topic68", "Topic69", "Topic70", "Topic71", "Topic72", "Topic73", "Topic74", "Topic75", "Topic76", "Topic77", "Topic78", "Topic79", "Topic80", "Topic81", "Topic82", "Topic83", "Topic84", "Topic85", "Topic86", "Topic87", "Topic88", "Topic89", "Topic90", "Topic91", "Topic92", "Topic93", "Topic94", "Topic95", "Topic96", "Topic97", "Topic98", "Topic99", "Topic100", "Topic101", "Topic102", "Topic103", "Topic104", "Topic105", "Topic106", "Topic107", "Topic108", "Topic109", "Topic110", "Topic111","Topic112", "Topic113", "Topic114", "Topic115","Topic116", "Topic117", "Topic118", "Topic119", "Topic120", "Topic121", "Topic122", "Topic123", "Topic124", "Topic125", "Topic126", "Topic127", "Topic128","Topic129", "Topic130", "Topic131", "Topic132", "Topic133", "Topic134", "Topic135", "Topic136", "Topic137", "Topic138", "Topic139", "Topic140", "Topic141", "Topic142", "Topic143", "Topic144", "Topic145", "Topic146", "Topic147", "Topic148", "Topic149", "Topic150", "Topic151", "Topic152", "Topic153", "Topic154", "Topic155", "Topic156", "Topic157", "Topic158", "Topic159", "Topic160","Topic161", "Topic162", "Topic63", "Topic64", "Topic165", "Topic166","Topic167", "Topic168", "Topic169", "Topic170", "Topic171", "Topic172", "Topic173", "Topic174", "Topic175", "Topic176", "Topic177", "Topic178", "Topic179", "Topic180", "Topic181", "Topic182", "Topic183", "Topic184", "Topic185", "Topic186", "Topic187", "Topic188", "Topic189", "Topic190", "Topic191", "Topic192", "Topic193", "Topic194", "Topic195", "Topic196", "Topic197", "Topic198", "Topic199", "Topic200")
```

Add topic names to data

```{r covtwit_02_16, echo=T, eval = T, message=FALSE, warning=FALSE}


library(dplyr)
Conspiracydata <- Conspiracydata %>%mutate (Topic = topicNames_manual[as.integer(DominantTopic)] )

head(Conspiracydata)

write.csv(Conspiracydata,"termsandtopic.csv")
```
IMPORTANT. RUN THIS.

# Period Keywords

```{r }
perioddata <- Conspiracydata %>%
  dplyr::group_by(Period_Keyword) %>%
  unnest_tokens(Word, text) %>%
  dplyr::group_by(Period_Keyword, Word) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::group_by(Word) %>%
  dplyr::mutate(Total = sum(Frequency)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Word = ifelse(Total < 10, "other", Word)) %>%
  dplyr::group_by(Period_Keyword, Word) %>%
  dplyr::summarise(Frequency = sum(Frequency)) %>%
  tidyr::spread(Period_Keyword, Frequency) %>%
  replace(is.na(.), 0)
# inspect data
head(perioddata)
```


IMPORTANT! RUN THIS.
Function for data processing and extraction of significant keyterms


```{r covtwit_04_05, echo=T, eval = T, message=FALSE, warning=FALSE}
pdperiod <- function(perioddata, Targetperiod){
  periodtb <- perioddata %>%
    tidyr::gather(Period, Frequency, Phase_01:Phase_02) %>%
    dplyr::mutate(Period = ifelse(Period != Targetperiod, "Other", Period)) %>%
    dplyr::group_by(Period, Word) %>%
    dplyr::mutate(Frequency = sum(Frequency)) %>%
    unique() %>%
    tidyr::spread(Period, Frequency) %>%
    dplyr::rename(Target = Targetperiod, NonTarget = Other) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TotalTarget = sum(Target), TotalNonTarget = sum(NonTarget),
                  NRows = n())
  periodkeys <- CoocStatzFisher(periodtb)
  periodkeys <- periodkeys %>%
    dplyr::filter(CorrSignificance != "n.s.") %>%
    dplyr::mutate(Period = rep(Targetperiod, length(CorrSignificance)))
  return(periodkeys)
}
```
#-----------------------------------------------------------------------------#
IMPORTANT. RUN THIS.

# Extract keywords

```{r covtwit_04_06, echo=T, eval = T, message=FALSE, warning=FALSE}
keys_period1 <- pdperiod(perioddata, "Phase_01")
keys_period2 <- pdperiod(perioddata, "Phase_02")

# inspect data
head(keys_period1)
```

IMPORTANT. RUN THIS.

Combine keywords

```{r covtwit_04_07, echo=T, eval = T, message=FALSE, warning=FALSE}

period_keys <- rbind(keys_period1, keys_period2)
# inspect data
head(period_keys)
```

IMPORTANT. RUN THIS.

# Visualize keywords

```{r }
p1d <- period_keys %>%
  dplyr::mutate(Period = as.numeric(stringr::str_remove_all(Period, "Phase_"))) %>%
  dplyr::filter(CorrSignificance == "p<.001") %>%
  dplyr::filter(Word != "other") %>%
  #  dplyr::filter(phi > .001) %>%
  dplyr::mutate(x2 = log(x2)) %>%
  dplyr::mutate(x2 = ifelse(Type == "Overuse", x2, -x2)) 

```
#------------------------------------------------------------------------------#
RUN THIS FOR KEYWORD ASSOCIATION STRENGTH.
  
  ```{r covtwit_04_09, echo=T, eval = T, message=FALSE, warning=FALSE}

p1 <- ggplot(p1d, aes(x = Period, y = x2)) +
  #  geom_point()+
  #geom_text(aes(label=Word),hjust=0, vjust=-1,
  #          position=position_jitter(width=.5,height=1), size = 2) +
  geom_text(aes(label = Word), check_overlap = TRUE, vjust = 1.5) +
  labs(x = "Phase", y = "Association strength (logged X2)") +
  scale_x_continuous(breaks = seq(1, 2, 1), 
                     labels= seq(1, 2, 1), 
                     limits = c(0.5, 9.5)) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 0, size =7.5),
        plot.margin = unit(c(.2, .2, .2, .2), "cm")) +
  coord_cartesian(ylim = c(-10, 10)) 

ggsave(file = here::here("images", "Keyterms_Period.png"), 
       height = 4,  width = 2, dpi = 320)
p1
```


# Sentiment Analysis

Perform sentiment analysis on cleaned texts

```{r }
# load library
library(syuzhet)
```


```{r }
# perform sentiment analysis
sentiments <- get_nrc_sentiment(Conspiracydata$text)
# add doc_id to sentiments
Conspiracydata <- cbind(Conspiracydata, sentiments)
head(Conspiracydata)
write.csv(Conspiracydata, "sentimentanalysis_phases1+2.csv")
```

```{r }
Conspiracydata=read.csv("sentimentanalysis_phases1+2.csv")

```


```{r }
p9d <- Conspiracydata %>%
  rename(Period=Period_Keyword) %>% 
  dplyr::select(Period, Topic, anger, fear, sadness, anticipation, joy, trust) %>%
  gather(Emotion, Rate, anger:trust) %>%
  dplyr::mutate(Period = str_remove_all(Period, "Phase_"),
                Period = as.numeric(Period)) %>%
  dplyr::group_by(Period, Topic, Emotion) %>%
  dplyr::summarise(Rate = mean(Rate)) %>%
  
  dplyr::ungroup() %>%
  dplyr::mutate(Emotion = toupper(Emotion),
                Emotion = factor(Emotion, levels = c("ANGER", "ANTICIPATION",  "FEAR",
                                                     "JOY", "SADNESS", "TRUST")))
# inspect data
head(p9d)
```


```{r covtwit_03_07, echo=T, eval = T, message=FALSE, warning=FALSE}
grey_colors <- c("#444c38", "#c4aead", "#8c92ac", "#997a8d", "#898a74", "#989898", "#c9c0bb", "#465945", "#f4f0ec", "#8b8589", "#bfc1c2", "#cfcfc4", "#666699", "#dcdcdc", "#4f3a3c", "#848482", "#98817b", "#e5e4e2", "#4c516d", "#c0c0c0", "#c4c3d0", "#5e644f", "#51484f", "#acacac", "#7e5e60", "#2f4f4f", "#8a7f80", "#dbd7d2", "#232b2b", "#d3d3d3", "#353839", "#708090", "#b2beb5", "#808080", "#777696", "#4c516d", "#534b4f", "#6e7f80", "#91a3b0", "#4f666a", "#696969", "#645452", "#979aaa", "#354230", "#738276", "#253529", "#a9a9a9", "#4a646c", "#bebebe", "#838996", "#444c38", "#c4aead", "#8c92ac", "#997a8d", "#898a74", "#989898", "#c9c0bb", "#465945", "#f4f0ec", "#8b8589", "#bfc1c2", "#cfcfc4", "#666699", "#dcdcdc", "#4f3a3c", "#848482", "#98817b", "#e5e4e2", "#4c516d", "#c0c0c0", "#c4c3d0", "#5e644f", "#51484f", "#acacac", "#7e5e60", "#2f4f4f", "#8a7f80", "#dbd7d2", "#232b2b", "#d3d3d3", "#353839", "#708090", "#b2beb5", "#808080", "#777696", "#4c516d", "#534b4f", "#6e7f80", "#91a3b0", "#4f666a", "#696969", "#645452", "#979aaa", "#354230", "#738276", "#253529", "#a9a9a9", "#4a646c", "#bebebe", "#838996", "#444c38" ,"#c4aead" ,"#8c92ac", "#997a8d", "#898a74", "#989898", "#c9c0bb", "#465945", "#f4f0ec", "#8b8589", "#bfc1c2", "#cfcfc4", "#666699", "#dcdcdc", "#4f3a3c", "#848482", "#98817b", "#e5e4e2", "#4c516d", "#c0c0c0", "#c4c3d0", "#5e644f", "#51484f", "#acacac", "#7e5e60", "#2f4f4f", "#8a7f80", "#dbd7d2", "#232b2b", "#d3d3d3", "#353839", "#708090", "#b2beb5", "#808080", "#777696", "#4c516d", "#534b4f", "#6e7f80", "#91a3b0", "#4f666a", "#696969", "#645452", "#979aaa", "#354230", "#738276", "#253529", "#a9a9a9", "#4a646c", "#bebebe", "#838996", "#444c38", "#c4aead", "#8c92ac", "#997a8d", "#898a74", "#989898", "#c9c0bb", "#465945", "#f4f0ec", "#8b8589", "#bfc1c2", "#cfcfc4", "#666699", "#dcdcdc", "#4f3a3c", "#848482", "#98817b", "#e5e4e2", "#4c516d", "#c0c0c0", "#c4c3d0", "#5e644f", "#51484f", "#acacac", "#7e5e60", "#2f4f4f", "#8a7f80", "#dbd7d2", "#232b2b", "#d3d3d3", "#353839", "#708090", "#b2beb5", "#808080", "#777696", "#4c516d", "#534b4f", "#6e7f80", "#91a3b0", "#4f666a", "#696969", "#645452", "#979aaa", "#354230", "#738276", "#253529", "#a9a9a9", "#4a646c", "#bebebe", "#838996")
ggplot(p9d, aes(x = Period, y = Rate, group = Emotion, color = Emotion)) +
  #  facet_wrap(vars(Topic), ncol = 2, scales="free_y") +
  facet_wrap(vars(Topic), ncol = 1) +
  geom_line(size = .75) +  
  labs(x = "Phase", y = "Rate (among Conspiracy comments)") +
  theme_set(theme_bw(base_size = 12)) +
  theme(legend.position="top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 0, size =10)) +
  scale_colour_manual(values = paste0("gray", rep(c(30, 70), each = 3)),
                      name = "Core emotion",
                      breaks = names(table(p9d$Emotion)),  
                      labels = names(table(p9d$Emotion))) +
  scale_linetype_manual(labels = names(table(p9d$Emotion)),
                        breaks = names(table(p9d$Emotion)),
                        values = rep(c(1:3), 2), 
                        name = "Core emotion") +
  scale_x_continuous(breaks = seq(1, 9, 1), 
                     labels= seq(1, 9, 1), 
                     limits = c(1, 9)) +  
  theme(legend.position="top", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust = 1)) 

ggsave(file = here::here("images", "Conspiracy_Phases_Emotions.png"),
       height = 100,  width = 5, dpi = 320)
```

```{r covtwit_03_08, echo=T, eval = T, message=FALSE, warning=FALSE}
ggplot(p9d, aes(x = Period, y = Rate, group = Topic, color = Topic)) +
  #  facet_wrap(vars(Topic), ncol = 2, scales="free_y") +
  facet_wrap(vars(Emotion), ncol = 2) +
  geom_line(size = .75) +  
  labs(x = "Phase", y = "Rate (among Conspiracy comments)") +
  guides(color=guide_legend(nrow = 3)) +
  theme_set(theme_bw(base_size = 12)) +
  theme(legend.position="left",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 0, size =10)) +
  scale_colour_manual(labels = names(table(p9d$Topic)),
                      breaks = names(table(p9d$Topic)),
                      values = grey_colors,
                      name = "") +
  scale_linetype_manual(labels = names(table(p9d$Topic)),
                        breaks = names(table(p9d$Topic)),
                        values = seq(1, 5, 1), 
                        name = "") +
  scale_x_continuous(breaks = seq(1, 9, 1), 
                     labels= seq(1, 9, 1), 
                     limits = c(1, 9)) +  
  theme(legend.position="top", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust = 1)) 

ggsave(file = here::here("images", "Conspiracy_Phases_ByEmotion.png"),
       height = 20,  width = 5, dpi = 320)
```

Reformat data for mosaicplot

```{r covtwit_04_10, echo=T, eval = T, message=FALSE, warning=FALSE}
topicsperiod <- Conspiracydata %>%
  dplyr::select(Topic, Period_Keyword) %>%
  dplyr::mutate(Topic = str_remove_all(Topic, "_.*"),
                Topic = factor(Topic)) %>%
  group_by(Period_Keyword, Topic) %>%
  dplyr::summarise(Frequency = n()) %>%
  tidyr::spread(Topic, Frequency)
# inspect data
topicsperiod
```


```{r covtwit_04_11, echo=T, eval = T, message=FALSE, warning=FALSE}
topicsperiod_x2 <- chisq.test(as.matrix(topicsperiod[, 2:ncol(topicsperiod)]))
stdres <- topicsperiod_x2$stdres
rownames(stdres) <- topicsperiod$Period
stdres
```
# rownames

jpeg(here::here("images", "Conspiracy_Mosaic_PeriodsTopic.jpg"),
     quality = 100, width = 960, height = 1160)
mosaicplot(topicsperiod, shade = TRUE, type = "pearson", main = "", cex = 1.25)
dev.off()
mosaicplot(topicsperiod, shade = TRUE, type = "pearson", main = "")
```
