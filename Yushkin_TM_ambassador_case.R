# Title: Case I - Hult International Business School Student Spotlight
# NAME: Alexey Yushkin
# Date: Feb 28 2021


# Setting working directory
setwd("~/Downloads/TEXT_ANALYTICS_AND_NLP/Hult_NLP/personal/session II/student ambassadors")


#################### Importing libraries and data #################### 

# Uploading libraries
library(tm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(plotrix)
library(formattable)
library(wordcloud)


# Loading data
txt <- read.csv("final_student_data.csv", header = TRUE)


#################### EDA #################### 

# Checking dimensions, column names, types of data, and content
glimpse(txt)


# Checking number of students by program
length(unique(txt$programTitle))
table(txt$programTitle) 
# 9 programs with number of students from 1 to 30
# bachelors 30, masters 55
# Master of International Business 30, other masters 25

# Creating list for visualization
program_lst <- txt %>% 
  count(programTitle)

# Plotting the barchart
ggplot(program_lst, aes(x = reorder(programTitle, n), y = n, fill = programTitle)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Number of students by program") +
  labs(
    x = "Program Title",
    y = "Counts"
  ) +
  coord_flip()


# Checking number of students by campus
table(txt$campus) 
# Boston 28, Dubai 15, London 34, San Francisco 8

# Creating list for visualization
campus_lst <- txt %>% 
  count(campus)

# Plotting the barchart
ggplot(campus_lst, aes(x = reorder(campus, n), y = n, fill = campus)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Number of students by campus") +
  labs(
    x = "Campus",
    y = "Counts"
  ) +
  coord_flip()


# Checking number of students by gender
table(txt$namSorGender.likelyGender) # gender
# female 47, male 38

# Creating list for visualization
gender_lst <- txt %>% 
  count(namSorGender.likelyGender)

# Plotting the barchart
ggplot(gender_lst, aes(x = reorder(namSorGender.likelyGender, n), y = n, fill = namSorGender.likelyGender)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Number of students by gender") +
  labs(
    x = "Gender",
    y = "Counts"
  ) +
  coord_flip()


# Checking number of students by country
length(unique(txt$namSorCountry.country))
length(unique(txt$isoCode.Country))
table(txt$namSorCountry.country)
table(txt$isoCode.Country)
# namSorCountry.country and isoCode.Country data are messed up
# 39 countries, 1 to 10 students per country

# Creating list for visualization
country_lst <- txt %>% 
  count(isoCode.Country)

# Plotting the barchart
ggplot(country_lst, aes(x = reorder(isoCode.Country, n), y = n, fill = isoCode.Country)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 18)) +
  ggtitle("Number of students by country") +
  labs(
    x = "Country",
    y = "Counts"
  ) +
  scale_y_discrete(limits=c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)) +
  coord_flip()


# Checking number of students by alternative country
length(unique(txt$namSorCountry.countryAlt))
length(unique(txt$isoCode.countryAlt))
table(txt$namSorCountry.countryAlt)
table(txt$isoCode.countryAlt)
# namSorCountry.countryAlt and isoCode.countryAlt data are messed up
# 45 alternative countries, 1 to 7 students per country

# Creating list for visualization
countryAlt_lst <- txt %>% 
  count(isoCode.countryAlt)

# Plotting the barchart
ggplot(countryAlt_lst, aes(x = reorder(isoCode.countryAlt, n), y = n, fill = isoCode.countryAlt)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 18)) +
  ggtitle("Number of students by alternative country") +
  labs(
    x = "Alternative Country",
    y = "Counts"
  ) +
  scale_y_discrete(limits=c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L)) +
  coord_flip()


# Checking number of students by subregion
length(unique(txt$namSorCountry.subRegion))
table(txt$namSorCountry.subRegion)
# 14 subregions, 2 to 20 students per subregion

# Creating list for visualization
subregion_lst <- txt %>% 
  count(namSorCountry.subRegion)

# Plotting the barchart
ggplot(subregion_lst, aes(x = reorder(namSorCountry.subRegion, n), y = n, fill = namSorCountry.subRegion)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Number of students by subregion") +
  labs(
    x = "Subregion",
    y = "Counts"
  ) +
  coord_flip()


# Checking number of students by region
length(unique(txt$namSorCountry.region)) 
table(txt$namSorCountry.region)
# 5 regions
# Africa 15, Asia 23, Europe 37, Latin America and the Caribbean 7, Oceania 3

# Creating list for visualization
region_lst <- txt %>% 
  count(namSorCountry.region)

# Plotting the barchart
ggplot(region_lst, aes(x = reorder(namSorCountry.region, n), y = n, fill = namSorCountry.region)) + 
  geom_col() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), panel.background = element_blank(),
        text = element_text(size = 20)) +
  ggtitle("Number of students by region") +
  labs(
    x = "Region",
    y = "Counts"
  ) +
  coord_flip()


#################### Preparing text data #################### 

# Creating the column for all text data
txt$allText <- paste(txt$interests, txt$bio)


# Creating variable with stopwords
stops <- stopwords("SMART")


# Declaring custom function
# Function for changing case to lower
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Function for text cleaning
clean_text <- function(text, stopwords){
  # Removing URLs
  text <- qdapRegex::rm_url(text)
  # Making lower case
  text <- tryTolower(text)
  # Removing stop words
  text <- removeWords(text, stopwords)
  # Removing punctuation
  text <- removePunctuation(text)
  # Removing numbers
  text <- removeNumbers(text)
  # Removing extra whitespace
  text <- stripWhitespace(text)
  # Removing leading and trailing whitespace
  text <- trimws(text, which = "both")
  return(text)
}


# Cleaning text data
txt$allText <- clean_text(txt$allText, stops)

# Tokenizing the text data
text <- txt %>% 
  unnest_tokens(word, allText)

# Compute word counts and arrange the counts in descending order
text %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(20)
# "hult" is present 266 times and since all texts were created by Hult students, it makes sense to exclude 
# "hult" and cities where campuses are located, as well as word "ambassador" and terms which are connected  
#  to student ambassadors' duties and countries of origin because in context of the research these words 
#  don't have much value in terms of additional information


# Appending stopwords
stops <- c(stopwords("SMART"), "hult", "ambassador", "london", "boston", "san", "francisco", "dubai", "italy",
           "feel", "free", "reach", "contact", "questions", "hong", "kong", "india", "indian", "philippines",
           "mexico")

# Removing stopwords and extra whitespace
txt$allText <- clean_text(txt$allText, stops)


#################### Creating custom functions #################### 

# Function which creates word frequency matrix and, if necessary, plots wordcloud
wfm <- function(txt_subset, control = list(), show.wc = FALSE, n_words = 25){
  # Creating corpus
  corp <- VCorpus(VectorSource(txt_subset$allText))
  # Creating DTM
  dtm <- as.matrix(DocumentTermMatrix(corp, 
                                      control = control))
  # Creating WFM
  wfm <- colSums(dtm)
  # Word cloud
  if (show.wc == TRUE){
    wordcloud(names(wfm), wfm, max.words = n_words, min.freq = 3, scale = c(2, 1))
  }
  # Replacing frequency with relative frequency because of difference in corpus counts
  wfm <- 100 * wfm / dim(txt_subset)[1]
  return(wfm)
}

# Function which transforms WFM into dataframe and, if necessary, plots barchart
wfm_df <- function(wfm, show.bc = FALSE, n_bars = 10){
  # Creating dataframe
  wfm_df <- data.frame(word = names(wfm), freq = wfm)
  # Removing indexes
  rownames(wfm_df) <- NULL
  # Sorting words by frequency
  wfm_df <- wfm_df[order(wfm_df$freq, decreasing = TRUE),]
  # Plotting WFM
  if (show.bc == TRUE){
    barplot(wfm_df$freq[1:n_bars], names.arg = wfm_df$word[1:n_bars], las = 2)
  }
  # Returning dataframe with WFM
  return(wfm_df)
}


# Function which shows pyramid plot
pyramid_plot <- function(corp_1, corp_2, labels, n_words = 20){
  # Merging WFMs to create a corpora
  df <- merge(corp_1, corp_2, by ="row.names")
  names(df) <- c("terms", "first", "second")
  
  # Organizing df for plotting
  df <- df[order(df$first, decreasing = TRUE), ]
  top_n <- df[n_words:1, ]
  
  # Pyramid plot
  pyramid.plot(lx = top_n$first, # left
               rx = top_n$second, # right
               labels = top_n$terms, # terms
               top.labels = labels, # corpora
               gap = 15, # space for terms to be read
               main = "Words in Common", # title
               unit = "Relative Word Frequencies %", # x label
               show.values = FALSE, # show values
               ndig = 0) # number of decimals if values are shown
}


# Bigram token maker
bigram_tokens <- function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


# Trigram token maker
trigram_tokens <- function(x){
  unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse = " "), 
         use.names = FALSE)
}


#################### Comparing bachelor and masters programs #################### 

# Labels for visualization
labels = c("Bachelor Program", "Terms", "Masters Programs")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$programTitle == "Bachelor of Business Administration")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$programTitle != "Bachelor of Business Administration")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(BachelorProgram = corp_1[1:n_words, 1], 
                         MastersPrograms = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                BachelorProgram = formatter("span", 
                                            style = x ~ ifelse(x %in% common_terms, 
                                                               style(color = "green", 
                                                                     font.weight = "bold"), 
                                                               style(color = "red"))),
                MastersPrograms = formatter("span", 
                                            style = x ~ ifelse(x %in% common_terms, 
                                                               style(color = "green", 
                                                                     font.weight = "bold"), 
                                                               style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


############### Comparing Master of International Business and other Masters Programs ############### 

# Labels for visualization
labels = c("Master of International Business", "Terms", "Other Masters Programs")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$programTitle == "Master of International Business")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, 
                txt$programTitle != "Master of International Business" & 
                txt$programTitle != "Bachelor of Business Administration")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(MasterOfInternationalBusiness = corp_1[1:n_words, 1], 
                         OtherMastersPrograms = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                MasterOfInternationalBusiness = formatter("span", 
                                                          style = x ~ ifelse(x %in% common_terms, 
                                                                             style(color = "green", 
                                                                                   font.weight = "bold"), 
                                                                             style(color = "red"))),
                OtherMastersPrograms = formatter("span", 
                                                 style = x ~ ifelse(x %in% common_terms, 
                                                                    style(color = "green", 
                                                                          font.weight = "bold"), 
                                                                    style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


#################### Comparing London and Boston campuses #################### 

# Labels for visualization
labels = c("London Campus", "Terms", "Boston Campus")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$campus == "London")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$campus == "Boston")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(LondonCampus = corp_1[1:n_words, 1], 
                         BostonCampus = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                LondonCampus = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red"))),
                BostonCampus = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)
  

# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


#################### Comparing all campuses #################### 

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$campus == "London")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$campus == "Boston")

# For corpus 3
# Subsetting text data
txt_3 <- subset(txt, txt$campus == "Dubai")

# For corpus 4
# Subsetting text data
txt_4 <- subset(txt, txt$campus == "San Francisco")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, corp_3, corp_4, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(LondonCampus = corp_1[1:n_words, 1], 
                         BostonCampus = corp_2[1:n_words, 1],
                         DubaiCampus = corp_3[1:n_words, 1],
                         SanFranciscoCampus = corp_4[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(intersect(intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1]), 
                                      corp_3[1:n_words, 1]), 
                            corp_4[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                LondonCampus = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red"))),
                BostonCampus = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red"))),
                DubaiCampus = formatter("span", 
                                        style = x ~ ifelse(x %in% common_terms, 
                                                           style(color = "green", 
                                                                 font.weight = "bold"), 
                                                           style(color = "red"))),
                SanFranciscoCampus = formatter("span", 
                                               style = x ~ ifelse(x %in% common_terms, 
                                                                  style(color = "green", 
                                                                        font.weight = "bold"), 
                                                                  style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Corpus 3
# Creating WFM and, if necessary, word cloud
wfm_3 <- wfm(txt_3, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_3 <- wfm_df(wfm_3, show.bc = FALSE)

# Corpus 4
# Creating WFM and, if necessary, word cloud
wfm_4 <- wfm(txt_4, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_4 <- wfm_df(wfm_4, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2, corp_3 = wfm_df_3, corp_4 = wfm_df_4)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Corpus 3
# Creating WFM and, if necessary, word cloud
wfm_3 <- wfm(txt_3, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_3 <- wfm_df(wfm_3, show.bc = FALSE)

# Corpus 4
# Creating WFM and, if necessary, word cloud
wfm_4 <- wfm(txt_4, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_4 <- wfm_df(wfm_4, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2, corp_3 = wfm_df_3, corp_4 = wfm_df_4)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Corpus 3
# Creating WFM and, if necessary, word cloud
wfm_3 <- wfm(txt_3, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_3 <- wfm_df(wfm_3, show.bc = FALSE)

# Corpus 4
# Creating WFM and, if necessary, word cloud
wfm_4 <- wfm(txt_4, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_4 <- wfm_df(wfm_4, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2, corp_3 = wfm_df_3, corp_4 = wfm_df_4)


#################### Comparing students interests by gender #################### 

# Labels for visualization
labels = c("Female", "Terms", "Male")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$namSorGender.likelyGender == "female")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$namSorGender.likelyGender == "male")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(FemaleStudents = corp_1[1:n_words, 1], 
                         MaleStudents = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                FemaleStudents = formatter("span", 
                                           style = x ~ ifelse(x %in% common_terms, 
                                                              style(color = "green", 
                                                                    font.weight = "bold"), 
                                                              style(color = "red"))),
                MaleStudents = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE, n_bars = 10)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE, n_bars = 10)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)


#################### Comparing students from Europe and Asia #################### 

# Labels for visualization
labels = c("Students from Europe", "Terms", "Students from Asia")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$namSorCountry.region == "Europe")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$namSorCountry.region == "Asia")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(StudentsFromEurope = corp_1[1:n_words, 1], 
                         StudentsFromAsia = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                StudentsFromEurope = formatter("span", 
                                           style = x ~ ifelse(x %in% common_terms, 
                                                              style(color = "green", 
                                                                    font.weight = "bold"), 
                                                              style(color = "red"))),
                StudentsFromAsia = formatter("span", 
                                         style = x ~ ifelse(x %in% common_terms, 
                                                            style(color = "green", 
                                                                  font.weight = "bold"), 
                                                            style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE, n_bars = 10)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE, n_bars = 10)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)


#################### Comparing students from Europe and other regions #################### 

# Labels for visualization
labels = c("Students from Europe", "Terms", "Students from Other Regions")

# For corpus 1
# Subsetting text data
txt_1 <- subset(txt, txt$namSorCountry.region == "Europe")

# For corpus 2
# Subsetting text data
txt_2 <- subset(txt, txt$namSorCountry.region != "Europe")


# Creating custom function which shows the table of most frequent words for two corpuses
comparing_table <- function(corp_1, corp_2, n_words = 10){
  # Creating dataframe with top 10 terms for each program
  top_n_df <- data.frame(StudentsFromEurope = corp_1[1:n_words, 1], 
                         StudentsFromOtherRegions = corp_2[1:n_words, 1])
  # Searching for common terms
  common_terms <- intersect(corp_1[1:n_words, 1], corp_2[1:n_words, 1])
  
  # Printing table of top n terms for each program
  formattable(top_n_df, 
              align = c("c","c","c","c"),
              list(
                StudentsFromEurope = formatter("span", 
                                               style = x ~ ifelse(x %in% common_terms, 
                                                                  style(color = "green", 
                                                                        font.weight = "bold"), 
                                                                  style(color = "red"))),
                StudentsFromOtherRegions = formatter("span", 
                                                   style = x ~ ifelse(x %in% common_terms, 
                                                                      style(color = "green", 
                                                                            font.weight = "bold"), 
                                                                      style(color = "red")))
              )
  )
}


# Since n-grams are much rare than unigrams, it makes sense to analyze them separately

# Unigrams
control = list()
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Bigrams
control = list(tokenize = bigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)

# Creating pyramid plot
pyramid_plot(corp_1 = wfm_1, 
             corp_2 = wfm_2,
             labels = labels)


# Trigrams
control = list(tokenize = trigram_tokens)
# Corpus 1
# Creating WFM and, if necessary, word cloud
wfm_1 <- wfm(txt_1, control = control, show.wc = FALSE)
# Creating WFM dataframe and plotting 10 most frequent words
wfm_df_1 <- wfm_df(wfm_1, show.bc = FALSE, n_bars = 10)

# Corpus 2
# Creating WFM and, if necessary, word cloud
wfm_2 <- wfm(txt_2, control = control, show.wc = FALSE)
# Creating WFM dataframe and, if necessary, plotting 10 most frequent words
wfm_df_2 <- wfm_df(wfm_2, show.bc = FALSE, n_bars = 10)

# Creating comparing table
comparing_table(corp_1 = wfm_df_1, corp_2 = wfm_df_2)


# End