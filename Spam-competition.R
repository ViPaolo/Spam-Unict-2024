#### Import libraries and data ####

lib<- c("here","dplyr", "ggcorrplot","Metrics","boot","MASS", "readxl","tidyverse", "tidytext","tm","smotefamily","textstem","glmnet","ROSE", "caret", "writexl", "openxlsx") #libraries' name
#lapply(lib,install.packages, dependencies=TRUE)
lapply(lib, library, character.only = TRUE) #import all libraries at once
training <- read_excel(here("Dati", "spam_train.xlsx"))
test <- read_excel(here("Dati", "spam_test.xlsx"))

dataset <- training
dataset <- na.omit(dataset)



#### Feature Creation ####

#transform spam in 1 and 0
dataset <- dataset %>%
  mutate(class = ifelse(class == "spam", 1, ifelse(class == "ham", 0, NA)))

#create the list of known spam email 
spam_mail <- training$email[training$class == "spam"]

# Number of digits
dataset$digits <- str_count(dataset$email, "[0-9]")

#character count
dataset$length <- nchar(dataset$email)

#Uppercase character count
dataset$up_ch <- numeric(nrow(dataset)) #index to change with with character count

for (i in 1:nrow(dataset)) {
  # gregexpr to find all uppercase letters
  matches <- gregexpr("[A-Z]", dataset$email[i])
  # New column with count of uppercase letters
  dataset$up_ch[i] <- sum(unlist(matches) != -1)
}

#symbol character count 

dataset$symbol_count <- numeric(nrow(dataset))  # Symbol count

for (i in 1:nrow(dataset)) {
  # To find symbols
  matches <- gregexpr("[[:punct:]]", dataset$email[i])  # [:punct:] to capture punctuation, $ to capture dollar signs, (){} to capture brackets
  dataset$symbol_count[i] <- sum(unlist(matches) != -1)
}

# Check the presence of URL in the text 
dataset$contiene_url <- str_detect(dataset$email, "(https?://|www\\.)[\\S]+") #REGEX 

# Mail similarities 
dataset$duplicated <- ifelse(duplicated(dataset$email) | duplicated(dataset$email, fromLast = TRUE), TRUE, FALSE)

#### corpus #### 

#Creation of the document corpus
corpus <- Corpus(VectorSource(dataset$email))

# Document preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))  # Lowercase letters
corpus <- tm_map(corpus, removePunctuation)            # Punctuation
corpus <- tm_map(corpus, removeNumbers)                # Remove numbers
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b[\\w\\.-]+@[\\w\\.-]+\\b", "", x))) # email remotion
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www.\\S+", "", x))) # URL remotion
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b\\d{4}-\\d{2}-\\d{2}\\b", "", x))) # date remotion (format YYYY-MM-DD)
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Remove stopwords
corpus <- tm_map(corpus, function(x) lemmatize_words(x)) # lemmatization

corpus <- tm_map(corpus, stripWhitespace)              #Remove additional white spaces. 

# tokenize the data
dtm <- DocumentTermMatrix(corpus) 
dtm_tfidf <- weightTfIdf(dtm) # Convert matrix  TF-IDF in data frame
dtm_df <- as.data.frame(as.matrix(dtm_tfidf))

# Compute absolute frequencies 
frequenza_assoluta <- colSums(dtm_df)

#  99 percentile threshold
treshold <- quantile(frequenza_assoluta, probs = 0.99)

# Select only variables with value higher than threshold
dtm_df <- dtm_df[, colSums(dtm_df) > treshold]

dtm_df$id <- 1:nrow(dtm_df)  #Check to see if ID already exists 
dataset$id <- 1:nrow(dataset)
dataset<-merge(dataset, dtm_df, by = "id", all.x = TRUE) #merging with dataset. 


#### Logistic Regression Training ####
trial <- dataset[,-3]
trial <- trial[,-1]


#AIC: 599
model <- glm(class ~ digits + length + up_ch + symbol_count + contiene_url + 
               duplicated + dont + can + send + thanks + think + free + 
               get + now + take + call + claim + `â£` + mobile + number + 
               pls + many + reply + tomorrow + want + just + money + also + 
               babe + getting + please + message + text + txt + dear + thing + 
               time + back + home + stop + youre + like + new + phone + 
               good + thank + know + cool + min + great + care + last + 
               make + night + week + done + love + will + `next` + se, data = trial, family = binomial)


#### Model validation ####


#validation 

dataset2 <-dataset[,-3] #remove emails
dataset2 <- dataset2[,-1]

set.seed(123) # for reproducibility
#requires caret package
train_indices <- createDataPartition(dataset2$class, p = 0.8, list = FALSE)

train_data <- dataset2[train_indices, ]
test_data <- dataset2[-train_indices, ]

table(train_data$class)
table(test_data$class)


#model prediction

prediction <- predict.glm(model, newdata = test_data, type = "response")

prediction[prediction>= 0.5] <- 1
prediction[prediction < 0.5] <- 0

confusion_matrix <- as.matrix(table(Prediction = prediction, True = test_data$class))
print(confusion_matrix)

#Cross validation
cv_glm_trial <- cv.glm(trial, model,K=10)
cv_glm_trial$delta

#metrics train data

accuracy = (confusion_matrix[2,2]+confusion_matrix[1,1]) / (confusion_matrix[2,2]+confusion_matrix[1,1]+confusion_matrix[1,2]+confusion_matrix[2,1]) 
error_rate = 1 - accuracy

print(accuracy)
print(error_rate)


                 

#### Test set prediction ####
datatest<-test



# Number of digits
datatest$digits <- str_count(datatest$email, "[0-9]")

#character count
datatest$length <- nchar(datatest$email)

#Uppercase character count
datatest$up_ch <- numeric(nrow(datatest)) #serve per dare un indice da sostituire  con il conteggio dei caratteri 

for (i in 1:nrow(datatest)) {
  # Applica gregexpr per trovare i caratteri maiuscoli in ciascuna riga di testo
  matches <- gregexpr("[A-Z]", datatest$email[i])
  # Conta il numero di occorrenze di caratteri maiuscoli nella riga di testo e assegna il valore alla nuova colonna
  datatest$up_ch[i] <- sum(unlist(matches) != -1)
}

#symbol character count 

datatest$symbol_count <- numeric(nrow(datatest))  # Crea una nuova colonna per il conteggio dei simboli

for (i in 1:nrow(datatest)) {
  # Applica gregexpr per trovare i simboli nella mail
  matches <- gregexpr("[[:punct:]]", datatest$email[i])  # [:punct:] to capture punctuation, $ to capture dollar signs, (){} to capture brackets
  # Conta il numero di occorrenze di simboli nella mail e assegna il valore alla nuova colonna
  datatest$symbol_count[i] <- sum(unlist(matches) != -1)
}

# Check the presence of URL in the text 
datatest$contiene_url <- str_detect(datatest$email, "(https?://|www\\.)[\\S]+") #REGEX

#see the site where the Url takes to (e.g. if it does take to trusted site, it's less likely it's a spam)

datatest$duplicated <- ifelse(duplicated(datatest$email) | duplicated(datatest$email, fromLast = TRUE), TRUE, FALSE)

# Document Corpus
corpus <- Corpus(VectorSource(datatest$email))

# Document preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)           
corpus <- tm_map(corpus, removeNumbers)               
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b[\\w\\.-]+@[\\w\\.-]+\\b", "", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www.\\S+", "", x))) 
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b\\d{4}-\\d{2}-\\d{2}\\b", "", x))) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))  
corpus <- tm_map(corpus, function(x) lemmatize_words(x)) 

corpus <- tm_map(corpus, stripWhitespace)              

# tokenize the data
dtm <- DocumentTermMatrix(corpus) 

dtm_tfidf <- weightTfIdf(dtm) 

dtm_df <- as.data.frame(as.matrix(dtm_tfidf))

dtm_df$id <- 1:nrow(dtm_df)  #fai un check per vedere se non esiste già un id 
datatest$id <- 1:nrow(datatest)
datatest<-merge(datatest, dtm_df, by = "id", all.x = TRUE)


#### Fitting the model to the test data ####



#it misses lenght, we create it
datatest$length <- datatest$length.x
test_predict <-predict.glm(model, newdata  = datatest[,-1], type = "response")

test_predict <- ifelse(test_predict > 0.5, 1, 0)

## Export Data

datatest$class <- test_predict
datatest$email <- datatest$email.x

final_df <- datatest[, c("id_number", "email", "class")]


write.xlsx(final_df, "D:/Unict/2 Semestre/Statistical learning/Projects/Competizione/Dati/predictions.xlsx")

#write.xlsx(final_df, here("Dati", "output.xlsx"))






