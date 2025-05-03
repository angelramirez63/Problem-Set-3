install.packages("tidyverse")
install.packages("tokenizers")
install.packages("syuzhet") # emociones

library(syuzhet)
library(tidyverse)
library(tokenizers)
library(ggplot2)
## Análisis
install.packages("wordcloud")
install.packages("tm")

library(tm)
library(wordcloud)
base_url <- "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/basic-text-processing-in-r/"

archivos <- sprintf("%s/sotu_text/%03d.txt", base_url, 1:236)
texto <- c()
for (f in archivos) {
  texto <- c(texto, paste(readLines(f), collapse = "\n"))
}

# forma alternativa

#archivos <- dir(input_loc, full.names = TRUE)
#texto <- c()
#for (f in archivos) {
#texto <- c(texto, paste(readLines(f), collapse = "\n"))
#}


metadatos <- read_csv(sprintf("%s/%s", base_url, "metadata.csv"))
metadatos
metadatos$textos <- texto


write_csv(metadatos, "C:/Users/danie/OneDrive - Universidad de los andes/Taller de R/2024-I/Clase 10/discursos.csv")

palabras <- tokenize_words(metadatos$textos)
longitudes <- sapply(palabras, length)

# grafico de la longitud de los textos
ggplot(data= metadatos, aes(x = year, y = sapply(palabras, length))) + 
  geom_point() + labs(x = "Año", y = "Número de palabras") + theme_bw()

# grafico de la longitud de los textos
ggplot(data= metadatos, aes(x = year, y = sapply(palabras, length), color= party)) + 
  geom_point() + labs(x = "Año", y = "Número de palabras") + theme_bw()


# discursos por presidente 
ggplot(data= metadatos, aes(x = president)) + geom_bar(orientation = "x") +
  coord_flip() +
  theme_minimal()  # Ejemplo de ajuste de tema

ggplot(metadatos, aes(x=reorder(president, freq_total), y = freq_total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip()

ggplot(data= metadatos, aes(x = president, fill = party)) + geom_bar(orientation = "x") +
  coord_flip() +
  theme_minimal()  # Ejemplo de ajuste de tema

metadatos <- metadatos %>% filter(year>1980)

# se agrega toda la información 
corpus <- Corpus(VectorSource(metadatos$textos))

######################$
# procesamos el texto 
corpus <- tm_map(corpus, content_transformer(tolower))  # Convertir a minúsculas
corpus <- tm_map(corpus, removePunctuation)             # Eliminar puntuación
corpus <- tm_map(corpus, removeNumbers)                 # Eliminar números
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Eliminar palabras vacías (stop words)
corpus <- tm_map(corpus, stripWhitespace)   

dtm <- TermDocumentMatrix(corpus)
# Aca ya lo puedo ver. 
tdm <- as.matrix(dtm)

## visualizar las palabras más usadas 
freq_total <- rowSums(tdm)
df_freq_total <- as.data.frame(freq_total)

# Ensure the word column is available and properly formatted
df_freq_total$word <- rownames(df_freq_total)

# Sort the dataframe by frequency in descending order and keep only the top 10 words
df_freq_total_top <- head(df_freq_total[order(-df_freq_total$freq_total), ], 15)

# Create a bar plot for the top 10 words
ggplot(df_freq_total_top, aes(x = word, y = freq_total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title = "Top 10 Most Frequent Words", x = "Words", y = "Frequency")

# Create a bar plot for the top 10 words
ggplot(df_freq_total_top, aes(x = reorder(word, freq_total), y = freq_total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title = "Top 10 Most Frequent Words", x = "Words", y = "Frequency")

# nube de palabras
# Create a word cloud using the top 10 words
wordcloud(
  words = df_freq_total$word, 
  freq = df_freq_total$freq_total, 
  min.freq = 1,            # Minimum frequency to include words
  max.words = 100,           # Maximum number of words
  random.order = FALSE,     # Arrange words by frequency
  colors = brewer.pal(8, "Dark2")  # Set color palette
)


# Que mas se puede hacer 
### Encontrar Asociaciones
findAssocs(dtm, terms = "war", corlimit = 0.72)  # Words correlated with 'victory'


# Analisis por clusters
library(cluster)
dist_matrix <- dist(t(tdm))  # Compute distances between terms
clustering <- hclust(dist_matrix, method = "ward.D2")
plot(clustering)  # Dendrogram of term clusters

# Latent Dirichlet Allocation
install.packages("topicmodels")
library(topicmodels)
lda_model <- LDA(t(tdm), k = 3)  # Find 3 topics
terms(lda_model, 10)  # Top 5 terms for each topic

## Sentiment analysis 
sentimientos <- get_nrc_sentiment(metadatos$textos, lang="english")
# vector de los nombres de las columnas
colnames(sentimientos) <- c('Rabia', 'Anticipacion', 'disgusto', 'miedo', "Alegria","Tristeza","sorpresa", 
                                  "confianza", "negativo", "positivo")

# barplot de los sentimientos
barplot(
  colSums(prop.table(sentimientos[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Analisis discursos desde 1980",
  xlab="Emociones", ylab = "Frecuencia relativa de la emoción")

simple_plot((sentimientos$negativo*-1) + sentimientos$positivo)

simple_plot(sentimientos$Rabia)


### Feature extraction 
# Function to check for a specific word in the corpus
# Add a dummy column to metadatos using str_detect
word_to_check <- "war"
metadatos <- metadatos %>%
  mutate(dummy = sapply(corpus, function(text) str_detect(text, fixed(word_to_check))))

# View the updated dataframe
head(metadatos)



# Stematization
library(SnowballC)
corpus <- tm_map(corpus, stemDocument, language = "english")
dtm <- TermDocumentMatrix(corpus)
# Aca ya lo puedo ver. 
tdm <- as.matrix(dtm)