library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)

data <- read.csv("complaints.csv")

data$Date.received <- as.Date(data$Date.received,
                              format = "%Y-%m-%d")

data$Date.sent.to.company <- as.Date(data$Date.sent.to.company,
                              format = "%Y-%m-%d")

data$Consumer.complaint.narrative[which(data$Consumer.complaint.narrative == "")] <- NA

as.data.frame(sort(table(data$Company, useNA = "always"), decreasing = T)) -> df_bank

as.character(df_bank$Var1[1:5])

df_ba <- data |> subset(Company == "BANK OF AMERICA, NATIONAL ASSOCIATION")

df_ba$Consumer.complaint.narrative[which(df_ba$Consumer.complaint.narrative == "")] <- NA

# Use word cloud and sentiment bing
# get_sentiments("bing")

# tokenize words
df_ba <- df_ba %>%
  unnest_tokens(word, Consumer.complaint.narrative)

# get negative words from bing
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# count negative word to prep for word cloud
negative_word_count <- df_ba %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

library(wordcloud2)
library(webshot)
library(htmlwidgets)
wordcloud2(data = negative_word_count,
                             size = 3,
                             gridSize = 3,
                             color = "random-dark",
                             shape = "circle",
                             backgroundColor = "white")

# Create chart to analyze emotion based on each product
complaint_product_sentiment <- df_ba %>%
  inner_join(get_sentiments("bing"),
             relationship = "many-to-many") %>%
  count(Product, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


complaint_product_sentiment$Product <- reorder(complaint_product_sentiment$Product,
                                               complaint_product_sentiment$sentiment)



levels(complaint_product_sentiment$Product)[1:20] -> top10

complaint_product_sentiment_top10 <- complaint_product_sentiment |> subset(Product %in% top10)

complaint_product_sentiment_top10$Product <- as.character(complaint_product_sentiment_top10$Product)

complaint_product_sentiment_top10$Product[8] <- "Credit reporting, credit repair services,\nor other personal consumer reports"

complaint_product_sentiment_top10$Product <- reorder(complaint_product_sentiment_top10$Product,
                                               complaint_product_sentiment_top10$sentiment)

library(RColorBrewer)

n <- 10
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
set.seed(17)
col_vector_a <- col_vector[sample(1:length(col_vector), size = n)]

ggplot(complaint_product_sentiment_top10, 
       aes(x = Product, 
           y = sentiment,
           fill = Product)) +
  geom_col() +
  labs(title = "Net Sentiment by Product",
       x = "Product",
       y = "Net Sentiment (Positive - Negative)") +

  
  theme_bw(base_size = 10) +

theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                 color = "black"))  +
  
  theme(text = element_text(face = "bold",
                            color = "black"))

## Comparative Analysis using `nrc` sentiment

nrc <- get_sentiments("nrc")

# join with nrc
complaints_tibble <- df_ba %>%
  anti_join(stop_words) %>% # remove common stop word
  inner_join(nrc, relationship = "many-to-many")

# calculate emotion scores for each complaint
emotion_scores <- complaints_tibble %>%
  count(Complaint.ID, Consumer.disputed., sentiment) %>%
  group_by(Complaint.ID) %>%
  mutate(emotion_proportion = n / sum(n)) %>%
  ungroup()

# aggregate emotions by dispute status
emotion_by_dispute <- emotion_scores %>%
  group_by(Consumer.disputed., sentiment) %>%
  summarise(
    avg_score = mean(n, na.rm = TRUE),
    avg_proportion = mean(emotion_proportion, na.rm = TRUE),
    .groups = "drop"
  )

# Create stacked bar chart visualization
emotion_by_dispute$Consumer.disputed. <- factor(emotion_by_dispute$Consumer.disputed.,
                                                levels = c("No", "Yes", "N/A"))


emotion_by_dispute |> dplyr:::arrange(desc(sentiment), desc(avg_score)) -> emotion_by_dispute

ggplot(emotion_by_dispute,
       aes(x = sentiment,
           y = avg_proportion,
           fill = Consumer.disputed.)) +
  geom_bar(position = "Stack", stat = "identity") +
  scale_fill_manual(values = c("No" = "#E74C3C",
                               "Yes" = "#3498DB"),
                    labels = c("No" = "Disputed", 
                               "Yes" = "Not Disputed")) +
  labs(title = "Emotional Content in Disputed vs. Non-Disputed Complaints",
       subtitle = "Comparison of normalized emotion scores",
       x = "Emotion",
       y = "Average Proportion of Words",
       fill = "Complaint Status") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60,
                                   hjust = 1,
                                   size = 15,
                                   face = "bold")) +

theme(axis.text.y = element_text(angle = 0,
                                   hjust = 1,
                                   size = 15,
                                   face = "bold")) 

# Statistical analysis - logistic regression
# Reshape data for logistic regression
complaints_tibble <- complaints_tibble %>%
  mutate(binary_dispute = case_when(
    Consumer.disputed. == "Yes" ~ 1,
    Consumer.disputed. == "No" ~ 0,
    .default = NA
  ))

emotion_wide <- complaints_tibble %>%
  count(Complaint.ID,binary_dispute, sentiment) %>%
  pivot_wider(
    id_cols = c(Complaint.ID, binary_dispute),
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

# Run logistic regression
table(emotion_wide$binary_dispute, useNA = "always")

dispute_model <- glm(binary_dispute ~ anger + fear + joy + sadness + trust + surprise + anticipation + disgust,
                     data = emotion_wide, family = "binomial")

# View model summary
summary(dispute_model)

## Validate model with Chi-Squared Test 

chisq_test <- anova(dispute_model, test = "Chisq")
print(chisq_test)




