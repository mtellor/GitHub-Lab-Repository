# Set-up ----
knitr::opts_chunk$set(eval = FALSE, include  = TRUE)

## Loading packages
install.packages("tidytext")
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


## Loading data
mt_samples <- read_csv("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv")
mt_samples <- mt_samples |>
  select(description, medical_specialty, transcription)

head(mt_samples)


# Question 1 ----

mt_samples %>%
  count(medical_specialty, sort = TRUE) %>%
  print(n = 40)


# Question 2 ----

mt_samples %>% 
  unnest_tokens(word,transcription) %>% 
  count(word,sort=TRUE) %>% 
  top_n(20,n) %>% 
  ggplot(aes(x=reorder(word,n),y=n))+
  geom_col()+
  coord_flip()+
  labs(x="Word",y="Count",title="Top 20 Most Frequent Words")

mt_samples %>% 
  unnest_tokens(word,transcription) %>% 
  count(word,sort=TRUE) %>% 
  top_n(20,n) %>% 
  print ()


# Question 3 ----

mt_samples %>%
  unnest_tokens(word, transcription) %>%
  anti_join(stop_words %>% filter(!word %in% c("right", "left")), by = "word") %>%
  filter(!grepl("^[0-9]+$", word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  ggplot(aes(x=reorder(word, n), y =n)) +
  geom_col() +
  coord_flip() +
  labs(x="Words", y="Count", title="Top 20 Words (Excluding Stop Words & Numbers)")

mt_samples %>%
  unnest_tokens(word, transcription) %>%
  anti_join(stop_words %>% filter(!word %in% c("right", "left")), by = "word") %>%
  filter(!grepl("^[0-9]+$", word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  print ()


# Question 4 ----

## bi-gram
mt_samples %>%
  unnest_tokens(bigram, transcription, token = "ngrams", n=2) %>%
  count(bigram ,sort = TRUE) %>%
  top_n(20,n)

## tri-gram
mt_samples %>%
  unnest_tokens(trigram, transcription, token = "ngrams", n=3) %>%
  count(trigram ,sort = TRUE) %>%
  top_n(20,n)


# Question 5 ----

mt_samples %>%
  unnest_tokens(trigram, transcription, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(word2 == "incision") %>%
  count(word1, word2, word3, sort = TRUE)


# Question 6 ----

mt_samples %>% 
  unnest_tokens(word,transcription) %>% 
  anti_join(stop_words,by="word") %>% 
  filter(!grepl("^[0-9]+$",word)) %>% 
  group_by(medical_specialty) %>% 
  count(word,sort=TRUE) %>% 
  top_n(5, n) %>% 
  arrange(medical_specialty,desc(n))

# Question 7 ----

## Admitting dx: Neonatal
mt_samples %>%
  filter(medical_specialty == "Pediatrics - Neonatal") %>%
  filter(grepl("ADMITTING DIAG", transcription, ignore.case = TRUE)) %>%
  print()

## Preop dx: Neonatal
mt_samples %>%
  filter(medical_specialty == "Pediatrics - Neonatal") %>%
  filter(grepl("PREOPERATIVE DIAG", transcription, ignore.case = TRUE)) %>%
  print()

## Preop dx: Post-op post-partum: OB
mt_samples %>%
  filter(medical_specialty == "Obstetrics / Gynecology") %>%
  filter(grepl("PREOPERATIVE DIAG", transcription, ignore.case = TRUE)) %>%
  filter(grepl("postpartum", description, ignore.case = TRUE)) %>%
  print()


## Prepop dx for endoscopic procedures: Neurosurg
mt_samples %>%
  filter(medical_specialty == "Neurosurgery") %>%
  filter(grepl("PREOPERATIVE DIAG", transcription, ignore.case = TRUE)) %>%
  filter(grepl("endoscopic", description, ignore.case = TRUE)) %>%
  print()

## HPI for Hospice pts
mt_samples %>%
  filter(medical_specialty == "Hospice - Palliative Care") %>%
  filter(grepl("HISTORY", transcription, ignore.case = TRUE)) %>%
  filter(!grepl("REASON FOR", transcription, ignore.case = TRUE)) %>%
  print()

## Cardiac arrest

arrest_cases <- c(
  "Urgent cardiac catheterization with coronary angiogra.",
  "Mediastinal exploration and delayed primary chest closure. The patient is a 12-day-old infant who has undergone a modified stage I Norwood procedure with a Sano modification",
  "Left hip fracture. The patient is a 53-year-old female with probable pathological fracture of the left proximal femur",
  "Patient with a previous history of working in the coalmine and significant exposure to silica with resultant pneumoconiosis and fibrosis of the lung",
  "History of diabetes, osteoarthritis, atrial fibrillation, hypertension, asthma, obstructive sleep apnea on CPAP, diabetic foot ulcer, anemia, and left lower extremity cellulitis",
  "Cardiac arrest, severe congestive heart failure, acute on chronic respiratory failure, osteoporosis, and depression"
)

excluded_specialties <- c(
  "Pediatrics - Neonatal",
  "Emergency Room Reports",
  "Consult - History and Phy",
  "Cardiovascular / Pulmonary"
)

pattern <- paste(arrest_cases, collapse = "|")

filtered_data <- mt_samples %>%
  filter(!medical_specialty %in% excluded_specialties) %>%
  filter(grepl(pattern, description, ignore.case = TRUE)) %>%
  select(description, medical_specialty, transcription)

filtered_data





