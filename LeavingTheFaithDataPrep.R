#Libraries
library(readxl)
library(tidyr)
library(dplyr)

#Data Source 
#How we let go (of faith beliefs)
#https://howweletgo.org/
#https://docs.google.com/spreadsheets/d/11b8jconbn-eNEo4GapuiTimaLSeooj4_eA1QR2jT7nI/edit#gid=0 

#Load data
data <- read_xlsx("Letting go of Beliefs_NewVariableNames.xlsx")

#Filter for just those raised in Christianity and no longer believe in a god
data <- data %>% filter(religion_raised == "Christian" & current_believe == "No")

#New Variable for supernatural experiences
data <- data %>%  mutate(supernatural_experience2 = case_when(
  grepl("No", supernatural_experience) ~ "No",
  grepl("Yes", supernatural_experience) ~ "Yes",
  T ~ NA_character_))

#Compare by supernatural_experience2
data %>% count(supernatural_experience2)

#mental_issues
data <- data %>% mutate(mental_issues2 = case_when(
  grepl("No", mental_issues) ~ "No",
  grepl("Yes", mental_issues) ~ "Yes",
  T ~ NA_character_))

data %>% group_by(supernatural_experience2) %>% count(mental_issues2)

#stop_questioning
data <- data %>% mutate(stop_questioning2 = case_when(
  grepl("No", stop_questioning) ~ "No",
  grepl("Yes", stop_questioning) ~ "Yes",
  T ~ NA_character_))

data %>% group_by(supernatural_experience2) %>% count(stop_questioning2)

#personality
data <- data %>% mutate(personality2 = case_when(
  grepl("No", personality) ~ "No",
  grepl("Yes", personality) ~ "Yes",
  T ~ NA_character_))

data %>% group_by(supernatural_experience2) %>% count(personality2)

#ideas
data <- data %>% mutate(ideas2 = case_when(
  grepl("No", ideas) ~ "No",
  grepl("Yes", ideas) ~ "Yes",
  T ~ NA_character_))

data %>% group_by(supernatural_experience2) %>% count(ideas2)

write.csv(data, "LeavingTheFaith.csv")
