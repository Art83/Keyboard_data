library(dplyr)

complete_df <- read.csv("F:/honours/raw_data/complete.csv", stringsAsFactors = F)





PHQ9 <- read.csv("D:/Computational_psych/csv_files/PHQ-9.csv", stringsAsFactors = F)


PHQ9_df <- PHQ9 %>%
  select(PIN, item, response) %>%
  filter(item != "last") %>% 
  tidyr::pivot_wider(names_from = "item", 
              values_from = "response") %>%
  mutate_at(vars(2:10), ~as.numeric(.)) %>%
  mutate(total_score = rowSums(select(., 2:10))) %>%
  mutate(PHQ_classification = ifelse(total_score < 10, "healthy", "sick")) %>%
  select(PIN, PHQ_classification) %>% 
  mutate(PHQ_classification = factor(PHQ_classification, levels = c("sick", "healthy")))


complete_df <- complete_df %>%
  mutate(stage = ifelse(stage == "RL1", "VVR1", stage)) %>% 
  filter(PIN %in% PHQ9_df$PIN)

saveRDS(PHQ9_df, "./objects/phq9_df")
saveRDS(complete_df, "./objects/complete_df")

