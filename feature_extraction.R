
complete_df <- readRDS("./objects/complete_df")

overall_df <- complete_df %>%
  filter(stage %in% c("key_testing", "FHQ1", "VVR1", "pav_con", "transfer1", "deval_test", "FHQ2",
                      "recall", "transfer_q")) %>% 
  filter(event_type == "key press" | event_type == "key release") %>% 
  filter(!grepl('mouse', event_converted)) %>% 
  group_by(PIN, stage) %>% 
  mutate(
    key_hold_time = ifelse(event_type == "key release", timestamp - lag(timestamp), NA),
    flight_time = ifelse(event_type == "key press", timestamp - lag(timestamp, n = 2), NA)
  )

overall_summary <- overall_df %>%
  group_by(PIN) %>%
  summarize(kht_median = median(key_hold_time, na.rm = T),
            kht_q25 = quantile(key_hold_time, 0.25, na.rm = T),
            kht_q75 = quantile(key_hold_time, 0.75, na.rm = T),
            kht_mx = max(key_hold_time, na.rm = T),
            kht_mn = min(key_hold_time, na.rm = T),
            ft_median = median(flight_time, na.rm = T),
            ft_q25 = quantile(flight_time, 0.25, na.rm = T),
            ft_q75 = quantile(flight_time, 0.75, na.rm = T),
            ft_mx = max(flight_time, na.rm = T),
            ft_mn = min(flight_time, na.rm = T)
            )

# Relevant vs nonrelevant key presses.


# Stage by Stage metrics


