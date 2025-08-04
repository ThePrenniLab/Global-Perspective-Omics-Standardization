#load libraries
library(tidyverse)


# Read data
df <- read_csv("analytical-platforms.csv")

names(df)[3:12] <- c(
  "plans_sampling", "started_sampling",
  "plans_metabolomics", "started_metabolomics",
  "plans_fames", "started_fames",
  "plans_lipidomics", "started_lipidomics",
  "plans_ionomics", "started_ionomics"
)

df_long <- df %>%
  pivot_longer(cols = starts_with("plans_") | starts_with("started_"),
               names_to = c("stage", "method"),
               names_sep = "_",
               values_to = "response") %>%
  filter(response == "Yes") %>%
  group_by(Group, method, stage) %>%
  summarise(count = n(), .groups = "drop")

ggplot(df_long, aes(x = method, y = count, fill = stage)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Group) +
  labs(
    title = "PTFI Method Implementation and Onboarding by Group",
    x = "Method",
    y = "Number of Individuals",
    fill = "Stage"
  ) +
  theme_minimal()

