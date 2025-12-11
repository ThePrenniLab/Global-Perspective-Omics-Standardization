# ---- Libraries ----
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

# ---- Read and clean data ----
df <- read.csv("participant_significance.csv") %>%
  mutate(
    Discipline = trimws(tolower(Discipline.of.Focus..select.the.most.relevant.choice.)),
    Experience = trimws(tolower(Experience.in.field.of.study)),
    Group = Global.group
  )

df$Discipline <- recode(df$Discipline,
                        "food science and nut" = "food science and technology",
                        "food science and technology " = "food science and technology",
                        "environmental scien" = "environmental science",
                        "analytical chemistry " = "analytical chemistry"
)

df$Experience <- recode(df$Experience,
                        "5-10 yea" = "5-10 years",
                        "10+ years " = "10+ years"
)

# ---- Function to compare category by group (binary) ----
compare_category <- function(df, column, category) {
  tab <- table(df$Group, df[[column]] == category)
  
  # Fisher's exact test
  test <- fisher.test(tab)
  
  # Compute proportions for each group
  prop_table <- prop.table(tab, margin = 1)[, "TRUE"]
  
  # Output
  data.frame(
    Category = category,
    North_n = tab["North", "TRUE"],
    South_n = tab["South", "TRUE"],
    North_prop = prop_table["North"],
    South_prop = prop_table["South"],
    p_value = test$p.value,
    test = "Fisher"
  )
}

# ---- Run tests for disciplines ----
disciplines <- unique(df$Discipline)
discipline_results <- bind_rows(lapply(disciplines, function(x) compare_category(df, "Discipline", x)))
discipline_results$FDR <- p.adjust(discipline_results$p_value, method = "fdr")

# ---- Run tests for experience ----
experiences <- unique(df$Experience)
experience_results <- bind_rows(lapply(experiences, function(x) compare_category(df, "Experience", x)))
experience_results$FDR <- p.adjust(experience_results$p_value, method = "fdr")

# ---- Save results ----
write.csv(discipline_results, "disc_results_proportions.csv", row.names = FALSE)
write.csv(experience_results, "exp_results_proportions.csv", row.names = FALSE)

# ---- Visualize Experience (percentages) ----
experience_summary <- df %>%
  group_by(Group, Experience) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(perc = n / sum(n) * 100)

exp_pie <- ggplot(experience_summary, aes(x = "", y = perc, fill = Experience)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ Group) +
  theme_void() +
  labs(title = "Experience Distribution by Group") +
  geom_text(aes(label = paste0(round(perc), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set3")

ggsave("exp_sum_proportions.png", plot = exp_pie, dpi = 1200)

# ---- Visualize Discipline (percentages) ----
discipline_summary <- df %>%
  group_by(Group, Discipline) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(perc = n / sum(n) * 100)

disc_pie <- ggplot(discipline_summary, aes(x = "", y = perc, fill = Discipline)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ Group) +
  theme_void() +
  labs(title = "Discipline of Focus by Group") +
  geom_text(aes(label = paste0(round(perc), "%")), position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Pastel1")

ggsave("disc_sum_proportions.png", plot = disc_pie, dpi = 1200)