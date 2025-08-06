#read libraries
library(dplyr)
library(janitor)
library(ggplot2)

# read in data
df <- read.csv("participant_significance.csv")

df <- df %>%
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

# Function to compare a category between groups
compare_category <- function(df, column, category) {
  tab <- table(
    Group = df$Group,
    Category = df[[column]] == category
  )
  
  # Fisher's exact test
  test <- fisher.test(tab)
  
  # Output
  data.frame(
    Category = category,
    North = tab["North", "TRUE"],
    South = tab["South", "TRUE"],
    p_value = test$p.value,
    test = "Fisher"
  )
}

# Run tests for each discipline
disciplines <- unique(df$Discipline)
discipline_results <- do.call(rbind, lapply(disciplines, function(x) compare_category(df, "Discipline", x)))

# Run tests for each experience level
experiences <- unique(df$Experience)
experience_results <- do.call(rbind, lapply(experiences, function(x) compare_category(df, "Experience", x)))

discipline_results$FDR <- p.adjust(discipline_results$p_value, method = "fdr")
experience_results$FDR <- p.adjust(experience_results$p_value, method = "fdr")


print("Discipline comparisons:")
print(discipline_results)

print("Experience comparisons:")
print(experience_results)

#write results
write.csv(discipline_results, "disc_results.csv")
write.csv(experience_results, "exp_results.csv")



library(ggplot2)
library(dplyr)

# Summarize experience
experience_summary <- df %>%
  group_by(Group, Experience) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100)

# visualize experience
exp_pie <- ggplot(experience_summary, aes(x = "", y = perc, fill = Experience)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ Group) +
  theme_void() +
  labs(title = "Experience Distribution by Group",
       subtitle = "Significant difference (p < 0.05) in 1–3 and 10+ years") +
  geom_text(aes(label = paste0(round(perc), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Set3")

#save experience plot
ggsave("exp_sum.png", plot = exp_pie, dpi = 1200)


# Create discipline
discipline_summary <- df %>%
  group_by(Group, Discipline) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100)

#plot disciplines
disc_pie <- ggplot(discipline_summary, aes(x = "", y = perc, fill = Discipline)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ Group) +
  theme_void() +
  labs(title = "Discipline of Focus by Group",
       subtitle = "Significant difference (p < 0.01) in Nutrition representation") +
  geom_text(aes(label = paste0(round(perc), "%")), 
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Pastel1")

#save disciplines plot
ggsave("disc_sum.png", plot = disc_pie, dpi = 1200)
