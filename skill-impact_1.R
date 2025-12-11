#load libraries
library(tidyverse)

#read dataframe
df <- read.csv("skill-impact.csv")

df_long <- df %>%
  pivot_longer(
    cols = -c(Participant, Group),
    names_to = "Category",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response) & Response != "NA")

# chi-squared statistics
results <- df_long %>%
  group_by(Category) %>%
  summarise(
    trend_pval = tryCatch(
      CochranArmitageTest(table(Group, Response))$p.value,
      error = function(e) NA
    ),
    .groups = "drop"   # optional: ungroup after summarise
  ) %>%
  mutate(
    trend_pval_BH = p.adjust(trend_pval, method = "BH")
  )

write.csv(results, "skill_impact_significance.csv")

#scale north and south data
df_counts <- df_long %>%
  group_by(Group, Category, Response) %>%
  summarise(n = n(), .groups = "drop")

df_prop <- df_counts %>%
  group_by(Group, Category) %>%
  mutate(total = sum(n),
         proportion = n / total) %>%
  ungroup()

df_prop <- df_prop %>%
  mutate(GroupBinned = paste(Group, Response, sep = "_"))


custom_colors <- c(
  "North_Increased" = "#7B3085",
  "North_Not Impacted" = "#D093D8",
  "North_Decreased" = "#F6E9F7",
  "South_Increased" = "#36827F",
  "South_Not Impacted" = "#75C6C2",
  "South_Decreased" = "#D1ECEB"
)


#create plot
ggplot(df_prop, aes(x = interaction(Group, Category), y = proportion, fill = GroupBinned)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 0.8),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10, color = "black", family = "sans"),
    axis.text.y = element_text(size = 10, color = "black", family = "sans"),
    axis.title = element_text(face = "bold", size = 14, color = "black", family = "sans"),
    legend.position = "top",
    legend.text = element_text(size = 10, color = "black", family = "sans"),
    legend.title = element_text(size = 14, face = "bold", color = "black", family = "sans")
  ) +
  labs(x = "Category / Group", y = "Proportion of Participants", fill = "Response")


#print and save data
ggsave("skill_impact_highres.png", plot = skill_impact, width = 14, height = 8, dpi = 1200)
