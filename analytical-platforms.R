library(tidyverse)
library(viridis)  

# Load and clean data
df <- read.csv("analytical-platforms.csv") 

df_clean <- df %>%
  rename(
    Group = Group,
    Metabolomics = `Met`,
    FAMEs = `FAM`,
    Lipidomics = `LIP`,
    Ionomics = `ION`,
    FoodSampling = `FS`,
  )

methods <- c("Metabolomics", "FAMEs", "Lipidomics", "Ionomics", "FoodSampling")

raw_pvals <- c()
tests <- c()

for (method in methods) {
  contingency <- table(df_clean$Group, df_clean[[method]])
  if (any(contingency < 5)) {
    test_result <- fisher.test(contingency)
    tests <- c(tests, "Fisher")
  } else {
    test_result <- chisq.test(contingency)
    tests <- c(tests, "Chi-squared")
  }
  raw_pvals <- c(raw_pvals, test_result$p.value)
}

# FDR correction
adjusted_pvals <- p.adjust(raw_pvals, method = "BH")

# Combine results into a tidy data frame
stats_df <- tibble(
  Method = methods,
  Raw_p = raw_pvals,
  FDR_p = adjusted_pvals,
  Test = tests,
  Significance = case_when(
    FDR_p < 0.001 ~ "***",
    FDR_p < 0.01 ~ "**",
    FDR_p < 0.05 ~ "*",
    TRUE ~ "ns"
  )
)

print(stats_df)

# Convert wide to long format
df_long <- df_clean %>%
  pivot_longer(cols = FoodSampling:Ionomics, names_to = "Method", values_to = "Response") %>%
  filter(Response == "Yes") %>%
  group_by(Group, Method) %>%
  summarise(Yes_Count = n(), .groups = "drop")

# Get total participants per group to calculate proportions
group_totals <- df_clean %>%
  group_by(Group) %>%
  summarise(Total = n())

# Merge and calculate proportions
df_plot <- df_long %>%
  left_join(group_totals, by = "Group") %>%
  mutate(Proportion = Yes_Count / Total)


# Merge with heatmap data
df_annotated <- df_plot %>%
  left_join(stats_df %>% select(Method, Significance), by = "Method") %>%
  mutate(
    label = paste0(scales::percent(Proportion, accuracy = 1), "\n", Significance)
  )

# Plot with viridis scale
heatmap_plot <- ggplot(df_annotated, aes(x = Method, y = Group, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Proportion", limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = label), color = "black", size = 4) +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(title = "Proportion of Labs Onboarding Each Method by Region\n(* = FDR-adjusted p < 0.05)")

# Show plot
print(heatmap_plot)

# Save with custom dimensions (in inches)
ggsave("onboarding_heatmap.pdf", plot = heatmap_plot, width = 6, height = 4, dpi = 900)



