# Load libraries
library(haven)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)      # needed for unnest()


# Load Data
mret_path <- "C:/Users/Davis/OneDrive/Desktop/mret7023.sas7bdat"
names_path <- "C:/Users/Davis/OneDrive/Desktop/Names.xlsx"

mret <- read_sas(mret_path)
names_data <- read_excel(names_path)

# Uppercase surname list
surname_list <- toupper(names_data$surname)

# Uppercase company names
mret <- mret %>%
  mutate(company_name = toupper(COMNAM))


# Tokenize company names
mret <- mret %>%
  mutate(tokens = str_split(company_name, "\\W+"))   # split on non-letters


# Flag whether company contains a surname
mret <- mret %>%
  mutate(
    NameMatch = sapply(tokens, function(words) {
      any(words %in% surname_list)
    }) %>% as.integer()
  )

# Summary statistics
match_rate <- mean(mret$NameMatch, na.rm = TRUE)
cat("\nPercentage of companies with surname-like names:",
    round(match_rate * 100, 2), "%\n")

summary_stats <- mret %>%
  group_by(NameMatch) %>%
  summarise(
    avg_return = mean(RET, na.rm = TRUE),
    median_return = median(RET, na.rm = TRUE),
    n = n()
  )

print(summary_stats)

# Boxplot
ggplot(mret, aes(x = as.factor(NameMatch), y = RET,
                 fill = as.factor(NameMatch))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightgreen"),
                    name="Name Match",
                    labels=c("No","Yes")) +
  labs(title="Return Distribution: Name Match vs Non-Match",
       x="Name Match", y="Return") +
  theme_minimal(base_size = 13)

# Regression
model1 <- lm(RET ~ NameMatch, data = mret)
summary(model1)

# Optional controls
if(all(c("SIZE", "BM") %in% names(mret))) {
  model2 <- lm(RET ~ NameMatch + SIZE + BM, data = mret)
  summary(model2)
}

# Surname frequency in company names
surname_frequency <- mret %>%
  unnest(tokens) %>%                        # expand token list
  filter(tokens != "", tokens %in% surname_list) %>% 
  group_by(tokens) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

top_surnames <- head(surname_frequency, 20)
cat("\nTop 20 surnames appearing in company names:\n")
print(top_surnames)

