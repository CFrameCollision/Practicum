library(tidyverse)

dataDeaths <- read.csv(file = "data/Overdose_Downloadable_Deaths.csv")
dataPrescrips <- read.csv(
  file = "data/Overdose_Downloadable_Prescriptions_DemographicsAge.csv"
)

#### Deaths ####
ageDeaths <- dataDeaths %>%
  filter(
    grepl("Any Opioid", Drug.Category, fixed = TRUE),
    Demographic.Category == "Age",
    Geography == "Statewide"
  )

# Change all suppressed (*) values to 5. Suppressed values can be between 1 and 10
ageDeaths$Death.Count[ageDeaths$Death.Count == "*"] %>% table()

ageDeaths$Death.Count[ageDeaths$Death.Count == "*"] <- 5

ageDeathAmount <- ageDeaths %>%
  mutate(Death.Count = as.numeric(Death.Count)) %>%
  group_by(Demographic) %>%
  summarise(
    totalDeaths = sum(Death.Count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(ageDeathAmount, aes(x = Demographic, y = totalDeaths)) +
  geom_col() +
  ylab("Total Deaths") +
  xlab("Age") +
  geom_text(
    aes(
      label = totalDeaths
    ),
    position = position_dodge(width = 1),
    color = "black",
    size = 4,
    vjust = -0.2
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#### Prescriptions #####
agePrescrips <- dataPrescrips %>%
  filter(Geography == "Statewide", Age.Group != "< 18", Age.Group != "18+")

# Change all suppressed (*) values to 5. Suppressed values can be between 1 and 10
agePrescrips$Count[agePrescrips$Count == "*" | is.na(agePrescrips$Count)] %>%
  table(useNA = "always")

agePrescrips$Count[agePrescrips$Count == "*"] <- 5
agePrescrips$Count[is.na(agePrescrips$Count)] <- 0

agePresAmount <- agePrescrips %>%
  group_by(Age.Group) %>%
  summarise(totalPrescrips = sum(Count), .groups = "drop")

ggplot(agePresAmount, aes(x = Age.Group, y = totalPrescrips)) +
  geom_col() +
  ylab("Total Prescriptions") +
  xlab("Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
