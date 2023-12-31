library(tidyverse)
library(wordcloud)
theme_set(theme_bw())

# Load data
df <- read.csv("data/data_ia_ki.csv", sep = ";")
# Load overall data (ratio between articles with and without AI)
dat_all <- read.csv("data/daten_alle_artikel_ki.csv", sep = ";")

# 1. Amount of media coverage over the years 2016-2022

df_yearly <- df %>%
  group_by(Jahr) %>%
  summarise(Anzahl = n()) %>%
  arrange(Jahr)

plot_yearly <- ggplot(df_yearly,
                      aes(x = as.factor(Jahr), y = Anzahl)) +
  geom_bar(stat = "identity", aes(fill = Jahr)) +
  labs(title = "Berichterstattung über die Jahre",
       x = "Jahre") +
  theme(legend.position = "none")

plot_yearly

## Differences between magazines

df_by_magazine <- df %>%
  group_by(Zeitschrift) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_by_magazine <- rbind(df_by_magazine, c("Musik & Bildung", 0))
df_by_magazine <- rbind(df_by_magazine, c("Rondo", 0))
df_by_magazine$Anzahl <- as.numeric(df_by_magazine$Anzahl)

plot_by_magazine <- ggplot(df_by_magazine,
                           aes(x = reorder(Zeitschrift, -Anzahl), y = Anzahl)) +
  geom_bar(stat = "identity", aes(fill = Zeitschrift)) +
  coord_flip() +
  labs(title = "Berichterstattung nach Zeitschriften",
       x = "Zeitschrift") +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "none")

plot_by_magazine

# 2. AI as main or sub theme

df_main_vs_sub <- df %>%
  group_by(Haupt..Nebenthema..1...Haupt..2...Neben.) %>%
  summarise(Anzahl = n())

df_main_vs_sub

# 3. Specific AI topics

df_ki_topics <- df %>%
  select(KI.Thema..KI..Machine.Learning..Neuronale.Netzwerke.,
         KI.Thema.2, KI.Thema.3) %>%
  gather(key = "Kategorie", value = "Thema") %>%
  group_by(Thema) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_ki_topics

# 4. Overall topics

df_general_topic <- df %>%
  group_by(Allgemeines.Bezugsthema..Musikproduktion..Musikvertrieb..Forschung.) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_general_topic

wordcloud(words = df_general_topic$Allgemeines.Bezugsthema..Musikproduktion..Musikvertrieb..Forschung.,
          freq = df_general_topic$Anzahl, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# 5. People and organisations
## People

df_actors <- df %>%
  select(starts_with("Akteure")) %>%
  gather(key = "AkteurNum", value = "Akteur") %>%
  group_by(Akteur) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_actors

## Organisations
df_companies <- df %>%
  select(starts_with("Beteiligte.Unternehmen")) %>%
  gather(key = "UnternehmenNum", value = "Unternehmen") %>%
  group_by(Unternehmen) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_companies

# 6. Framing of articles
df_orientation <- df %>%
  group_by(Ausrichtung..KI.ablehnend..2..KI.kritisch..1..neutral.0..KI.optimistisch.1..KI.befürwortend.2.) %>%
  summarise(Anzahl = n())

df_orientation

plot_orientation <- ggplot(df_orientation, aes(x = Ausrichtung..KI.ablehnend..2..KI.kritisch..1..neutral.0..KI.optimistisch.1..KI.befürwortend.2.,
                                               y = Anzahl)) +
  geom_bar(stat = "identity") +
  labs(title = "Ausrichtung der Artikel",
       x = "Ablehnende bis befürwortende Ausrichtung",
       y = "Anzahl der Artikel")

plot_orientation

df$framing <- df$Ausrichtung..KI.ablehnend..2..KI.kritisch..1..neutral.0..KI.optimistisch.1..KI.befürwortend.2.

xtabs(~df$framing+df$Zeitschrift)

group_by(df, Zeitschrift) %>%
  summarise(
    mean = mean(framing, na.rm = TRUE),
    sd = sd(framing, na.rm = TRUE)
  )

# 7. TAM
## Usability
df_usability <- df %>%
  group_by(Benutzungsfreundlichkeit..TAM....1.nicht.freundlich..0.neutral...1.freundlich...nicht.gegeben.) %>%
  summarise(Anzahl = n())

df_usability

plot_usability <- ggplot(df_usability, aes(x = Benutzungsfreundlichkeit..TAM....1.nicht.freundlich..0.neutral...1.freundlich...nicht.gegeben., y = Anzahl)) +
  geom_bar(stat = "identity", fill = "brown3") +
  labs(title = "Benutzungsfreundlichkeit (TAM)",
       x = "Wahrgenommene Benutzerfreundlichkeit",
       y = "Anzahl der Artikel")

plot_usability

## Usefulness
df_usefulness <- df %>%
  group_by(Nutzen..TAM....1.nicht.nützlich..0.neutral...1.nützlich...nicht.gegeben.) %>%
  summarise(Anzahl = n())

df_usefulness

plot_usefulness <- ggplot(df_usefulness, aes(x = Nutzen..TAM....1.nicht.nützlich..0.neutral...1.nützlich...nicht.gegeben.,
                                             y = Anzahl)) +
  geom_bar(stat = "identity", fill = "cornsilk3") +
  labs(title = "Nutzen (TAM)",
       x = "Wahrgenommener Nutzen",
       y = "Anzahl der Artikel")

plot_usefulness

## Enjoyment
df_enjoyment <- df %>%
  group_by(Unterhaltungswert..TAM....1.nicht.unterhaltsam..0.neutral...1.unterhaltsam...nicht.gegeben.) %>%
  summarise(Anzahl = n())

df_enjoyment

plot_enjoyment <- ggplot(df_enjoyment, aes(x = Unterhaltungswert..TAM....1.nicht.unterhaltsam..0.neutral...1.unterhaltsam...nicht.gegeben.,
                                           y = Anzahl)) +
  geom_bar(stat = "identity", fill = "darksalmon") +
  labs(title = "Unterhaltungswert (TAM)",
            x = "Wahrgenommene Unterhaltung",
            y = "Anzahl der Artikel")

plot_enjoyment
