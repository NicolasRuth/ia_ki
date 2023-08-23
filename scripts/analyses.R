library(tidyverse)
library(wordcloud)
theme_set(theme_bw())

# Datensatz laden
df <- read.csv("data/data_ia_ki.csv", sep = ";")
# Überblicksdatensatz laden (Verhältnis Artikel mit und ohne KI-Bezug)
dat_all <- read.csv("data/daten_alle_artikel_ki.csv", sep = ";")

# 1. Häufigkeit der Berichterstattung
## Über die Jahre
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

## Unterschiede zwischen den Zeitschriften
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

# 2. Haupt- vs. Nebenthema
df_main_vs_sub <- df %>%
  group_by(Haupt..Nebenthema..1...Haupt..2...Neben.) %>%
  summarise(Anzahl = n())

df_main_vs_sub

# 4. Spezifische KI-Themen
df_ki_topics <- df %>%
  select(KI.Thema..KI..Machine.Learning..Neuronale.Netzwerke.,
         KI.Thema.2, KI.Thema.3) %>%
  gather(key = "Kategorie", value = "Thema") %>%
  group_by(Thema) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_ki_topics

# 5. Allgemeines Bezugsthema
df_general_topic <- df %>%
  group_by(Allgemeines.Bezugsthema..Musikproduktion..Musikvertrieb..Forschung.) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

wordcloud(words = df_general_topic$Allgemeines.Bezugsthema..Musikproduktion..Musikvertrieb..Forschung.,
          freq = df_general_topic$Anzahl, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# 6. Akteure und Unternehmen
## Akteure
df_actors <- df %>%
  select(starts_with("Akteure")) %>%
  gather(key = "AkteurNum", value = "Akteur") %>%
  group_by(Akteur) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_actors

## Unternehmen
df_companies <- df %>%
  select(starts_with("Beteiligte.Unternehmen")) %>%
  gather(key = "UnternehmenNum", value = "Unternehmen") %>%
  group_by(Unternehmen) %>%
  summarise(Anzahl = n()) %>%
  arrange(-Anzahl)

df_companies

# 7. Ausrichtung der Artikel
df_orientation <- df %>%
  group_by(Ausrichtung) %>%
  summarise(Anzahl = n())

df_orientation

plot_orientation <- ggplot(df_orientation, aes(x = Ausrichtung, y = Anzahl)) +
  geom_bar(stat = "identity") +
  labs(title = "Ausrichtung der Artikel")

# 8. TAM-Einschätzungen
## Benutzungsfreundlichkeit
df_usability <- df %>%
  group_by(Benutzungsfreundlichkeit) %>%
  summarise(Anzahl = n())

df_usability

plot_usability <- ggplot(df_usability, aes(x = Benutzungsfreundlichkeit, y = Anzahl)) +
  geom_bar(stat = "identity") +
  labs(title = "Benutzungsfreundlichkeit (TAM)")

## Nutzen
df_utility <- df %>%
  group_by(Nutzen) %>%
  summarise(Anzahl = n())

df_utility

plot_utility <- ggplot(df_utility, aes(x = Nutzen, y = Anzahl)) +
  geom_bar(stat = "identity") +
  labs(title = "Nutzen (TAM)")

## Unterhaltungswert
df_entertainment <- df %>%
  group_by(Unterhaltungswert) %>%
  summarise(Anzahl = n())

plot_entertainment <- ggplot(df_entertainment, aes(x = Unterhaltungswert, y = Anzahl)) +
  geom_bar(stat = "identity") +
  labs(title = "Unterhaltungswert (TAM)")
