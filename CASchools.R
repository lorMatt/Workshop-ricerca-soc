if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, ggplot2, readr, ggridges, AER, forcats, tidymodels)
library(AER)

# Import data ------------------------------------------------------------------
data('CASchools')
df <- CASchools
rm(CASchools)
# Esplorativa ----
## Univariata Y ----
df |> 
  ggplot(aes(read)) +
  geom_histogram() +
  theme_minimal()

## Bivariata school ----
df |> 
  ggplot(aes(school, read)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_blank())

## Scatter read ~ english ----
df |> 
  ggplot(aes(x = english, y = read)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

## Scatter read ~ income ----
df |> 
  ggplot(aes(x = income, y = read)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

## Modello lm due reg (read ~ english + income2) ----
df <- df |> 
  mutate(income2 = income^2)

### def reg
lm <- linear_reg() |> set_engine('lm')

### fit
lm_fit <- lm |> fit(read ~ english + income2, data = df)

### plot
lm_fit |> 
  extract_fit_engine() |> 
  summary()

# Comunicare i risultati ----
## escaping flatland
df |> 
  rename('Reddito medio' = income,
         'Competenze linguistiche' = read,
         `Percentuale di studenti\ndi inglese L2` = english) |>
  ggplot(aes(x = `Reddito medio`, y = `Competenze linguistiche`,
             colour = `Percentuale di studenti\ndi inglese L2`)) +
  geom_point() +
  geom_smooth(colour = 'black', linewidth = .4) +
  scale_color_viridis_c(option = 'inferno', direction = -1, ) +
  theme_minimal()

df |> 
  rename('Reddito medio' = income,
         'Competenze linguistiche' = read,
         `Percentuale di studenti di inglese L2` = english) |>
  ggplot(aes(colour = `Reddito medio`, y = `Competenze linguistiche`,
             x = `Percentuale di studenti di inglese L2`)) +
  geom_point() +
  geom_smooth(colour = 'black', linewidth = .4) +
  scale_color_viridis_c(option = 'inferno', direction = -1, ) +
  theme_minimal()

## Recode income ----
df |> 
  ggplot(aes(income)) +
  geom_histogram(binwidth = .7) +
  theme_minimal()

df <- df |> 
  mutate(incRec = case_when(income <= 10 ~ 'Basso',
                            income >10 & income <= 20 ~ 'Medio',
                            income >20 ~ 'Alto'),
         incRec = factor(incRec, levels = c('Basso', 'Medio', 'Alto')))

## Recode students ----
df |> 
  ggplot(aes(students)) +
  geom_histogram(binwidth = 150) +
  theme_minimal()

df <- df |> 
  mutate(studRec = case_when(students <= 250 ~ 'Piccole',
                            students >250 & students <= 750 ~ 'Medie',
                            students >750 ~ 'Grandi'),
         studRec = factor(studRec, levels = c('Piccole', 'Medie', 'Grandi')))

## ridgeline ----
df |> 
  rename('Reddito medio' = incRec,
         'Competenze linguistiche' = read) |>
  ggplot(aes(`Competenze linguistiche`, `Reddito medio`, fill = `Reddito medio`)) +
  geom_density_ridges(scale = 1.5, rel_min_height = 0.0) +
  scale_fill_viridis_d(option = 'mako', direction = -1) +
  theme_ridges() +
  theme(legend.position = 'none')

## barplot ----
df |> 
  ggplot(aes(incRec, fill = studRec)) +
  coord_flip() +
  geom_bar(position = 'fill', width = .8) +
  scale_fill_viridis_d(option = 'mako', direction = -1) +
  labs(legend.title = 'School size') +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave(height = 2, width = 6, filename = 'img/barplot.pdf')

