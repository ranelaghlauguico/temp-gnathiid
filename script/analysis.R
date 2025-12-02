library(tidyverse)
library(ggplot2)
library(forcats)
library(knitr)
library(broom)

growth <- read.csv("data/Lifecycle_Growth.csv")
glimpse(growth)

stage_order <- c("ZA", "ZB", "ZC", "PA", "PB", "PC")

growth_clean <- growth |>
  mutate(
    stage = factor(Stage, levels = stage_order, ordered = TRUE),
    Temp_C = factor(Temp_C),
    size_ratio = Body_length_mm / Headwidth_mm
  ) |>
  filter(
    !is.na(Body_length_mm),
    !is.na(Headwidth_mm),
    !is.na(stage)
  )

glimpse(growth_clean)


summary_table <- growth_clean |>
  group_by(Temp_C, Stage) |>
  summarise(
    n          = n(),
    mean_body  = mean(Body_length_mm, na.rm = TRUE),
    sd_body    = sd(Body_length_mm,   na.rm = TRUE),
    mean_head  = mean(Headwidth_mm,   na.rm = TRUE),
    sd_head    = sd(Headwidth_mm,     na.rm = TRUE),
    mean_ratio = mean(size_ratio,     na.rm = TRUE),
    .groups = "drop"
  )



p_body_box <- ggplot(growth_clean,
                     aes(x = Stage, y = Body_length_mm, fill = Temp_C)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  labs(
    x = "Developmental stage",
    y = "Body length (mm)",
    fill = "Temp (°C)",
    title = "Body Length Across Stages and Temperatures"
  )
p_body_box





early_mean <- early |>
  group_by(Stage) |>
  summarise(
    mean_body = mean(Body_length_mm, na.rm = TRUE),
    sd_body   = sd(Body_length_mm, na.rm = TRUE))

early_mean



ggplot(early_mean, aes(x = Stage, y = mean_body, fill = Stage)) +
  geom_col(alpha = 0.8) +
  labs(
    title = "Mean Body Length (ZA & ZB)",
    x = "Stage",
    y = "Mean body length (mm)"
  )


early_mean_temp <- early |>
  group_by(Stage, Temp_C) |>
  summarise(
    mean_body = mean(Body_length_mm, na.rm = TRUE),
    sd_body   = sd(Body_length_mm, na.rm = TRUE),
    n         = n(),
    se_body   = sd_body / sqrt(n),
    .groups = "drop"
  )

early_mean_temp


ggplot(early_mean_temp,
       aes(x = Stage, y = mean_body, fill = Temp_C)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_body - se_body,
        ymax = mean_body + se_body),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    title = "Mean Body Length ± SE by Temperature",
    x = "Stage",
    y = "Mean body length (mm)",
    fill = "Temp (°C)"
  )


fed_gnathiids <- read.csv("data/Fed_gnathiids.csv")






library(tidyverse)
library(ggplot2)


fed <- read.csv("data/Fed_gnathiids.csv") |>
  mutate(
    Stage  = factor(Stage, levels = c("PA", "PB"), ordered = TRUE),
    Temp_C = factor(Temp_C)
  )

ggplot(fed, aes(x = Stage, y = Stomach_width_mm, fill = Temp_C)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  labs(
    title = "Stomach Width of Fed Gnathiids by Stage and Temperature",
    x = "Stage (fed)",
    y = "Stomach width (mm)",
    fill = "Temp (°C)"
  )

fed <- fed |>
  mutate(fullness = Stomach_width_mm / Body_length_mm)

fullness_summary <- fed |>
  group_by(Stage, Temp_C) |>
  summarise(
    mean_fullness = mean(fullness, na.rm = TRUE),
    sd_fullness   = sd(fullness, na.rm = TRUE),
    n             = n(),
    se_fullness   = sd_fullness / sqrt(n),
    .groups = "drop"
  )


ggplot(fullness_summary,
       aes(x = Stage, y = mean_fullness, fill = Temp_C)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_fullness - se_fullness,
        ymax = mean_fullness + se_fullness),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    title = "Relative Stomach Fullness by Stage and Temperature",
    x = "Stage (fed)",
    y = "Mean stomach width / body length",
    fill = "Temp (°C)"
  )

ggplot(fed, aes(x = Body_length_mm,
                y = Stomach_width_mm,
                colour = Stage)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Relationship Between Body Size and Stomach Width in Fed Gnathiids",
    x = "Body length (mm)",
    y = "Stomach width (mm)",
    colour = "Stage"
  )





