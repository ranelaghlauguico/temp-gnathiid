
library(tidyverse)
library(patchwork)
library(ggplot2)

unfed <- read_csv("data/Lifecycle_Growth.csv") |>
  filter(Stage %in% c("ZA", "ZB")) |>
  mutate(
    Stage  = factor(Stage, levels = c("ZA", "ZB"), ordered = TRUE),
    Temp_C = factor(Temp_C)
  )

fed <- read_csv("data/Fed_gnathiids.csv") |>
  filter(Stage %in% c("PA", "PB")) |>
  mutate(
    Stage  = factor(Stage, levels = c("PA", "PB"), ordered = TRUE),
    Temp_C = factor(Temp_C)
  )


p_unfed <- ggplot(unfed,
                  aes(x = Stage, y = Body_length_mm, fill = Temp_C)) +
  geom_boxplot() +
  labs(x = "Stage",
       y = "Body length (mm)",
       fill = "Temperature (°C)"
  )


p_fed <- ggplot(fed,
                aes(x = Stage, y = Body_length_mm, fill = Temp_C)) +
  geom_boxplot() +
  labs(x = "Stage",
       y = "Body length (mm)",
       fill = "Temperature (°C)"
  )

p_unfed + p_fed



ggplot(fed,
       aes(x = Body_length_mm,
           y = Stomach_width_mm,
           colour = Stage)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(x = "Body length (mm)",
    y = "Stomach width (mm)",
    colour = "Stage"
  )