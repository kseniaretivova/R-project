# Загрузка пакетов
library(readxl)
library(tidyverse)
library(psych)

# Импорт данных
killers <- read_excel("C:\\Users\\1610043\\Desktop\\SKdatabase.xlsx") %>%
  as_tibble()

# Препроцессинг
killers_final <- killers %>%
  # 1. Удаляем некорректный столбец
  select(-`Birthdate`) %>%
  # 2. Корректируем конкретные строки
  mutate(
    `Zodiac sign` = case_when(
      row_number() == 3 ~ "Aquarius",
      row_number() == 37 ~ "Libra",
      row_number() == 47 ~ "Leo",
      TRUE ~ `Zodiac sign`
    )
  ) %>%
  # 3. Заменяем "Unknown" и "U" на NA
  mutate(
    Gender = ifelse(Gender == "U", NA_character_, Gender),
    `Zodiac sign` = ifelse(`Zodiac sign` == "Unknown", NA_character_, `Zodiac sign`)
  ) %>%
  # 4. Добавляем столбцы с информацией о стихии и модальности
  mutate(
    Triplicity = case_when(
      `Zodiac sign` %in% c("Aries", "Leo", "Sagittarius") ~ "Fire",
      `Zodiac sign` %in% c("Taurus", "Virgo", "Capricorn") ~ "Earth",
      `Zodiac sign` %in% c("Gemini", "Libra", "Aquarius") ~ "Air",
      `Zodiac sign` %in% c("Cancer", "Scorpio", "Pisces") ~ "Water",
      TRUE ~ NA_character_
    ),
    Modality = case_when(
      `Zodiac sign` %in% c("Aries", "Cancer", "Libra", "Capricorn") ~ "Cardinal",
      `Zodiac sign` %in% c("Leo", "Scorpio", "Aquarius", "Taurus") ~ "Fixed",
      `Zodiac sign` %in% c("Sagittarius", "Pisces", "Gemini", "Virgo") ~ "Mutable",
      TRUE ~ NA_character_
    )
  ) %>%
  # 5. Очистка от NA
  filter(
    !is.na(`Zodiac sign`),
    !is.na(Triplicity),
    !is.na(Modality)
  )

# Создаём график
sunburst_data <- killers_final %>%
  group_by(Triplicity, `Zodiac sign`) %>%
  summarise(
    count_killers = n(),
    total_victims = sum(`Proven victims`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(factor(Triplicity, levels = c("Fire", "Earth", "Air", "Water"))) %>%
  mutate(
    angle = 360 * cumsum(count_killers) / sum(count_killers),
    label_angle = angle - 180 * count_killers / sum(count_killers)
  
  )
# Цвета
zodiac_colors <- c(
  "Aries" = "#CD5C5C",
  "Taurus" ="#a16e33",
  "Gemini" = "#274754",
  "Cancer" = "#41695d",
  "Leo" = "#a84040",
  "Virgo" = "#c99355",
  "Libra" = "#3a545e",
  "Scorpio" = "#638a7e",
  "Sagittarius" = "#995050",
  "Capricorn" = "#946837",
  "Aquarius" = "#607e8a",
  "Pisces" = "#275447"
)

ggplot(sunburst_data, aes(
  x = angle,
  y = total_victims,
  fill = `Zodiac sign`
)) +
  geom_rect(
    aes(xmin = lag(angle, default = 0), xmax = angle, ymin = 0, ymax = total_victims),
    linewidth = 0.8
  ) +
  # Цифры внутри секторов
  geom_text(
    aes(
      x = label_angle,
      y = total_victims / 2,
      label = count_killers
    ),
    colour = "white",
    size = 5,
    fontface = "bold"
  ) +
  coord_polar(theta = "x", start = pi/2, direction = 1) +
  scale_fill_manual(values = zodiac_colors) +
  # Убираем центр (делаем "бублик")
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.05))) +
  theme_void() +
  labs(
    title = "Serial Killers (by Zodiac sign)"
  ) +
  guides(fill = guide_legend(
    title = "Zodiac signs",
    position = "right"
  )) +
  theme(
    plot.title = element_text(
      hjust = 0.8,
      vjust = -6,
      size = 35,
      face = "bold",        
      family = "Times New Roman"
    ),
    # Легенда
    legend.background = element_rect(
      fill = "white"
    ),
    legend.key.size = unit(0.75, "cm"),
    legend.text = element_text(
      size = 15,
      family = "Times New Roman"
    ),
    legend.title = element_text(
      size = 19,
      face = "bold",
      family = "Times New Roman"
    )
  )

# Описательная статистика
killers_final %>%
  group_by(`Zodiac sign`) %>%
  summarise(describe(`Proven victims`))

killers_final %>%
  group_by(`Modality`) %>%
  summarise(describe(`Proven victims`))