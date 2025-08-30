library(tidyverse)
library(metill)
library(geomtextpath)

offense <- results$summary("offense")
defense <- results$summary("defense")

plot_dat <- offense |>
  mutate(
    round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
    team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
  ) |>
  select(round, team_nr, median, q5, q95) |>
  inner_join(
    teams
  ) |>
  mutate(
    variable = "Sóknarstyrkur"
  ) |>
  bind_rows(
    defense |>
      mutate(
        round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
        team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
      ) |>
      select(round, team_nr, median, q5, q95) |>
      inner_join(
        teams
      ) |>
      mutate(
        variable = "Varnarstyrkur"
      )
  )



plot_dat |>
  inner_join(
    d |>
      pivot_longer(c(home, away)) |>
      select(
        season,
        game_nr,
        date,
        name,
        value
      ) |>
      mutate(
        round = row_number(),
        .by = value
      ) |>
      select(
        season,
        round,
        team = value,
        date
      )
  ) |>
  filter(
    team %in% c(
      "Iceland", "Belgium"
    )
  ) |>
  mutate(
    team = case_when(
      team == "Iceland" ~ "Ísland",
      team == "Belgium" ~ "Belgía",
      TRUE ~ team
    )
  ) |> 
  ggplot(aes(date, median)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_textline(
    aes(
      group = team,
      label = team,
      col = team
    ),
    linewidth = 1,
    size = 5
  ) +
  geom_richtext(
    data = tibble(x = 1),
    inherit.aes = FALSE,
    x = clock::date_build(2016, 1, 1),
    y = -1,
    label.colour = NA,
    fill = NA,
    label = "&larr; Undir meðaltali",
    hjust = 1,
    vjust = 0,
    angle = 90,
    size = 3.5,
    colour = "grey40"
  ) +
  geom_richtext(
    data = tibble(x = 1),
    inherit.aes = FALSE,
    x = clock::date_build(2016, 1, 1),
    y = 1,
    label.colour = NA,
    fill = NA,
    label = "Yfir meðaltali &rarr;",
    hjust = 0, 
    vjust = 0,
    angle = 90,
    size = 3.5,
    colour = "grey40"
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both"),
    breaks = breaks_width("2 year"),
    labels = label_date_short(),
    limits = clock::date_build(c(2016, 2026), 1, 1)
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    limits = 20 * c(-1, 1)
  ) +
  scale_colour_manual(
    values = c(
      "Ísland" = "#02529C",
      "Belgía" = "#2D2926"
    )
  ) +
  facet_wrap(
    vars(variable),
    ncol = 1
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Þróun styrks landsliða Íslands og Belgíu í Körfubolta karla",
    subtitle = "Íslenska karlalandsliðið hefur bætt sig í bæði sóknar- og varnarstyrk síðustu árin",
    x = NULL,
    y = "Samanburður við lönd sem hafa spilað á EM eða HM",
    col = NULL,
    fill = NULL
  )

ggsave(
  filename = here("results", "male", "evolution_iceland_belgium.png"),
  width = 8,
  height = 0.8 * 8,
  scale = 1.2
)
