# Paris participatory budget: one row per winning project
# Source: Ville de Paris, "Budget participatif : opérations des projets
# gagnants et réalisations", opendata.paris.fr, licence ODbL.
# Exploration: scratch/data-candidates/02-budget-participatif.qmd

library(tidyverse)

snapshot_date <- as.Date("2026-11-08") # day the data was downloaded

bp_raw <- read_delim(
  here::here("data/budget-participatif.csv"),
  delim = ";",
  col_types = cols(.default = "c")
)

themes <- c(
  "Attractivité et emploi" = "economy",
  "Cadre de vie" = "living environment",
  "Culture et patrimoine" = "culture",
  "Education et jeunesse" = "education",
  "Environnement" = "environment",
  "Mobilités" = "mobility",
  "Propreté" = "cleanliness",
  "Prévention et sécurité" = "safety",
  "Santé" = "health",
  "Solidarités" = "social",
  "Sport" = "sport"
)

departments <- c(
  "Direction des Affaires Scolaires" = "schools",
  "Direction des Espaces Verts et de l’Environnement" = "green spaces",
  "Direction de la Voirie et des Déplacements" = "roads",
  "Direction de l’Attractivité et de l’Emploi" = "economy",
  "Direction de la Jeunesse et des Sports" = "youth and sports",
  "Direction de la Propreté et de l’Eau" = "cleanliness and water",
  "Direction des Affaires Culturelles" = "culture",
  "Direction Constructions Publiques et Architecture" = "public buildings",
  "Direction de la Démocratie, des Citoyens et des Territoires" = "citizens",
  "Direction des Solidarités" = "social"
)

# abandoned operations will never open (a competing risk): drop them
operations <- bp_raw |>
  filter(avancement_operation != "ABANDONNÉ") |>
  mutate(
    project = identifiant_projet_gagnant,
    vote_date = make_date(as.integer(edition), 10, 1),
    open_date = ym(ouverture_operation),
    # opening dates after the snapshot are planned, not observed
    opened = !is.na(open_date) & open_date <= snapshot_date,
    budget = as.numeric(budget_global_projet_gagnant),
    edition = as.integer(edition),
    scale = if_else(
      echelle_bp == "Budget participatif parisien", "city-wide", "arrondissement"
    ),
    arrondissement = if_else(
      str_detect(arrondissement_operation, "^750\\d\\d$"),
      str_sub(arrondissement_operation, 4, 5),
      "outside / several"
    ),
    priority_neighbourhood = as.integer(operation_q_pop == "oui"),
    theme = themes[thematique],
    department = coalesce(departments[direction_pilote_operation], "other")
  )

one_or_several <- function(x) {
  if (n_distinct(x) == 1) first(x) else "several"
}

# a project is open once all of its operations are open
projects_dates <- operations |>
  summarise(
    vote_date = first(vote_date),
    edition = first(edition),
    theme = first(theme),
    scale = first(scale),
    budget = first(budget),
    n_operations = n(),
    department = one_or_several(department),
    arrondissement = one_or_several(arrondissement),
    priority_share = mean(priority_neighbourhood, na.rm = TRUE),
    opened = all(opened),
    last_open_date = max(open_date),
    .by = project
  ) |>
  mutate(
    end_date = if_else(opened, last_open_date, snapshot_date),
    months = pmax(interval(vote_date, end_date) / months(1), 0.5)
  ) |>
  select(
    project, vote_date, end_date, months, opened, theme, department, scale,
    arrondissement, priority_share, edition, budget, n_operations
  ) |>
  drop_na()

# modelling data: the Surv column is created in the slides
projects <- projects_dates |>
  select(-vote_date, -end_date)
