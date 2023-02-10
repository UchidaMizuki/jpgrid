library(tidyverse)
library(rvest)

pkgload::load_all()

# grid_city ---------------------------------------------------------------

pref_name <- rnaturalearth::ne_states(country = "japan",
                                      returnclass = "sf") |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  select(iso_3166_2, name) |>
  rename(pref_code = iso_3166_2,
         pref_name = name) |>
  mutate(pref_code = pref_code |>
           str_extract("(?<=JP-)\\d+") |>
           as.integer())

html_grid_city <- read_html("https://www.stat.go.jp/data/mesh/m_itiran.html") |>
  html_elements("#section > article:nth-child(2) > ul:nth-child(7) > li > a")

grid_city <- tibble(pref_name_ja = html_grid_city |>
                      html_text() |>
                      str_extract("(?<=^\\d{2}\\s).+(?=（)"),
                    file = html_grid_city |>
                      html_attr("href") |>
                      fs::path_file()) |>
  mutate(pref_code = file |>
           str_extract("^\\d{2}") |>
           as.integer(),
         file = str_glue("https://www.stat.go.jp/data/mesh/csv/{file}")) |>
  rowwise() |>
  mutate(grid_city = read_csv(file,
                              locale = locale(encoding = "shift-jis"),
                              col_types = cols(.default = "c"),
                              col_select = 1:3) |>
           list()) |>
  ungroup() |>
  select(!file) |>
  unnest(grid_city) |>
  rename(city_code = `都道府県市区町村コード`,
         city_name_ja = `市区町村名`,
         grid = `基準メッシュ・コード`) |>
  mutate(grid = parse_grid(grid, "1km")) |>
  left_join(pref_name,
            by = "pref_code") |>
  relocate(pref_code, city_code, pref_name, pref_name_ja, city_name_ja, grid)

usethis::use_data(grid_city,
                  overwrite = TRUE)
