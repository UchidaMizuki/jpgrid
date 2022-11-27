library(tidyverse)

pkgload::load_all()

# grid_city2015 -----------------------------------------------------------

pref_code <- str_pad(1:47, 2,
                     pad = "0")
url_grid_city2015 <- str_glue("https://www.stat.go.jp/data/mesh/csv/{pref_code}.csv")

grid_city2015 <- url_grid_city2015 %>%
  map_dfr(partial(read_csv,
                  locale = locale(encoding = "shift-jis"),
                  col_types = cols(.default = "c"),
                  col_select = 1:3)) |>
  rename(city_code = `都道府県市区町村コード`,
                city_name = `市区町村名`,
                grid = `基準メッシュ・コード`) %>%
  mutate(pref_code = city_code %>%
                  str_sub(1, 2) %>%
                  as.integer(),
                grid = grid_1km(grid)) %>%
  relocate(city_code, city_name, pref_code, grid) %>%
  as_tbl_grid(grid,
              size = "1km")

usethis::use_data(grid_city2015,
                  overwrite = TRUE)
