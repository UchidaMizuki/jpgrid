
pkgload::load_all()

url_grid_city2015 <- "http://www.stat.go.jp/data/mesh/"
url_grid_city2015 <- stringr::str_c(url_grid_city2015,
                                    rvest::read_html(stringr::str_c(url_grid_city2015, "m_itiran.html")) %>%
                                      rvest::html_elements("#section > article:nth-child(2) > ul > li > a") %>%
                                      rvest::html_attr("href"))

grid_city2015 <- url_grid_city2015 %>%
  purrr::map_dfr(function(url_grid_city2015) {
    readr::read_csv(url_grid_city2015,
                    locale = readr::locale(encoding = "cp932"),
                    col_types = readr::cols(.default = "c"),
                    col_select = 1:3)
  }) %>%
  dplyr::rename(city_code = `都道府県市区町村コード`,
                city_name = `市区町村名`,
                grid = `基準メッシュコード`) %>%
  dplyr::mutate(pref_code = city_code %>%
                  stringr::str_sub(1, 2) %>%
                  as.integer(),
                grid = grid_1km(grid)) %>%
  dplyr::relocate(city_code, city_name, pref_code, grid)

usethis::use_data(grid_city2015,
                  overwrite = TRUE)
