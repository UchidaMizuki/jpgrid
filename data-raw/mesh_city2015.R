
# List of mesh codes by municipality in 2015 ------------------------------

pkgload::load_all()

url_mesh_city2015 <- "http://www.stat.go.jp/data/mesh/"
url_mesh_city2015 <- stringr::str_c(url_mesh_city2015,
                                    rvest::read_html(stringr::str_c(url_mesh_city2015, "m_itiran.html")) %>%
                                      rvest::html_elements("#section > article:nth-child(2) > ul > li > a") %>%
                                      rvest::html_attr("href"))

mesh_city2015 <- url_mesh_city2015 %>%
  purrr::map_dfr(function(url_mesh_city2015) {
    readr::read_csv(url_mesh_city2015,
                    locale = readr::locale(encoding = "cp932"),
                    col_types = readr::cols(.default = "c"),
                    col_select = 1:3)
  }) %>%
  dplyr::rename(city_code = `都道府県市区町村コード`,
                city_name = `市区町村名`,
                mesh = `基準メッシュコード`) %>%
  dplyr::mutate(pref_code = city_code %>%
                  stringr::str_sub(1, 2) %>%
                  as.integer(),
                mesh = mesh_1km(mesh)) %>%
  dplyr::relocate(city_code, city_name, pref_code, mesh)

usethis::use_data(mesh_city2015,
                  overwrite = TRUE)
