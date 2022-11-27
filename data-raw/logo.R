library(tidyverse)
library(hexSticker)

pkgload::load_all()

theme_set(theme_void())

font_logo_en <- "Poppins"
font_logo_ja <- "Noto Sans JP"

sysfonts::font_add_google(font_logo_en)
sysfonts::font_add_google(font_logo_ja)

# logo --------------------------------------------------------------------

file_logo <- "man/figures/logo.png"

fill_logo <- "#BC002D"
color_logo <- "snow"

JGD2011 <- 6668

plot_grid_city2015 <- grid_city2015 |>
  mutate(grid = grid_80km(grid)) |>
  distinct(grid) |>
  st_as_sf(crs = JGD2011) |>

  mutate(fill = if_else(as.character(grid) == "3653",
                        "fill_logo",
                        "color_logo")) |>

  ggplot() +
  geom_sf(aes(fill = fill),
          show.legend = FALSE,
          color = fill_logo) +
  scale_fill_manual(values = c(fill_logo = fill_logo,
                               color_logo = color_logo))

sticker(plot_grid_city2015,
        package = "",
        filename = file_logo,

        s_width = 1.6,
        s_height = 1.6,
        s_x = 1,
        s_y = 1,

        h_fill = fill_logo,
        h_color = "transparent",

        spotlight = TRUE,
        l_x = 1,
        l_y = 1,
        l_width = 6,
        l_height = 6) +
  geom_url(url = "jpgrid",
           y = 0.2,
           family = font_logo_en,
           fontface = "bold.italic",
           size = 22,
           color = color_logo) +
  theme(plot.margin = margin(r = -1.5,
                             l = -1.5))

save_sticker(file_logo)
usethis::use_logo(file_logo)
