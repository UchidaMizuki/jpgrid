
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpgrid <a href="https://uchidamizuki.github.io/jpgrid/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/jpgrid)](https://CRAN.R-project.org/package=jpgrid)
[![R-CMD-check](https://github.com/UchidaMizuki/jpgrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/jpgrid/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/jpgrid/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/jpgrid?branch=main)
<!-- badges: end -->

*[The English version of the README is
here.](https://github.com/UchidaMizuki/jpgrid/blob/main/README.en.md)*

jpgridは，日本産業規格JIS X 0410
[地域メッシュコード](https://www.jisc.go.jp/app/jis/general/GnrJISNumberNameSearchList?show&jisStdNo=X0410)
（Grid Square Code）で定められた基準地域メッシュ (1次～3次)
と分割地域メッシュ，および3次メッシュ1/10細分区画を利用するためのRパッケージです．
地域メッシュコードは，経度・緯度に基づいて，日本全国の地域に設定された正方形に近い地域区分です．
地域メッシュコードの詳細については，[統計局ページ](https://www.stat.go.jp/data/mesh/pdf/gaiyo1.pdf)を確認してください．

地域メッシュコードの概要を以下に示します．jpgridでは，`grid_80km`のように，メッシュの一片の長さで各地域メッシュコードを区別します．

| 名称                        | 一片の長さ | 桁数 |
|:----------------------------|:-----------|-----:|
| 第1次地域区画 (1次メッシュ) | 約80km     |    4 |
| 第2次地域区画 (2次メッシュ) | 約10km     |    6 |
| 第3次地域区画 (3次メッシュ) | 約1km      |    8 |
| 2分の1地域メッシュ          | 約500m     |    9 |
| 4分の1地域メッシュ          | 約250m     |   10 |
| 8分の1地域メッシュ          | 約125m     |   11 |
| 3次メッシュ1/10細分区画     | 約100m     |   10 |

jpgridは，Rパッケージの[jpmesh](https://github.com/uribo/jpmesh)より高速な処理を可能とするために開発されたものです．
jpgridとjpmeshとの主な違いとして以下が挙げられます．

1.  メッシュコードに，メッシュサイズ (`grid_80km`など)
    が明示的に与えられます．
2.  国土にかからない (海上の) メッシュにも対応しています．
3.  n次隣接メッシュの抽出・メッシュ間の (線分)
    経路上のメッシュ抽出や距離算出などの複雑な処理が可能です．

## インストール方法

CRANからインストールが可能です．

``` r
install.packages("jpgrid")
```

開発版はGitHubからインストールしてください．

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/jpgrid")
```

## 使用方法

``` r
library(jpgrid)
library(tidyverse)

JGD2011 <- 6668
```

### ジオメトリの地域メッシュコードへの変換

`geometry_to_grid()`により，`sf`オブジェクトを地域メッシュコードに変換することができます．
また，`grid_as_sf()`により地域メッシュ（`grid`クラス）を含むデータを`sf`オブジェクトに変換できます．

``` r
geom_chiba <- rnaturalearth::ne_states(country = "japan",
                                       returnclass = "sf") |> 
  filter(name == "Chiba")
grid_chiba <- geometry_to_grid(geom_chiba, "10km") |> 
  first() |> 
  grid_as_sf(crs = sf::st_crs(geom_chiba))

grid_chiba |> 
  ggplot() +
  geom_sf(data = geom_chiba) +
  geom_sf(fill = "transparent") +
  geom_sf_text(aes(label = as.character(grid)),
               size = 2)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

また，`grid_city`には，市区町村別の1
kmメッシュコードが格納されています．

``` r
grid_city |> 
  filter(str_starts(city_code, "121")) |> 
  grid_as_sf(crs = JGD2011) |> 
  ggplot(aes(fill = as_factor(city_name_ja))) +
  geom_sf() +
  scale_fill_brewer("City",
                    palette = "Set2")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### 文字列・数値からの地域メッシュコードの生成

文字列・数値から地域メッシュコードを生成するためには`parse_grid()`を使用します．

- `grid_size = "80km"`のようにメッシュサイズを指定します．
  - `grid_size = NULL`の場合はメッシュサイズが自動的に決定されます．
- デフォルト（`strict = TRUE`）では，メッシュコードの桁数が所定の桁数であることを要求します．

``` r
x <- c("53394526313", "5339358633", "533945764", "53394611", "523503", "5339", NA)

parse_grid(x, grid_size = "80km")
#> <grid_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>
parse_grid(x, grid_size = "125m")
#> <grid_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
parse_grid(x)
#> Guessing, grid_size = "80km"
#> <grid_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>

parse_grid(x, "80km",
           strict = FALSE)
#> <grid_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
parse_grid(x, "125m",
           strict = FALSE)
#> <grid_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
parse_grid(x, 
           strict = FALSE)
#> Guessing, grid_size = "80km"
#> <grid_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
```

### 地域メッシュコードのサイズの変換

地域メッシュコードのメッシュサイズを粗くする場合には，`grid_convert()`を使用します．
また，`grid_subdivide()`により，地域メッシュコードの細分化を行います．

- `grid_subdivide()`は，元のメッシュに含まれるメッシュを要素にもつリストを出力します．
- 500 mメッシュ・100 mメッシュ間の変換に対応しています．

``` r
grid_500m <- parse_grid("533945764", "500m")

grid_convert(grid_500m, "1km")
#> <grid_1km[1]>
#> [1] 53394576

grid_100m <- grid_subdivide(grid_500m, "100m")
grid_100m
#> [[1]]
#> <grid_100m[25]>
#>  [1] 5339457655 5339457665 5339457675 5339457685 5339457695 5339457656
#>  [7] 5339457666 5339457676 5339457686 5339457696 5339457657 5339457667
#> [13] 5339457677 5339457687 5339457697 5339457658 5339457668 5339457678
#> [19] 5339457688 5339457698 5339457659 5339457669 5339457679 5339457689
#> [25] 5339457699

tibble(grid_100m = grid_100m[[1]]) |> 
  grid_as_sf(crs = JGD2011) |>  
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = as.character(grid_100m)))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### 経度・緯度から地域メッシュコードへの変換

`coords_to_grid()`は，経度・緯度を地域メッシュコードに変換します．

``` r
tibble(X = c(139.7008, 135.4375), # 経度
       Y = c(35.68906, 34.70833)) |>  # 緯度
  mutate(grid_100m = coords_to_grid(X, Y, "100m"),
         grid_125m = coords_to_grid(X, Y, "125m")) |> 
  knitr::kable()
```

|        X |        Y | grid_100m  | grid_125m   |
|---------:|---------:|:-----------|:------------|
| 139.7008 | 35.68906 | 5339452660 | 53394526313 |
| 135.4375 | 34.70833 | 5235034499 | 52350344444 |

### 地域メッシュコードから経度・緯度への変換

`grid_to_coords()`は，地域メッシュコードを経度・緯度に変換します．

``` r
tibble(grid = parse_grid(c("5339452660", "5235034590"), "100m")) |> 
  mutate(grid_to_coords(grid)) |> 
  knitr::kable()
```

| grid       |        X |        Y |
|:-----------|---------:|---------:|
| 5339452660 | 139.7006 | 35.68875 |
| 5235034590 | 135.4381 | 34.70792 |

### 隣接メッシュの算出

`grid_neighbor()`関数は，隣接するメッシュを算出します．

- `n`を指定することでn次隣接メッシュの算出が可能
- `moore = FALSE`でノイマン近傍での算出が可能

``` r
neighbor <- parse_grid("644142", "10km") |> 
  grid_neighbor(n = c(0:2),
                simplify = FALSE)

neighbor[[1]] |> 
  grid_as_sf(crs = JGD2011) |> 
  
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = as.character(grid_neighbor)))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
neighbor_neumann <- parse_grid("644142", "10km") |> 
  grid_neighbor(n = c(0:2),
                simplify = F,
                moore = F)

neighbor_neumann[[1]] |> 
  grid_as_sf(crs = JGD2011) |> 
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = as.character(grid_neighbor)))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### メッシュ間の線分描画

`grid_line()`関数により，メッシュ間の線分上に存在するメッシュを抽出します．

``` r
grid_from <- parse_grid(c("6441", "5339"), "80km")
grid_to <- parse_grid(c("5237", "5235"), "80km")

line <- grid_line(grid_from, grid_to)

tibble::tibble(grid = line[[1]]) |> 
  grid_as_sf(crs = JGD2011) |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = as.character(grid)))
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

メッシュの`list`を与えることで複数メッシュを通る場合に対応可能です．

- `close = TRUE`で線分を閉じます．
- `skip_na = TRUE`で`NA`をスキップします．

``` r
grid_1 <- parse_grid(c("6441", "5339", NA, "5250"), "80km")
grid_2 <- parse_grid(c("6439", "5211", "4013", "6635"), "80km")

line <- grid_line(list(grid_1, grid_2), 
                  close = TRUE,
                  skip_na = TRUE)

tibble::tibble(grid = line[[1]]) |> 
  grid_as_sf(crs = JGD2011) |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = as.character(grid)))
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

### メッシュ間距離の算出

`grid_distance()`関数は，メッシュ間距離（大円距離）を算出します．

- `grid_line()`と同様にメッシュの`list`で経路距離を算出可能です．

``` r
grid_from <- parse_grid(c("6441", "5339"), "80km")
grid_to <- parse_grid(c("5237", "5235"), "80km")

distance <- grid_distance(grid_from, grid_to)

print(distance)
#> Units: [m]
#> [1] 953014.2 371081.9
```

### その他

- `grid_move()`関数により，東西南北方向の地域メッシュコードを算出可能です．
- 80kmメッシュの桁が負や三桁以上になる範囲外のメッシュについては，当該コードを`<-1>`，`<123>`のように表示し，既存メッシュと明確に区別できるようにしています．

## jpmeshとの処理速度の比較

本パッケージのメッシュ・緯度経度間の変換速度は，jpmeshパッケージと比べて数十～数百倍ほど高速です．

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />
