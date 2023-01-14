
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpgrid <a href="https://uchidamizuki.github.io/jpgrid/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/jpgrid)](https://CRAN.R-project.org/package=jpgrid)
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

library(tibble)
library(dplyr)
library(ggplot2)
```

### 文字列・数値からの地域メッシュコードの生成

文字列・数値から地域メッシュコードを生成するためには`grid_80km()`，`grid_auto()`などの関数を使用します．

- `grid_auto()`関数はメッシュサイズを自動的に決定します．
- デフォルト（`strict = TRUE`）では，メッシュコードの桁数が所定の桁数であることを要求します．

``` r
library(jpgrid)

x <- c("53394526313", "5339358633", "533945764", "53394611", "523503", "5339", NA)

grid_80km(x)
#> <grid_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>
grid_125m(x)
#> <grid_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
grid_auto(x)
#> Guessing grid size as `80km`
#> <grid_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>

grid_80km(x, strict = FALSE)
#> <grid_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
grid_125m(x, strict = FALSE)
#> <grid_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
grid_auto(x, strict = FALSE)
#> Guessing grid size as `80km`
#> <grid_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
```

### 地域メッシュコードのサイズの変換

地域メッシュコードのメッシュサイズを粗くする場合には，`grid_80km()`などの関数を使用します．
また，`grid_subdivide()`関数により，地域メッシュコードの細分化を行います．

- `grid_subdivide()`は元のメッシュに含まれるメッシュを要素にもつリストを出力します．
- 500 mメッシュ・100 mメッシュ間の変換に対応しています．

``` r
grid500m <- grid_500m("533945764")

grid_1km(grid500m)
#> <grid_1km[1]>
#> [1] 53394576

grid100m <- grid_subdivide(grid500m,
                           size = "100m")
grid100m
#> [[1]]
#> <grid_100m[25]>
#>  [1] 5339457655 5339457665 5339457675 5339457685 5339457695 5339457656
#>  [7] 5339457666 5339457676 5339457686 5339457696 5339457657 5339457667
#> [13] 5339457677 5339457687 5339457697 5339457658 5339457668 5339457678
#> [19] 5339457688 5339457698 5339457659 5339457669 5339457679 5339457689
#> [25] 5339457699

tibble(grid100m = grid100m[[1]]) |> 
  as_tbl_grid() |> 
  sf::st_as_sf() |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = grid100m))
#> Don't know how to automatically pick scale for object of type
#> <grid_100m/grid/vctrs_rcrd/vctrs_vctr>. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### 経度・緯度から地域メッシュコードへの変換

`XY_to_grid()`関数は，経度・緯度を地域メッシュコードに変換します．

``` r
tibble(X = c(139.7008, 135.4375), # 経度
       Y = c(35.68906, 34.70833)) |>  # 緯度
  mutate(grid100m = XY_to_grid(X, Y, size = "100m"),
         grid125m = XY_to_grid(X, Y, size = "125m")) |> 
  knitr::kable()
```

|        X |        Y | grid100m   | grid125m    |
|---------:|---------:|:-----------|:------------|
| 139.7008 | 35.68906 | 5339452660 | 53394526313 |
| 135.4375 | 34.70833 | 5235034499 | 52350344444 |

### 地域メッシュコードから経度・緯度への変換

`grid_to_XY()`関数は，地域メッシュコードを経度・緯度に変換します．

``` r
tibble(grid = grid_100m(c("5339452660", "5235034590"))) |> 
  mutate(grid_to_XY(grid)) |> 
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
neighbor <- grid_10km("644142") |> 
  grid_neighbor(n = c(0:2),
                simplify = FALSE)

neighbor[[1]] |> 
  as_tbl_grid() |> 
  sf::st_as_sf() |> 
  
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = grid_neighbor))
#> Don't know how to automatically pick scale for object of type
#> <grid_10km/grid/vctrs_rcrd/vctrs_vctr>. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
neighbor_neumann <- grid_10km("644142") |> 
  grid_neighbor(n = c(0:2),
                simplify = F,
                moore = F)

neighbor_neumann[[1]] |> 
  as_tbl_grid() |> 
  sf::st_as_sf() |> 
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = grid_neighbor))
#> Don't know how to automatically pick scale for object of type
#> <grid_10km/grid/vctrs_rcrd/vctrs_vctr>. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

### メッシュ間の線分描画

`grid_line()`関数により，メッシュ間の線分上に存在するメッシュを抽出します．

``` r
grid_from <- grid_80km(c("6441", "5339"))
grid_to <- grid_80km(c("5237", "5235"))

line <- grid_line(grid_from, grid_to)

tibble::tibble(grid = line[[1]]) |> 
  as_tbl_grid() |> 
  sf::st_as_sf() |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = grid))
#> Don't know how to automatically pick scale for object of type
#> <grid_80km/grid/vctrs_rcrd/vctrs_vctr>. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

メッシュの`list`を与えることで複数メッシュを通る場合に対応可能です．

- `close = TRUE`で線分を閉じます．
- `skip_na = TRUE`で`NA`をスキップします．

``` r
grid_1 <- grid_80km(c("6441", "5339", NA, "5250"))
grid_2 <- grid_80km(c("6439", "5211", "4013", "6635"))

line <- grid_line(list(grid_1, grid_2), 
                  close = TRUE,
                  skip_na = TRUE)

tibble::tibble(grid = line[[1]]) %>% 
  as_tbl_grid() |> 
  sf::st_as_sf() |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = grid))
#> Don't know how to automatically pick scale for object of type
#> <grid_80km/grid/vctrs_rcrd/vctrs_vctr>. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### メッシュ間距離の算出

`grid_distance()`関数は，メッシュ間距離（大円距離）を算出します．

- `grid_line()`と同様にメッシュの`list`で経路距離を算出可能です．

``` r
grid_from <- grid_80km(c("6441", "5339"))
grid_to <- grid_80km(c("5237", "5235"))

distance <- grid_distance(grid_from, grid_to)

print(distance)
#> Units: [m]
#> [1] 953014.2 371081.9
```

### その他

- `grid_move()`関数により，東西南北方向の地域メッシュコードを算出可能です．
- `sf::st_as_sfc`関数により`sfc`ジオメトリを出力可能です．
- 80kmメッシュの桁が負や三桁以上になる範囲外のメッシュについては，当該コードを`<-1>`，`<123>`のように表示し，既存メッシュと明確に区別できるようにしています．

## jpmeshとの処理速度の比較

本パッケージのメッシュ・緯度経度間の変換速度は，jpmeshパッケージと比べて数十～数百倍ほど高速です．

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />
