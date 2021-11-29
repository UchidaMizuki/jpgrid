
<!-- README.md is generated from README.Rmd. Please edit that file -->

# japanmesh

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/japanmesh)](https://CRAN.R-project.org/package=japanmesh)
<!-- badges: end -->

README is currently only available in Japanese.

japanmeshは，日本で用いられる「[地域メッシュ](https://ja.wikipedia.org/wiki/%E5%9C%B0%E5%9F%9F%E3%83%A1%E3%83%83%E3%82%B7%E3%83%A5)」を利用するためのRパッケージです．
80 kmメッシュ (1次メッシュ) から100 mメッシュ (3次メッシュ1/10細分区画)
までに対応しています．
地域メッシュの詳細については，[統計局ページ](https://www.stat.go.jp/data/mesh/pdf/gaiyo1.pdf)を確認してください．

japanmeshは，[jpmesh](https://github.com/uribo/jpmesh)より高速な処理を可能とするために開発されたものです．
japanmeshのjpmeshとの主な違いとして以下が挙げられます．

1.  メッシュコードに，メッシュサイズ (`mesh_80km`など)
    が明示的に与えられます．
2.  国土にかからない (海上の) メッシュにも対応しています．
3.  n次隣接メッシュの抽出・メッシュ間の (線分)
    経路上のメッシュ抽出や距離算出などの複雑な処理が可能です．

## インストール方法

CRANからインストールが可能です．

``` r
install.packages("japanmesh")
```

開発版はGitHubからインストールしてください．

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/japanmesh")
```

## 使用方法

``` r
library(japanmesh)

library(tibble)
library(dplyr)
library(ggplot2)
```

### 文字列・数値からの地域メッシュの生成

文字列・数値から地域メッシュを生成するためには`mesh_80km()`，`mesh_auto()`などの関数を使用します．

-   `mesh_auto()`関数はメッシュサイズを自動的に決定します．
-   デフォルト（`strict = T`）では，メッシュコードの桁数が所定の桁数であることを要求します．

``` r
library(japanmesh)

x <- c("53394526313", "5339358633", "533945764", "53394611", "523503", "5339", NA)

mesh_80km(x)
#> <mesh_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>
mesh_125m(x)
#> <mesh_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
mesh_auto(x)
#> Guessing mesh size as `80km`
#> <mesh_80km[7]>
#> [1] <NA> <NA> <NA> <NA> <NA> 5339 <NA>

mesh_80km(x, strict = F)
#> <mesh_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
mesh_125m(x, strict = F)
#> <mesh_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
mesh_auto(x, strict = F)
#> Guessing mesh size as `80km`
#> <mesh_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
```

### 地域メッシュのサイズの変換

地域メッシュのメッシュサイズを粗くする場合には，`mesh_80km()`などの関数を使用します．
また，`mesh_subdivide()`関数により，地域メッシュの細分化を行います．

-   `mesh_subdivide()`は元のメッシュに含まれるメッシュを要素にもつリストを出力します．
-   500 mメッシュ・100 mメッシュ間の変換に対応しています．

``` r
mesh500m <- mesh_500m("533945764")

mesh_1km(mesh500m)
#> <mesh_1km[1]>
#> [1] 53394576

mesh100m <- mesh_subdivide(mesh500m,
                           size = "100m")
mesh100m
#> [[1]]
#> <mesh_100m[25]>
#>  [1] 5339457655 5339457665 5339457675 5339457685 5339457695 5339457656
#>  [7] 5339457666 5339457676 5339457686 5339457696 5339457657 5339457667
#> [13] 5339457677 5339457687 5339457697 5339457658 5339457668 5339457678
#> [19] 5339457688 5339457698 5339457659 5339457669 5339457679 5339457689
#> [25] 5339457699

tibble(mesh100m = mesh100m[[1]]) %>% 
  mutate(polygon = mesh_to_polygon(mesh100m)) %>% 
  sf::st_as_sf() %>%
  
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = mesh100m))
#> Don't know how to automatically pick scale for object of type mesh_100m/mesh/vctrs_rcrd/vctrs_vctr. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### 経度・緯度から地域メッシュへの変換

`XY_to_mesh()`関数は，経度・緯度を地域メッシュに変換します．

``` r
tibble(X = c(139.7008, 135.4375), # 経度
       Y = c(35.68906, 34.70833)) %>% # 緯度
  mutate(mesh100m = XY_to_mesh(X, Y, size = "100m"),
         mesh125m = XY_to_mesh(X, Y, size = "125m")) %>% 
  knitr::kable()
```

|        X |        Y | mesh100m   | mesh125m    |
|---------:|---------:|:-----------|:------------|
| 139.7008 | 35.68906 | 5339452660 | 53394526313 |
| 135.4375 | 34.70833 | 5235034499 | 52350344444 |

### 地域メッシュから経度・緯度への変換

`mesh_to_XY()`関数は，地域メッシュを経度・緯度に変換します．

``` r
tibble(mesh = c("5339452660", "5235034590")) %>% 
  mutate(mesh %>% 
           mesh_100m() %>% 
           mesh_to_XY()) %>% 
  knitr::kable()
```

| mesh       |        X |        Y |
|:-----------|---------:|---------:|
| 5339452660 | 139.7006 | 35.68875 |
| 5235034590 | 135.4381 | 34.70792 |

### 隣接メッシュの算出

`mesh_neighbor()`関数は，隣接するメッシュを算出します．

-   `n`を指定することでn次隣接メッシュの算出が可能
-   `moore = F`でノイマン近傍での算出が可能

``` r
neighbor <- mesh_10km("644142") %>% 
  mesh_neighbor(n = c(0:2),
                simplify = F)

neighbor[[1]] %>% 
  mutate(geometry = mesh_to_polygon(mesh_neighbor)) %>% 
  sf::st_as_sf() %>% 
  
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = mesh_neighbor))
#> Don't know how to automatically pick scale for object of type mesh_10km/mesh/vctrs_rcrd/vctrs_vctr. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
neighbor_neumann <- mesh_10km("644142") %>% 
  mesh_neighbor(n = c(0:2),
                simplify = F,
                moore = F)

neighbor_neumann[[1]] %>% 
  mutate(geometry = mesh_to_polygon(mesh_neighbor)) %>% 
  sf::st_as_sf() %>% 
  
  ggplot(aes(fill = as.factor(n))) +
  geom_sf() +
  geom_sf_text(aes(label = mesh_neighbor))
#> Don't know how to automatically pick scale for object of type mesh_10km/mesh/vctrs_rcrd/vctrs_vctr. Defaulting to continuous.
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### メッシュ間の線分描画

`mesh_line()`関数により，メッシュ間の線分上に存在するメッシュを抽出します．

``` r
mesh_from <- mesh_80km(c("6441", "5339"))
mesh_to <- mesh_80km(c("5237", "5235"))

line <- mesh_line(mesh_from, mesh_to)

print(line)
#> [[1]]
#> <mesh_80km[13]>
#>  [1] 6441 6341 6240 6140 6040 5939 5839 5739 5638 5538 5438 5337 5237
#> 
#> [[2]]
#> <mesh_80km[5]>
#> [1] 5339 5338 5237 5236 5235
plot(line[[1]])
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

メッシュの`list`を与えることで複数メッシュを通る場合に対応可能です．

-   `close = T`で線分を閉じます．
-   `skip_na = T`で`NA`をスキップします．

``` r
mesh_1 <- mesh_80km(c("6441", "5339", NA, "5250"))
mesh_2 <- mesh_80km(c("6439", "5211", "4013", "6635"))

line <- mesh_line(list(mesh_1, mesh_2), 
                  close = T,
                  skip_na = T)

print(line)
#> [[1]]
#> <mesh_80km[37]>
#>  [1] 6441 6341 6241 6140 6040 5940 5840 5740 5640 5539 5439 5339 5339 5340 5341
#> [16] 5342 5343 5344 5245 5246 5247 5248 5249 5250 5250 5349 5448 5548 5647 5746
#> [31] 5845 5945 6044 6143 6242 6342 6441
#> 
#> [[2]]
#> <mesh_80km[74]>
#>  [1] 6439 6438 6337 6336 6235 6234 6133 6132 6131 6030 6029 5928 5927 5826 5825
#> [16] 5824 5723 5722 5621 5620 5519 5518 5517 5416 5415 5314 5313 5212 5211 5211
#> [31] 5111 5011 4912 4812 4712 4612 4512 4412 4313 4213 4113 4013 4013 4114 4215
#> [46] 4316 4416 4517 4618 4719 4820 4921 5021 5122 5223 5324 5425 5526 5627 5727
#> [61] 5828 5929 6030 6131 6232 6332 6433 6534 6635 6635 6536 6537 6438 6439
plot(line[[1]])
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

### メッシュ間距離の算出

`mesh_distance()`関数は，メッシュ間距離（大円距離）を算出します．

-   `mesh_line()`と同様にメッシュの`list`で経路距離を算出可能です．

``` r
mesh_from <- mesh_80km(c("6441", "5339"))
mesh_to <- mesh_80km(c("5237", "5235"))

distance <- mesh_distance(mesh_from, mesh_to)

print(distance)
#> Units: [m]
#> [1] 953014.2 371081.9
```

### その他

-   `mesh_move()`関数により，東西南北方向の地域メッシュを算出可能です．
-   `mesh_to_polygon()`関数および`mesh_to_point()`関数により`sfc`ジオメトリを出力可能です．
-   80kmメッシュの桁が負や三桁以上になる範囲外のメッシュについては，当該コードを`<-1>`，`<123>`のように表示し，既存メッシュと明確に区別できるようにしています．

## jpmeshとの処理速度の比較

本パッケージのメッシュ・緯度経度間の変換速度は，`jpmesh`パッケージと比べて数百倍ほど高速です．

    #> Coordinate system already present. Adding new coordinate system, which will replace the existing one.

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

    #> Coordinate system already present. Adding new coordinate system, which will replace the existing one.

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />
