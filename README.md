
<!-- README.md is generated from README.Rmd. Please edit that file -->

# japanmesh

<!-- badges: start -->
<!-- badges: end -->

地域メッシュと経度・緯度との変換や異なるサイズの地域メッシュ間の変換などを行うためのRパッケージです．

## インストール方法

<!-- You can install the released version of japanmesh from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("japanmesh") -->
<!-- ``` -->
<!-- And the development version from [GitHub](https://github.com/) with: -->

CRANには登録されていないためGitHubからインストールしてください．

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/japanmesh")
```

## 使用方法

``` r
library(japanmesh)
library(tibble)
library(dplyr)
```

### 文字列・数値からの地域メッシュの生成

文字列・数値から地域メッシュを生成するためには`as_mesh()`関数を使用します．
デフォルトでは，`NA`を除く値のうち最もサイズが大きい地域メッシュに合わせてサイズが決定されます．
`size`でサイズを指定することもできます（その場合，桁が不足している地域メッシュは`NA`になります）．

``` r
library(japanmesh)

x <- c("53394526313", "5339358633", "533945764", "53394611", "523503", "5339", NA)

as_mesh(x)
#> <mesh_80km[7]>
#> [1] 5339 5339 5339 5339 5235 5339 <NA>
as_mesh(x, size = "1km")
#> <mesh_1km[7]>
#> [1] 53394526 53393586 53394576 53394611 <NA>     <NA>     <NA>
as_mesh(x, size = "125m")
#> <mesh_125m[7]>
#> [1] 53394526313 <NA>        <NA>        <NA>        <NA>        <NA>       
#> [7] <NA>
```

### 地域メッシュのサイズの変換

`as_mesh()`で，地域メッシュをよりサイズが大きい地域メッシュに変換することもできます．
また，100 mメッシュから500 mメッシュへの変換も対応しています．

``` r
tibble(mesh_10km = as_mesh(x, size = "10km")) %>%
   mutate(mesh_80km = as_mesh(mesh_10km, size = "80km"))
#> # A tibble: 7 x 2
#>   mesh_10km mesh_80km
#>    <msh10k>  <msh80k>
#> 1    533945      5339
#> 2    533935      5339
#> 3    533945      5339
#> 4    533946      5339
#> 5    523503      5235
#> 6        NA        NA
#> 7        NA        NA

tibble(mesh_100m = as_mesh(x, size = "100m")) %>%
  mutate(mesh_500m = as_mesh(mesh_100m, size = "500m"))
#> # A tibble: 7 x 2
#>    mesh_100m mesh_500m
#>     <msh100>  <msh500>
#> 1         NA        NA
#> 2 5339358633 533935861
#> 3         NA        NA
#> 4         NA        NA
#> 5         NA        NA
#> 6         NA        NA
#> 7         NA        NA
```

### 経度・緯度から地域メッシュへの変換

`XY_to_mesh()`関数で経度・緯度を地域メッシュに変換します．

``` r
tibble(X = c(139.7008, 135.4375), # 経度
       Y = c(35.68906, 34.70833)) %>% # 緯度
  mutate(mesh_100m = XY_to_mesh(X, Y, size = "100m"),
         mesh_125m = XY_to_mesh(X, Y, size = "125m"))
#> # A tibble: 2 x 4
#>       X     Y  mesh_100m   mesh_125m
#>   <dbl> <dbl>   <msh100>    <msh125>
#> 1  140.  35.7 5339452660 53394526313
#> 2  135.  34.7 5235034499 52350344444
```

### 地域メッシュから経度・緯度への変換

`mesh_to_XY()`関数で地域メッシュを経度・緯度に変換します．

``` r
tibble(mesh = c("5339452660", "5235034590")) %>% 
  mutate(mesh %>% 
           as_mesh(size = "100m") %>% 
           mesh_to_XY())
#> # A tibble: 2 x 3
#>   mesh           X     Y
#>   <chr>      <dbl> <dbl>
#> 1 5339452660  140.  35.7
#> 2 5235034590  135.  34.7
```

### その他

-   `mesh_move()`関数により，東西南北方向の地域メッシュを算出できます．
-   `mesh_neighbor()`関数により，隣接するメッシュを算出できます．

## 他パッケージとの比較

地域メッシュを扱うRパッケージとして，本パッケージの他に`jpmesh`パッケージがあります．
本パッケージの`jpmesh`との優位点として，以下が挙げられます．

-   処理速度が`jpmesh`パッケージより速い場合があります．
-   `jpmesh::meshcode()`と違い，`as_mesh()`に`NA`を入力してもエラーを吐きません．

### `jpmesh`パッケージとの処理速度の比較

以下の例では，本パッケージの計算速度は，`jpmesh`パッケージと比べて数百倍程度，高速です．

``` r
set.seed(1234)
df <- tibble(X = runif(10 ^ 3, 139, 140),
             Y = runif(10 ^ 3, 39, 40))

# XY to mesh
# jpmesh
tictoc::tic()
df_jpmesh <- df %>% 
  mutate(mesh = jpmesh::coords_to_mesh(X, Y))
head(df_jpmesh)
#> # A tibble: 6 x 3
#>       X     Y       mesh
#>   <dbl> <dbl> <meshcode>
#> 1  139.  39.8   59396009
#> 2  140.  39.5   59391489
#> 3  140.  39.1   58395438
#> 4  140.  39.4   59390429
#> 5  140.  39.8   59395618
#> 6  140.  39.4   59390561
tictoc::toc()
#> 7.93 sec elapsed

# japanmesh
tictoc::tic()
df_japanmesh <- df %>% 
  mutate(mesh = XY_to_mesh(X, Y, size = "1km"))
head(df_japanmesh)
#> # A tibble: 6 x 3
#>       X     Y     mesh
#>   <dbl> <dbl>  <msh1k>
#> 1  139.  39.8 59396009
#> 2  140.  39.5 59391489
#> 3  140.  39.1 58395438
#> 4  140.  39.4 59390429
#> 5  140.  39.8 59395618
#> 6  140.  39.4 59390561
tictoc::toc()
#> 0.03 sec elapsed

# mesh to XY
# jpmesh
tictoc::tic()
df_jpmesh <- df_jpmesh %>% 
  select(mesh) %>% 
  mutate(jpmesh::mesh_to_coords(mesh),
         .keep = "unused")
head(df_jpmesh)
#> # A tibble: 6 x 5
#>     meshcode lng_center lat_center lng_error lat_error
#>   <meshcode>      <dbl>      <dbl>     <dbl>     <dbl>
#> 1   59396009       139.       39.8   0.00625   0.00417
#> 2   59391489       140.       39.5   0.00625   0.00417
#> 3   58395438       140.       39.1   0.00625   0.00417
#> 4   59390429       140.       39.4   0.00625   0.00417
#> 5   59395618       140.       39.8   0.00625   0.00417
#> 6   59390561       140.       39.4   0.00625   0.00417
tictoc::toc()
#> 8.41 sec elapsed

# japanmesh
tictoc::tic()
df_japanmesh <- df_japanmesh %>% 
  select(mesh) %>% 
  mutate(mesh_to_XY(mesh))
head(df_japanmesh)
#> # A tibble: 6 x 3
#>       mesh     X     Y
#>    <msh1k> <dbl> <dbl>
#> 1 59396009  139.  39.8
#> 2 59391489  140.  39.5
#> 3 58395438  140.  39.1
#> 4 59390429  140.  39.4
#> 5 59395618  140.  39.8
#> 6 59390561  140.  39.4
tictoc::toc()
#> 0.01 sec elapsed
```
