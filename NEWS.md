# jpgrid 0.4.0

- Add `grid_neighborhood()` and deprecate `grid_neighbor()`.
  - `grid_neighborhood()` has `type` argument to specify the type of neighborhood and by default `type = "von_neumann"`.
- Add `grid_components()` to get the connected components of grid square codes.
- Rename `grid_city` to `grid_city_2020`.
- Delete deprecated functions.

# jpgrid 0.3.1

- Fix bug in `st_as_sfc.grid()`.
- Fix `vec_ptype_abbr()` methods.

# jpgrid 0.3.0

- Add `parse_grid()` and `grid_convert()`, and deprecate `grid_80km()`, `grid_10km()` and so on.
- Add `grid_to_coords()` and `coords_to_grid()`, and deprecate `grid_to_XY()` and `XY_to_grid()`.
- Remove deprecations for `grid_as_sf()` and `grid_as_stars()`, and deprecated `as_tbl_grid()`.
- Remove `grid_bbox()` and `grid_circle()`.
- Rename `grid_city2015` to `grid_city`.

# jpgrid 0.2.1
- Add `as_tbl_grid()`.
- Add `st_as_sf()`, `st_bbox()`, `st_as_stars()` and `plot()` for `tbl_grid`. 
- Add `grid_circle()`.
- Rename `grid_rectangle()` to `grid_bbox()`.

# jpgrid 0.2.0
- Change package name from `japanmesh` to `jpgrid` to match the English name, 
"Grid Square Codes".
- Replace `mesh` with `grid` in the function names.
- Remove `mesh_to_polygon()` and `mesh_to_point()` and add `sf::st_as_sfc()` 
method.

# jpgrid 0.1.1
## New Features
- `mesh_zoomin()` is soft-deprecated and replaced by `mesh_subdivide()`.
- `mesh_zoomout()` is soft-deprecated and replaced by `mesh_80km()`, `mesh_10km()`, etc.
-  New `point_to_mesh()`, `geometry_to_mesh()`, `mesh_grid()` and `bbox_to_mesh()`. 
## Others
- Fix the package versions of `utils` and other imports.
- Update DESCRIPTION and README, and add an explanation of `regional mesh codes` in Japan.

# jpgrid 0.1.0
Initial version.
