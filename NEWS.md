# version 0.2.0
- Change package name from `japanmesh` to `jpgrid` to match the English name, 
"Grid Square Codes".
- Replace `mesh` with `grid` in the function names.
- Remove `mesh_to_polygon()` and `mesh_to_point()` and add `sf::st_as_sfc()` 
method.

# version 0.1.1
## New Features
- `mesh_zoomin()` is soft-deprecated and replaced by `mesh_subdivide()`.
- `mesh_zoomout()` is soft-deprecated and replaced by `mesh_80km()`, `mesh_10km()`, etc.
-  New `point_to_mesh()`, `geometry_to_mesh()`, `mesh_grid()` and `bbox_to_mesh()`. 
## Others
- Fix the package versions of `utils` and other imports.
- Update DESCRIPTION and README, and add an explanation of `regional mesh codes` in Japan.

# version 0.1.0
Initial version.
