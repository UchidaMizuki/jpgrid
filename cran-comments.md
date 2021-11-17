## Test environments
* Windows 10 Pro, R 4.1.2

## R CMD check results

0 errors v | 0 warnings v | 1 note x

-- R CMD check results ------------------------ japanmesh 0.1.0 ----
Duration: 1m 27.5s

> checking R code for possible problems ... NOTE
  mesh_distance: no visible binding for global variable 'diff_n_X'
  mesh_distance: no visible binding for global variable 'n_Y'
  mesh_distance: no visible binding for global variable 'n_Y_to'
  mesh_distance: no visible binding for global variable 'Y'
  mesh_distance: no visible binding for global variable 'diff_X'
  mesh_distance: no visible binding for global variable 'Y_to'
  mesh_distance: no visible binding for global variable 'm'
  mesh_impl: no visible binding for global variable 'code'
  mesh_line: no visible binding for global variable 'x_to'
  mesh_line: no visible binding for global variable 'x'
  mesh_line: no visible binding for global variable 'y_to'
  mesh_line: no visible binding for global variable 'y'
  mesh_line: no visible binding for global variable 'dx'
  mesh_line: no visible binding for global variable 'dy'
  mesh_line: no visible binding for global variable 'err'
  mesh_line: no visible binding for global variable 'sx'
  mesh_line: no visible binding for global variable 'sy'
  mesh_neighbor : <anonymous>: no visible binding for global variable
    'n_X'
  mesh_neighbor : <anonymous>: no visible binding for global variable
    'n_Y'
  mesh_neighbor: no visible binding for global variable 'n_X'
  mesh_neighbor: no visible binding for global variable 'n_Y'
  mesh_to_XY: no visible binding for global variable 'X_min'
  mesh_to_XY: no visible binding for global variable 'Y_min'
  mesh_to_sfc: no visible binding for global variable 'X_min'
  mesh_to_sfc: no visible binding for global variable 'Y_min'
  mesh_to_sfc: no visible binding for global variable 'X_max'
  mesh_to_sfc: no visible binding for global variable 'Y_max'
  size_match: no visible binding for global variable 'm'
  Undefined global functions or variables:
    X_max X_min Y Y_max Y_min Y_to code diff_X diff_n_X dx dy err m n_X
    n_Y n_Y_to sx sy x x_to y y_to
