### ワーキングディレクトリ設定
setwd("C:/jikkenB_ookusa/")

### packageの準備
source("Rscripts/package.R", encoding="UTF-8")

file_list <- c(paste0(getwd(), "/", list.files("pkl/", full.names = TRUE)), 
               list.files(paste0(fs::path_home(),"/Desktop"), full.names = TRUE))
file_list <- file_list[which(str_detect(file_list, ".*\\.pkl$|.*\\.rds$"))]

py_builtin <- import_builtins()
py_pickle <- import("pickle")
py_numpy <- import("numpy")
py_scipy <- import("scipy")

py.is_all_zeros <- function(input_array) {
  py_numpy$all(input_array==0)
}
py.interpolate_zeros <- function(input_array) {
  # 0以外の値を持つインデックスと対応する値を取得
  non_zero_indices <- py_numpy$nonzero(input_array)[[1]] + 1
  non_zero_values <- input_array[non_zero_indices]
  # 線形補間を実行
  py_numpy$interp(
    py_numpy$arange(length(input_array)), non_zero_indices, non_zero_values) %>% 
    return()
}
py.spline_hokan <- function(input_array) {
  if (!py.is_all_zeros(input_array)) {
    hokan <- py.interpolate_zeros(input_array)
  } else {
    hokan <- input_array
  }
  return(hokan)
}

new_frames <- 100

for (file in file_list[length(file_list)]) {
  with (
    py_builtin$open(file, "rb") %as% f, {
      # py_builtin$print(file)
      data <- py_pickle$load(f)
    })
  
  # data[frame, pos, dim(x-y-acc)]
  new_data <- data.frame()
  for (pos in 1:ncol(data[, , 3])) {
    f_old <- py_numpy$linspace(1, nrow(data[, , 1]), nrow(data[, , 1]))
    f_new <- py_numpy$linspace(1, nrow(data[, , 1]), as.integer(nrow(data[, , 1])*new_frames))
    
    spline.x <- 
      py_scipy$interpolate$make_interp_spline(
        f_old, py.spline_hokan(data[, pos, 1]))
    spline.y <- 
      py_scipy$interpolate$make_interp_spline(
        f_old, py.spline_hokan(data[, pos, 2]))
    
    new_data <- 
      bind_rows(
        new_data, 
        data.frame(
          x.est = spline.x(f_new), 
          y.est = spline.y(f_new), 
          acc = 0, 
          pos = pos) %>% 
        mutate(
          frame = row_number()))
    
  }
}

# ggplot() +
#   geom_segment(
#     data = new_data %>% 
#       filter(!(x.est==0&y.est==0)) %>% 
#       mutate(
#         x.est_next = lead(x.est, default = NA), 
#         y.est_next = lead(y.est, default = NA)),
#     mapping = aes(x = x.est, y = y.est, xend = x.est_next, yend = y.est_next, color = as.character(pos))) +
#   geom_point(
#     data = new_data,
#     size = 2, alpha = 0, 
#     mapping = aes(x = x.est, y = y.est, color = as.character(pos))) +
#   scale_y_reverse()
