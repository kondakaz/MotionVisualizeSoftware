# .pklデータ読み込み
py.pkl <- import("pickle")
py.built <- import_builtins()

get.pkl <- function(file_id) {
  file_path <- file_id
  
  with(
    py.built$open(file_path, "rb") %as% file, {
      data <- py.pkl$load(file)
    })
  
  tmp <- data.frame()
  for (i in 1:25) {
    tmp <- 
      bind_rows(
        tmp, 
        data.frame(
          pos0 = i, 
          x = data[, i, 1], 
          y = data[, i, 2], 
          acc = data[, i, 3]) %>%
          mutate(
            frame = row_number() - 1)
      )
  }
  
  data <- 
    left_join(
      tmp, 
      pos.data %>%
        rename(
          pos0 = V1, pos = V2), 
      by = "pos0") %>% 
    select(
      !pos0)
  return(data)
}