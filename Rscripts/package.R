### pacman が入っていなければインストール
if (!require(pacman)) install.packages("pacman")
require(pacman)

packages <- c(
  ### shinyアプリ用
  "shiny", 
  ### データ集計・整形用
  "data.table", "dplyr", "mgcv", "reticulate", "stringr", "tidyr", 
  ### グラフィカル
  "cowplot", "plotly"
)

# pacman::p_update(char = packages)
pacman::p_load(char = packages)

### miniconda が入っていなければインストール
if (
  tryCatch(
    expr = reticulate::install_miniconda(), 
    error = function(e) {"TRUE"})=="TRUE") {
  print("minicondaはインストール済みです")
} else {
  print("minicondaをインストールしました")
  reticulate::py_install("pandas")
}
# reticulate::miniconda_uninstall()
