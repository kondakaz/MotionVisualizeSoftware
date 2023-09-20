### ワーキングディレクトリ設定
setwd("C:/R_links/jikkenB_ookusa/")

### packageの準備
source("Rscripts/package.R", encoding="UTF-8")

### UI
source("Rscripts/ui.R", encoding="UTF-8")

### Server
source("Rscripts/server.R", encoding="UTF-8")

### Run the application 
shinyApp(
  ui = ui, 
  server = server, 
  ### WebブラウザでShinyを起動したい場合TRUE・推奨
  options = list(launch.browser = TRUE))