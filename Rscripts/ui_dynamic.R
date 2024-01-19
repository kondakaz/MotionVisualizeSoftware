### ファイル入力 ----
output$select.input_file1 <- 
  renderUI({
    file_name <- c(list.files("pkl/", full.names = TRUE), 
                   list.files(paste0(fs::path_home(),"/Desktop"), full.names = TRUE))
    file_name <- file_name[which(str_detect(file_name, ".*.pkl$|.*.rds$"))]
    file_name <- c(file_name[which(str_detect(file_name, "Prom"))], 
                   file_name[which(str_detect(file_name, "average"))], 
                   sort(file_name[which(!str_detect(file_name, "Prom|average"))]))
    selectInput(
      inputId = "file_id1", 
      label = "読込ファイル名① (.pkl, .rds) ：",
      choices = c(Choose="", file_name), 
      selectize=TRUE)
})
output$select.input_file2 <- 
  renderUI({
    file_name <- c(list.files("pkl/", full.names = TRUE), 
                   list.files(paste0(fs::path_home(),"/Desktop"), full.names = TRUE))
    file_name <- file_name[which(str_detect(file_name, ".*.pkl$|.*.rds$"))]
    file_name <- c(file_name[which(str_detect(file_name, "Prom"))], 
                   file_name[which(str_detect(file_name, "average"))], 
                   sort(file_name[which(!str_detect(file_name, "Prom|average"))]))
    selectInput(
      inputId = "file_id2", 
      label = "読込ファイル名① (.pkl, .rds) ：",
      choices = c(Choose="", file_name), 
      selectize=TRUE)
})

### 描画部位 ----
output$select.position <- 
  renderUI({
    selectInput(
      inputId = "select.pos", 
      label = "描画部位選択 (複数選択可・無選択の場合全選択)：", 
      choices = pos.data$V2, 
      selected = "", 
      multiple = TRUE)
})

### スプライン補間関連 ----
# 読み込みファイルの座標補間
output$select.spline.coordinate <- 
  renderUI({
    radioButtons(
      inputId = "button.spline", 
      label = "座標をスプライン補間する：", 
      choices = c("しない"=".org"
                  # , "準"="NULL", spline.level.coordinate
                  ), 
      selected = ".org", 
      inline = TRUE)
})
# 運動量・角度推移に平滑化曲線描画
output$select.add.spline.curve <- 
  renderUI({
    radioButtons(
      inputId = "button.smooth", 
      label = "平滑化曲線を表示する：", 
      choices = c("非表示"=-1, spline.level.curve), 
      selected = -1, 
      inline = TRUE)
})

### １フレーム分析 ----
# 1フレーム分析時描画フレームの指定
output$slider.frame <- 
  renderUI({
    sliderInput(
      inputId = "plot.frame", 
      label = "描画フレーム指定：", 
      min = 0, 
      max = min(frame.range()[which(frame.range()!=0)]),
      value = 0, 
      step = 1, 
      animate = animationOptions(
        interval = 1000, 
        loop = FALSE, 
        playButton = actionButton(
          inputId = "AnimationStartButton",
          label = "Start",
          icon = icon("play"),
          width = "100px",
          style = "margin-top: 10px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        pauseButton = actionButton(
          inputId = "AnimationPauseButton",
          label = "Pause",
          icon = icon("pause"),
          width = "100px",
          style = "margin-top: 10px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ))
})

### 部位別運動量分析 ----
# 運動量分析時注目部位指定
output$select.momentum.position <- 
  renderUI({
    selectInput(
      inputId = "select.mom.pos", 
      label = "描画部位指定：", 
      choices = pos.data$V2, 
      selected = "LKnee")
})

### 注目部位角度 ----
# 中心角部位
output$select.position.center <- 
  renderUI({
    selectInput(
      inputId = "select.pos.cent", 
      label = "中心角部位選択：", 
      choices = pos.data$V2, 
      selected = "LKnee")
})
# 中心以外2部位
output$select.position.other <- 
  renderUI({
    selectInput(
      inputId = "select.pos.other", 
      label = "他2点部位選択 (2点選択)：", 
      choices = pos.data$V2[which(!pos.data$V2%in%input$select.pos.cent)], 
      selected = list("LHip", "LAnkle"), 
      multiple = TRUE)
})


