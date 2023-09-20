### 環境設定 ----
source("Rscripts/app_setting.R")
source("Rscripts/server_function_plotly_interactive.R")

### ｐythonデータ入力 ----
source("Rscripts/server_function_read_py.R")

### 入力データ整形 ----
source("Rscripts/server_function_shaping.R")

### プロット関数 ----
source("Rscripts/server_function_plot.R")

### アプリケーション Server ----
server <- function(input, output, session) {
  
  ### 動的UI読み込み ----
  source("Rscripts/ui_dynamic.R", local=T, encoding="UTF-8")
  
  ### 終了ボタン動作 ----
  observeEvent(
    input$stopButton, {
      stopApp()
  })
  
  ### ファイル入力 ----
  data1.org <- 
    eventReactive(input$Submit, {
      if (str_detect(input$file_id1, "rds")) {
        readRDS(input$file_id1)
      } else {
        make.dt(
          get.pkl(input$file_id1))
      }
    })
  data2.org <- 
    eventReactive(input$Submit, {
      if (input$file_id2=="") {
        data1.org()
      } else {
        if (str_detect(input$file_id2, "rds")) {
          readRDS(input$file_id2)
        } else {
          make.dt(
            get.pkl(input$file_id2))
        }
      }
    })
  
  data1 <- 
    reactive({
      data.shaping(
        data1.org(), data2.org(), input$file_id1, 
        input$button.spline, input$button.unit, input$per_frame, 
        input$button.rescale.frame, input$button.rescale.height)
    })
  data2 <- 
    reactive({
      data.shaping(
        data2.org(), data1.org(), input$file_id2, 
        input$button.spline, input$button.unit, input$per_frame, 
        input$button.rescale.frame, input$button.rescale.height)
    })
  
  ### 描画部位 ----
  plot.pos <- 
    reactive({
      if (sum(input$select.pos%in%pos.data$V2)==0) {
        return(pos.data$V2)
      } else {
        return(input$select.pos)
      }
    })
  
  ### １フレーム分析 ----
  frame.range <- 
    reactive({
      if (input$file_id2=="") {
        c(0, max(data1()$frame, na.rm=T))
      } else {
        c(min(max(data1()$frame, na.rm=T), max(data2()$frame, na.rm=T), na.rm=T), 
          max(max(data1()$frame, na.rm=T), max(data2()$frame, na.rm=T), na.rm=T))
      }
    })
  
  output$plot.1frame.compare <- 
    renderPlotly({
      plot.1f(
        bind_rows(
          data.frame(
            file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
            data1()), 
          data.frame(
            file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
            data2())), 
        input$plot.frame, plot.pos(), input$button.skeleton, input$button.unit)
      # subplot(
      #   plot.1f(data1(), input$plot.frame, plot.pos(), input$button.skeleton, input$button.unit), 
      #   plot.1f(data2(), input$plot.frame, plot.pos(), input$button.skeleton, input$button.unit), 
      #   margin = 0.05)
    })
  
  ### 部位別軌跡分析 ----
  output$plot.trace.compare <- 
    renderPlotly({
      plot.trc(
        bind_rows(
          data.frame(
            file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
          data1()), 
          data.frame(
            file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
          data2())), 
        plot.pos(), input$button.unit)
    })
  
  ### 部位別運動量分析 ----
  output$plot.momentum.x.compare <- 
    renderPlotly({
      if (length(input$select.momentum.line)!=0) {
        subplot(
          plot.momentum.pile(
            bind_rows(
              data.frame(
                file = str_remove_all(input$file_id1, ".*/|.rds$|.pkl$"), 
                data1()), 
              data.frame(
                file = str_remove_all(input$file_id2, ".*/|.rds$|.pkl$"), 
                data2())), 
            "x", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          plot.momentum(
            bind_rows(
              data.frame(
                file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
                data1()), 
              data.frame(
                file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
                data2())), 
            "x", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          widths = c(0.33, 0.67), margin = 0.05)
      }
    })
  
  output$plot.momentum.y.compare <- 
    renderPlotly({
      if (length(input$select.momentum.line)!=0) {
        subplot(
          plot.momentum.pile(
            bind_rows(
              data.frame(
                file = str_remove_all(input$file_id1, ".*/|.rds$|.pkl$"), 
                data1()), 
              data.frame(
                file = str_remove_all(input$file_id2, ".*/|.rds$|.pkl$"), 
                data2())), 
            "y", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          plot.momentum(
            bind_rows(
              data.frame(
                file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
                data1()), 
              data.frame(
                file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
                data2())), 
            "y", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          widths = c(0.33, 0.67), margin = 0.05)
      }
    })
  
  output$plot.momentum.compare <- 
    renderPlotly({
      if (length(input$select.momentum.line)!=0) {
        subplot(
          plot.momentum.pile(
            bind_rows(
              data.frame(
                file = str_remove_all(input$file_id1, ".*/|.rds$|.pkl$"), 
                data1()), 
              data.frame(
                file = str_remove_all(input$file_id2, ".*/|.rds$|.pkl$"), 
                data2())), 
            "", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          plot.momentum(
            bind_rows(
              data.frame(
                file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
                data1()), 
              data.frame(
                file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
                data2())), 
            "", 
            input$select.momentum.line, input$select.mom.pos, 
            max(frame.range()), input$button.smooth), 
          widths = c(0.33, 0.67), margin = 0.05)
      }
    })
  
  
  ### ３点角度推移分析 ----
  output$plot.angle.pile <- 
    renderPlotly({
      if (length(input$select.pos.other)==2) {
        plot.angle.pile(
          bind_rows(
            data.frame(
              file = str_remove_all(input$file_id1, ".*/|.rds$|.pkl$"), 
              calc.angle(
                data1(), input$select.pos.cent, input$select.pos.other)), 
            data.frame(
              file = str_remove_all(input$file_id2, ".*/|.rds$|.pkl$"), 
              calc.angle(
                data2(), input$select.pos.cent, input$select.pos.other))), 
          input$button.smooth)
      }
    })
  
  output$plot.angle.compare <- 
    renderPlotly({
      if (length(input$select.pos.other)==2) {
        plot.angle(
          bind_rows(
            data.frame(
              file = paste0("ファイル①: ", str_remove_all(input$file_id1, ".*/|.rds$|.pkl$")), 
              calc.angle(
                data1(), input$select.pos.cent, input$select.pos.other)), 
            data.frame(
              file = paste0("ファイル②: ", str_remove_all(input$file_id2, ".*/|.rds$|.pkl$")), 
              calc.angle(
                data2(), input$select.pos.cent, input$select.pos.other))), 
            max(frame.range()), input$button.smooth)
      }
    })
}
