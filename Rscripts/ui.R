### Shiny UI
ui <- fluidPage(
  # Application title
  titlePanel("Motion Data"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;", 
      width = 3, 
      h3("pklファイル名入力"),
      uiOutput("select.input_file1"), 
      uiOutput("select.input_file2"), 
      # fileInput(
      #   inputId = "file_id1",
      #   label = "読込ファイル名① (.pkl, .rds) ：",
      #   accept = c(".pkl", ".rds")),
      # fileInput(
      #   inputId = "file_id2",
      #   label = "読込ファイル名② (.pkl, .rds)：",
      #   accept = c(".pkl", ".rds")), 
      fluidRow(
        column(
          width = 6, 
          actionButton(
            inputId = "Submit", 
            label = "Submit")), 
        column(
          width = 6, 
          actionButton(
            inputId = "stopButton", 
            label = "Stop"))), 
      h3("================"), 
      h4("全般設定"), 
      radioButtons(
        inputId = "button.unit", 
        label = "単位：", 
        choices = c(
          "px (測定値)"=FALSE
          # , "mm (推定値)"=TRUE
          ), 
        selected = FALSE, 
        inline = TRUE),
      radioButtons(
        inputId = "button.rescale.frame", 
        label = "2ファイルのフレーム数を合わせる：", 
        choices = c("はい"=TRUE, "いいえ"=FALSE), 
        selected = FALSE, 
        inline = TRUE), 
      radioButtons(
        inputId = "button.rescale.height", 
        label = "2ファイルの身長帯を合わせる：", 
        choices = c(
          # "はい (約170cm)"=TRUE, 
          "いいえ"=FALSE), 
        selected = FALSE, 
        inline = TRUE), 
      uiOutput("select.spline.coordinate"), 
      uiOutput("select.add.spline.curve"), 
      h3("================"), 
      h4("1フレーム分析・軌跡分析用"), 
      uiOutput("select.position"), 
      uiOutput("slider.frame"), 
      h6("注：ポーズしても描画が終わるまで止まりません"), 
      radioButtons(
        inputId = "button.skeleton", 
        label = "骨格を表示する：", 
        choices = c("非表示"=0, "簡易"=1, "詳細"=2), 
        # choices = c("非表示"=0, "簡易"=1, "詳細"=2, "詳細 (左右色分け)"=3), 
        selected = 0, 
        inline = TRUE), 
      h3("================"), 
      h4("運動量分析用"), 
      uiOutput("select.momentum.position"), 
      checkboxGroupInput(
        inputId = "select.momentum.line", 
        label = "描画する運動量を選択：", 
        choices = c("変位"="mov", "速度"="velocity", "加速度"="acceleration"), 
        selected = "mov", 
        inline = TRUE),
      sliderInput(
        inputId = "per_frame", 
        label = "単位あたりフレーム数設定 [frame (240fps)]：", 
        min = 1, 
        max = 240, 
        value = 1, 
        step = 1),
      h3("================"), 
      h4("角度推移分析用"), 
      uiOutput("select.position.center"),
      uiOutput("select.position.other"),
      h3("================"), 
      h6("中央大学 理工学部"), 
      h6("ビジネスデータサイエンス学科"), 
      h6("2023年度 前期"), 
      h6("データサイエンス実験B"), 
      h6("大草班"), 
      h6("ストリームデータ解析用アプリケーション"), 
      h6("v2.2 (2023-07-04)")
    ), 
    
    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "1フレーム分析", 
        #   plotlyOutput(
        #     outputId = "plot.1frame", 
        #     width = "50%", height = "600px")), 
        # tabPanel(
        #   title = "1フレーム分析(比較)", 
          plotlyOutput(
            outputId = "plot.1frame.compare", 
            height = "700px")), 
        tabPanel(
          title = "軌跡分析", 
        #   plotlyOutput(
        #     outputId = "plot.trace", 
        #     width = "50%", height = "600px")), 
        # tabPanel(
        #   title = "軌跡分析(比較)", 
          plotlyOutput(
            outputId = "plot.trace.compare", 
            height = "700px")), 
        tabPanel(
          title = "運動量分析", 
          h4("※単位：1秒≒240frame, 変位 [全般設定単位], 速度 [全般設定単位/frame], 加速度 [全般設定単位/frame^2]"), 
          h3("運動量"), 
          plotlyOutput(
            outputId = "plot.momentum.compare",
            height = "250px"),
          h3("x軸方向運動量 (マイナス方向：前方)"), 
          plotlyOutput(
            outputId = "plot.momentum.x.compare",
            height = "250px"), 
          h3("y軸方向運動量 (プラス方向：上方)"), 
          plotlyOutput(
            outputId = "plot.momentum.y.compare",
            height = "250px")),
        tabPanel(
          title = "角度推移分析", 
          #   plotlyOutput(
          #     outputId = "plot.angle", 
          #     width = "50%", height = "400px")), 
          # tabPanel(
          #   title = "3点角度分析(比較)",
          plotlyOutput(
            outputId = "plot.angle.pile", 
            width = "50%", height = "400px"),
          plotlyOutput(
            outputId = "plot.angle.compare",
            height = "400px"))#, 
        # tabPanel(
        #   title = "3点角度運動量分析", 
        #     plotlyOutput(
        #       outputId = "plot.momentum",
        #       width = "50%", height = "400px")),
        #   tabPanel(
        #     title = "3点角度運動分析(比較)",
        #   plotlyOutput(
        #     outputId = "plot.momentum.pile", 
        #     width = "50%", height = "400px"),
        #   plotlyOutput(
        #     outputId = "plot.momentum.compare",
        #     height = "400px"))
    ))
  )
)
