# 設定ファイル読み込み
# 各マーカーの部位名称
pos.data <- fread("Csvfiles/pos.csv")
# 各マーカーの接続情報
skeleton.data <- fread("Csvfiles/skeleton.csv")


# 映像の寸法をpxからmmへ変更する縮尺
# px_to_mm.x = 667/180
# px_to_mm.y = 412/180

# 単位：mm 時 1フレーム分析・軌跡分析 描画範囲
# plot.limit.x.mm <- c(-1000, 1000)
# plot.limit.y.mm <- c(-100, 2100)

# 単位：px 時 1フレーム分析・軌跡分析 描画範囲
# plot.limit.x.px <- c(-500, 500)
# plot.limit.y.px <- c(-100, 1100)

# スプライン補間程度設定
# mgcv::gam の sp 引数 (平滑化パラメータ) を設定
# x 座標におけるフレーム数、y 座標における x 座標の平滑化の程度
# 変更した場合 server_function_shaping.R の make.dt 関数も修正必須 (関数内の列名と引数)
spline.level.coordinate <- 
  c(
    # しない ... 平滑化しない
    # 適 ... gam 関数のデフォルト設定
    "微"="0.00001", 
    "弱"="0.01", 
    "中"="0.1", 
    "強"="0.9"
  )

# 運動量・角度推移に追記する loess 曲線のパラメータ設定
# ggplot2::geom_smooth の span 引数 (平滑化の程度) を設定
spline.level.curve <- 
  c(
    "微"=0.1, 
    "弱"=0.25, 
    "中"=0.5, 
    "準"=0.75, # 0.75 が geom_smooth 関数のデフォルト
    "強"=0.9
  )