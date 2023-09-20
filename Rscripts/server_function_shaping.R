# 角度計算
calc.acos <- function(list) {
  v1 <- c(list[3], list[4]) - c(list[1], list[2])
  v2 <- c(list[5], list[6]) - c(list[1], list[2])
  # 標準化
  v1 <- v1/sqrt(sum(v1^2))
  v2 <- v2/sqrt(sum(v2^2))
  # 出力
  return(acos(v1%*%v2)*180/pi)
}

calc.angle <- function(data, plot.o, plot.ab) {
  bind_rows(
    data %>% 
      select(
        !c(mov, x.mov, y.mov, velocity, x.velocity, y.velocity, 
           acceleration, x.acceleration, y.acceleration)) %>% 
      filter(pos==plot.o), 
    data %>% 
      select(
        !c(mov, x.mov, y.mov, velocity, x.velocity, y.velocity, 
           acceleration, x.acceleration, y.acceleration)) %>% 
      filter(pos%in%plot.ab)) %>% 
    pivot_longer(
      !c(frame, pos, acc), names_to = "hoge", values_to = "val") %>% 
    select(
      !c(acc, pos, hoge)) %>%
    group_nest(frame) %>% 
    mutate(
      angle = map_dbl(data, ~calc.acos(.$val))) %>% ungroup()
}

# 運動量計算
calc.momentum <- function(data, select.per_frame) {
  data %>% 
    group_by(pos) %>% 
    mutate(
      y2 = y - mean(y), 
      cumsum.x = cumsum(x), 
      cumsum.y = cumsum(y2), 
      x.mov = lead(cumsum.x, n = select.per_frame, default = NA) - cumsum.x, 
      y.mov = lead(cumsum.y, n = select.per_frame, default = NA) - cumsum.y, 
      mov = sqrt(x.mov^2 + y.mov^2), 
      x.velocity = x.mov/select.per_frame, 
      y.velocity = y.mov/select.per_frame, 
      velocity = sqrt(x.velocity^2 + y.velocity^2), 
      # x.acceleration = (x.velocity^2 - lead(x.velocity, n = select.per_frame, default = NA)^2)/(2*x.mov),
      # y.acceleration = (y.velocity^2 - lead(y.velocity, n = select.per_frame, default = NA)^2)/(2*y.mov),
      x.acceleration = (lead(x.velocity, n = select.per_frame, default = NA) - x.velocity)/select.per_frame,
      y.acceleration = (lead(y.velocity, n = select.per_frame, default = NA) - y.velocity)/select.per_frame,
      acceleration = (lead(velocity, n = select.per_frame, default = NA) - velocity)/select.per_frame) %>% 
    ungroup() %>% 
    select(
      !c(y2, cumsum.x, cumsum.y))
}

# 読み込んだ2ファイルのフレーム数を 多いほうに 合わせる
calc.rescale.frame <- function(data, data2) {
  if (nrow(data2)>nrow(data)) {
    frame.max <- max(data$frame, data2$frame)
    frame.range <- seq(0, frame.max, by = 1)
    frame.rate <- max(data2$frame)/max(data$frame)
    tmp.data <- data.frame()
    
    for (i in 1:25) { 
      pos.name <- pos.data$V2[i] 
      
      tmp <- 
        data %>% 
        filter(
          pos==pos.name) %>% 
        mutate(
          frame = frame*frame.rate)
      
      if (nrow(tmp)>10) {
        spline.x <- 
          splinefun(tmp$frame, tmp$x)
        spline.y <- 
          splinefun(tmp$frame, tmp$y)
        
        tmp.data <- 
          bind_rows(
            tmp.data, 
            data.frame(
              pos = pos.name, 
              x = spline.x(frame.range), 
              y = spline.y(frame.range), 
              acc = 0, 
              frame = frame.range))
      }
    }
    
    return(tmp.data)
  } else {
    return(data)
  }
}

# 読み込んだ2ファイルの身長を 約170cm に変換
calc.rescale.height <- function(data, button.unit=TRUE) {
  data.range <- 
    data %>% 
    filter(
      pos%in%c("Nose", "LBigToe", "RBigToe")) %>% 
    .$y %>% 
    range(., na.rm = T)
  
  data %>% 
    mutate(
      y = (y - min(data.range)) * ((1700 / ifelse(button.unit==TRUE, 1, px_to_mm.y))/(max(data.range) - min(data.range)))) %>% 
    return()
}

# 読み込んだ座標をスプライン補間
calc.spline <- function(data, sp.gam = NULL) {
  tmp.full <- data.frame()
  
  for (i in 1:25) {
    pos.name <- pos.data$V2[i] 
    tmp <- 
      left_join(
        data.frame(
          frame = c(0:max(data$frame))), 
        data %>% 
          filter(
            pos==pos.name), 
        by = "frame")
    
    if (nrow(na.omit(tmp))>10&nrow(distinct(tmp, x, y))>10) {
      gam.x <-
        gam(
          x ~ s(frame),
          data = tmp, sp = sp.gam)
      tmp <- 
        tmp %>% 
        mutate(
          x.est = predict(
            gam.x, 
            newdata = data.frame(frame = tmp$frame)))
      
      gam.y <- 
        gam(
          y ~ s(x.est) + frame, 
          data = tmp, sp = sp.gam)
      tmp <- 
        tmp %>% 
        mutate( 
          y.est = predict(
            gam.y, 
            data.frame(
              frame = tmp$frame, 
              x.est = tmp$x.est)))
    }
    
    tmp.full <- 
      bind_rows(
        tmp.full, tmp)
  }
  
  tmp.full %>%
    return()
}

# 読み込んだデータの整形
data.shaping <- function(data, data2, file_id, 
                         select.button.spline, select.button.unit, select.per_frame, 
                         select.button.rescale.frame, select.button.rescale.height) {
  tmp <- 
    data %>% 
    select(
      acc, frame, pos, ends_with(select.button.spline)) %>% 
    rename(
      x = 4, y = 5) %>% 
    mutate(
      x = x - mean(x, na.rm = T)) %>% 
    na.omit()
  
  if (select.button.unit) {
    tmp <- 
      tmp %>% 
      mutate(
        x = x*px_to_mm.x, 
        y = y*px_to_mm.y)
  }
  
  if (select.button.rescale.frame) {
    tmp <- 
      calc.rescale.frame(tmp, data2)
  }
  
  if (select.button.rescale.height) {
    tmp <- 
      calc.rescale.height(tmp, select.button.unit)
  }
  
  heel.max <- 
    tmp %>% 
    filter(
      pos%in%c("LHeel", "RHeel")) %>% 
    .$y %>% 
    max(., na.rm=T)
  
  tmp <- tmp %>%
    mutate(
      y = -y + heel.max)
  
  tmp %>% 
    calc.momentum(., select.per_frame) %>%
    return()
}

# 一括スプライン補間
make.dt <- function(data) {
  tmp <- 
    data %>%
    filter(
      !(x==0&y==0)) %>% 
    na.omit()
  
  bind_cols(
    expand.grid(
      frame = c(0:max(data$frame)),
      pos = pos.data$V2, 
      acc = 0),
    calc.spline(tmp, 0) %>% 
      select(
        x.org = x.est, y.org = y.est)) %>% 
    bind_cols(
      calc.spline(tmp, NULL) %>% 
        select(
          xNULL = x.est, yNULL = y.est)) %>% 
    bind_cols(
      calc.spline(tmp, 0.00001) %>% 
        select(
          x0.00001 = x.est, y0.00001 = y.est)) %>% 
    bind_cols(
      calc.spline(tmp, 0.01) %>% 
        select(
          x0.01 = x.est, y0.01 = y.est)) %>% 
    bind_cols(
      calc.spline(tmp, 0.1) %>% 
        select(
          x0.1 = x.est, y0.1 = y.est)) %>% 
    bind_cols(
      calc.spline(tmp, 0.9) %>% 
        select(
          x0.9 = x.est, y0.9 = y.est))
}
