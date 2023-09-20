# １フレーム中の全posプロット
plot.1f <- function(data, plot.frame, plot.pos, mode.skeleton) {
  g <- ggplot() + 
    geom_hline(yintercept = 0, color = "gray20") + 
    geom_point(
      data = data %>%
        filter(
          frame==plot.frame, pos%in%plot.pos) %>%
        mutate(
          pos = factor(pos)), 
      mapping = aes(x = x, y = y, color = pos), 
      size = 2)
  
  if (mode.skeleton!=0) {
    g <- g + 
      geom_segment(
        data = skeleton.data %>% 
          filter(
            mode<as.numeric(mode.skeleton)) %>% 
          left_join(
            data %>% 
              filter(
                frame==plot.frame) %>% 
              select(
                from_x=x, from_y=y, pos), 
            by = c("from"="pos")) %>% 
          left_join(
            data %>% 
              filter(
                frame==plot.frame) %>% 
              select(
                to_x=x, to_y=y, pos), 
            by = c("to"="pos")) %>% 
          na.omit(), 
        mapping = aes(x = from_x, xend = to_x, y = from_y, yend = to_y))
  }
  
  g + 
    lims(
      x = c(-500, 500), y = c(-100, 2100)) +
    theme_minimal_grid() +
    gginteractive()
}

# 指定したposの軌跡
plot.trc <- function(data, plot.pos) {
  ggplot() + 
    geom_hline(yintercept = 0, color = "gray20") + 
    geom_path(
      data = rbind(data) %>% 
        filter(
          pos%in%plot.pos) %>%
        mutate(
          pos = factor(pos)), 
      mapping = aes(x = x, y = y, color = pos, group = pos), 
      linewidth = .5) +
    lims(
      x = c(-500, 500), y = c(-100, 2100)) + 
    theme_minimal_grid() +
    gginteractive()
}

# 運動量
plot.momentum <- function(data, axis="", 
                          momentum.line, mom.pos, 
                          frame_max, plot.smooth, plot.color="black") {
  tmp <- 
    data %>% 
    filter(
      pos==mom.pos) %>% 
    select(
      acc, frame, pos, 
      all_of(paste0(ifelse(axis=="", "", paste0(axis, ".")), momentum.line))) %>% 
    pivot_longer(
      cols = !c(acc, frame, pos), names_to = "name", values_to = "value")
  
  g <- 
    ggplot(
      data = tmp, 
      mapping = aes(x = frame, y = value, linetype = name)) + 
    geom_hline(yintercept = 0, color = "gray20") + 
    geom_line(
      color = plot.color, alpha = .7)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        span = as.numeric(plot.smooth), 
        color = paste0("dark", plot.color), alpha = .5, linetype = "solid", 
        method = "loess", se = F, linewidth = .5)
  }
  
  if (axis=="x") {
    g <- g + 
      scale_y_reverse()
  }
  
  g + 
    lims(
      x = c(1, frame_max)) + 
    labs(
      x = "frame", y = "pixel", linetype = "momentum") +
    theme_minimal_grid() +
    gginteractive()
}

# ３点角度推移
plot.angle <- function(data, plot.o, plot.ab, 
                       frame_max, plot.smooth, plot.color="black") {
  g <- 
    ggplot(
      data = calc.angle(data, plot.o, plot.ab), 
      mapping = aes(x = frame, y = angle)) +
    geom_path(
      color = plot.color, alpha = .7)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        span = as.numeric(plot.smooth), 
        color = paste0("dark", plot.color), alpha = .5, 
        method = "loess", se = F, linewidth = .5)
  }
  
  g + 
    lims(
      x = c(1, frame_max), y = c(0, 200)) + 
    theme_minimal_grid() +
    gginteractive()
}
plot.angle.pile <- function(data1, data2, plot.o, plot.ab, 
                            plot.smooth) {
  tmp1 <- 
    calc.angle(
      data1, plot.o, plot.ab)
  tmp2 <- 
    calc.angle(
      data2, plot.o, plot.ab)
  
  g <- 
    ggplot(
      mapping = aes(x = frame, y = angle)) +
    geom_path(
      data = tmp1, 
      alpha = .6, 
      color = "red") +
    geom_path(
      data = tmp2, 
      alpha = .6, 
      color = "blue")
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        data = tmp1, 
        formula = y ~ x, 
        span = as.numeric(plot.smooth), 
        color = "darkred", alpha = .4, 
        method = "loess", se = F, linewidth = .5) +
      stat_smooth(
        geom = "smooth", 
        data = tmp2, 
        formula = y ~ x, 
        span = as.numeric(plot.smooth), 
        color = "darkblue", alpha = .4, 
        method = "loess", se = F, linewidth = .5)
  }
  
  g +
    lims(
      y = c(0, 200)) + 
    theme_minimal_grid() +
    gginteractive()
}