# １フレーム中の全posプロット
plot.1f <- function(data, plot.frame, plot.pos, mode.skeleton, select.unit) {
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
          # mutate(
          #   color = ifelse(mode.skeleton==3, color, "black")) %>% 
          left_join(
            data %>% 
              filter(
                frame==plot.frame) %>% 
              select(
                from_x=x, from_y=y, pos, file), 
            by = c("from"="pos")) %>% 
          left_join(
            data %>% 
              filter(
                frame==plot.frame) %>% 
              select(
                to_x=x, to_y=y, pos, file), 
            by = c("to"="pos", "file")) %>% 
          na.omit(), 
        mapping = aes(x = from_x, xend = to_x, y = from_y, yend = to_y))
        # mapping = aes(x = from_x, xend = to_x, y = from_y, yend = to_y, fill = color))
  }
  
  if (select.unit) {
    g <- g + 
      coord_cartesian(
        xlim = plot.limit.x.mm, ylim = plot.limit.y.mm)
  } else {
    g <- g + 
      coord_cartesian(
        xlim = plot.limit.x.px, ylim = plot.limit.y.px)
  }
  
  g +
    facet_grid(. ~ file) +
    theme_minimal_grid() +
    gginteractive()
}

# 指定したposの軌跡
plot.trc <- function(data, plot.pos, select.unit) {
  g <- 
    ggplot() + 
    geom_hline(yintercept = 0, color = "gray20") + 
    geom_path(
      data = rbind(data) %>% 
        filter(
          pos%in%plot.pos) %>%
        mutate(
          pos = factor(pos)), 
      mapping = aes(x = x, y = y, color = pos, group = pos), 
      linewidth = .5)# + 
    # geom_segment(
    #   data = rbind(data) %>% 
    #     filter(
    #       pos%in%plot.pos) %>%
    #     mutate(
    #       x_next = lead(x, default = NA), 
    #       y_next = lead(y, default = NA), 
    #       pos = factor(pos)), 
    #   mapping = aes(x = x, y = y, xend = x_next, yend = y_next, color = pos, group = pos), 
    #   linewidth = .5)
  
  if (select.unit) {
    g <- g + 
      coord_cartesian(
        xlim = plot.limit.x.mm, ylim = plot.limit.y.mm)
  } else {
    g <- g + 
      coord_cartesian(
        xlim = plot.limit.x.px, ylim = plot.limit.y.px)
  }
  
  g + 
    facet_grid(. ~ file) +
    theme_minimal_grid() +
    gginteractive()
}

# 運動量
plot.momentum <- function(data, axis="", 
                          momentum.line, mom.pos, 
                          frame_max, plot.smooth) {
  tmp <- 
    data %>% 
    filter(
      pos==mom.pos) %>% 
    select(
      acc, frame, pos, file, 
      all_of(paste0(ifelse(axis=="", "", paste0(axis, ".")), momentum.line))) %>% 
    pivot_longer(
      cols = !c(acc, frame, pos, file), names_to = "name", values_to = "value")
  
  g <- 
    ggplot(
      data = tmp, 
      mapping = aes(x = frame, y = value, linetype = name)) + 
    geom_hline(
      yintercept = 0, color = "gray20") + 
    geom_line(
      alpha = .8)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        method = "loess", se = F, 
        span = as.numeric(plot.smooth), 
        alpha = .5, linewidth = .5)
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
    facet_grid(. ~ file) + 
    theme_minimal_grid() +
    gginteractive()
}

plot.momentum.pile <- function(data, axis="", 
                               momentum.line, mom.pos, 
                               frame_max, plot.smooth) {
  tmp <- 
    data %>% 
    filter(
      pos==mom.pos) %>% 
    select(
      acc, frame, pos, file, 
      all_of(paste0(ifelse(axis=="", "", paste0(axis, ".")), momentum.line))) %>% 
    pivot_longer(
      cols = !c(acc, frame, pos, file), names_to = "name", values_to = "value")
  
  g <- 
    ggplot(
      data = tmp, 
      mapping = aes(x = frame, y = value, linetype = name, color = file)) + 
    geom_hline(
      yintercept = 0, color = "gray20") + 
    geom_line(
      alpha = .5)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        method = "loess", se = F, 
        span = as.numeric(plot.smooth), 
        alpha = .8, linewidth = .5)
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
plot.angle <- function(data, frame_max, plot.smooth) {
  g <- 
    ggplot(
      data = data, 
      mapping = aes(x = frame, y = angle)) +
    geom_path(
      alpha = .8)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        method = "loess", se = F, 
        span = as.numeric(plot.smooth), 
        alpha = .5, linewidth = .5)
  }
  
  g + 
    coord_cartesian(
      xlim = c(1, frame_max), ylim = c(0, 200)) + 
    facet_grid(. ~ file) +
    theme_minimal_grid() +
    gginteractive()
}

plot.angle.pile <- function(data, plot.smooth) {
  g <- 
    ggplot(
      data = data, 
      mapping = aes(x = frame, y = angle, color = file)) +
    geom_path(
      alpha = .5)
  
  if (plot.smooth!=-1) {
    g <- g + 
      stat_smooth(
        geom = "smooth", 
        formula = y ~ x, 
        method = "loess", se = F, 
        span = as.numeric(plot.smooth), 
        alpha = .8, linewidth = .5)
  }
  
  g +
    coord_cartesian(
      ylim = c(0, 200)) + 
    theme_minimal_grid() +
    gginteractive()
}