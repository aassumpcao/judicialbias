gg_outlier_bin <- function(x, var_name, cut_off_floor, cut_off_ceiling,
  col = 'black', fill = 'cornflowerblue', fill_outlier_bins = 'forestgreen',
  binwidth = NULL){

  printing_min_max <- x %>%
    summarize(
      sprintf('round(min(%s, na.rm = TRUE), 3)', var_name),
      sprintf('round(max(%s, na.rm = TRUE), 3)', var_name)
    )

  ceiling_filter <- ifelse(
    !is.na(cut_off_ceiling),
    sprintf('%s < %f', var_name, cut_off_ceiling),
    '1 == 1'
  )
  floor_filter <- ifelse(
    !is.na(cut_off_floor),
    sprintf('%s > %f', var_name, cut_off_floor),
    '1 == 1'
  )

  x_regular <- x %>%
    filter(
      !!rlang::parse_expr(ceiling_filter) & !!rlang::parse_expr(floor_filter)
    ) %>%
    select(var_name)

  x_to_roll_ceiling <- x %>%
    filter(
      !!rlang::parse_expr(sprintf('%s >= %f', var_name, cut_off_ceiling))
    ) %>%
    select(var_name)

  if (!is.na(cut_off_ceiling)) {x_to_roll_ceiling[, 1] <- cut_off_ceiling}

  x_to_roll_floor <- x %>%
    filter(!!rlang::parse_expr(sprintf('%s <= %f', var_name, cut_off_floor)))%>%
    select(var_name)

  if (!is.na(cut_off_floor)) {x_to_roll_floor[, 1] <- cut_off_floor}

  plot_obj <- ggplot(x_regular, aes_string(var_name)) +
              geom_histogram(col = col, fill = fill, binwidth = binwidth)

  if (!is.na(cut_off_ceiling)) {
    ticks_for_ceiling <- update_tickmarks_ceiling(plot_obj,
      cut_off_ceiling, printing_min_max[1, 2])
    plot_obj <- plot_obj + geom_histogram(data = x_to_roll_ceiling,
      fill = fill_outlier_bins, col = col, binwidth = binwidth) +
      scale_x_continuous(breaks = ticks_for_ceiling$tick_positions,
        labels = ticks_for_ceiling$tick_labels)
  }

  if (!is.na(cut_off_floor)) {
    ticks_for_floor <- update_tickmarks_floor(plot_obj,
      cut_off_floor, printing_min_max[1, 1])
    plot_obj <- plot_obj + geom_histogram(data = x_to_roll_floor,
      fill = fill_outlier_bins, col = col, binwidth = binwidth) +
      scale_x_continuous(breaks = ticks_for_floor$tick_positions,
        labels = ticks_for_floor$tick_labels)
  }

  return(plot_obj)
}


update_tickmarks_ceiling <- function(gg_obj, co, max_print) {
  ranges <- suppressMessages(ggplot_build(gg_obj)$layout$panel_params[[1]])
  label_to_add <- sprintf('(%s , %s)', round(co, 3), max_print)
  tick_positions <- ranges$x.major_source
  tick_labels <- ranges$x.labels
  if (overlap_ceiling(tick_positions, co)) {
    tick_positions <- tick_positions[-length(tick_positions)]
    tick_labels <- tick_labels[-length(tick_labels)]
  }
  return(list(tick_positions = c(tick_positions, co),
              tick_labels = c(tick_labels, label_to_add)))
}

overlap_ceiling <- function(positions, cut_off) {
  n <- length(positions)
  ticks_dif <- positions[n] - positions[n - 1]
  (cut_off - positions[n])/ticks_dif < 0.25
}

update_tickmarks_floor <- function(gg_obj, co, min_print) {
  ranges <- suppressMessages(ggplot_build(gg_obj)$layout$panel_params[[1]])
  label_to_add <- sprintf('(%s , %s)', min_print, round(co, 3))
  tick_positions <- ranges$x.major_source
  tick_labels <- ranges$x.labels
  if (overlap_floor(tick_positions, co)) {
    tick_positions <- tick_positions[-1]
    tick_labels <- tick_labels[-1]
  }
  return(list(tick_positions = c(co, tick_positions),
              tick_labels = c(label_to_add, tick_labels)))
}

overlap_floor <- function(positions, cut_off) {
  ticks_dif <- positions[2] - positions[1]
  (positions[1] - cut_off)/ticks_dif < 0.25
}
