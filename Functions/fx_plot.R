fx_plot <-
function (data, plotname = " ", variables_color = 12) {
  ggplot(
    pivot_longer(data,-Date, names_to = "Series", values_to = "Value"),
    aes(x = Date, y = Value, col = Series)
  ) +
    geom_line(linewidth = 1) +
    facet_wrap (. ~ Series, scale = "free") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 10),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 10),
      plot.tag = element_text(size = 10),
      plot.title = element_text(face = "bold")
    ) +
    labs(x = "", y = plotname) +
    scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
}


fx_plot_combined <-
  function (data, plotname = " ", variables_color = 6) {
    ggplot(
      pivot_longer(data,-Date, names_to = "Series", values_to = "Value"),
      aes(x = Date, y = Value, col = Series)
    ) +
      geom_line(linewidth = 1) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 10),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 10),
        legend.title = element_blank(),
        plot.title = element_text(face = "bold")
      ) +
      labs(x = "", y = plotname) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
  }


pivot <- function(data){
  result <- 
  pivot_longer(data,-Date, names_to = "Series", values_to = "Value") 
  result
}

fx_recode_plot <-
  function (data, 
            plotname = " ", 
            variables_color = 12, 
            ncol = NULL,
            nrow = NULL
            ) {
    ggplot(
      data,
      aes(x = Date, y = Value, color = Series)
    ) +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", ncol = ncol, nrow = nrow, labeller = label_parsed) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 10),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 10),
        legend.position = "none"
      ) +
      labs(x = "", y = plotname) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
  
}

fx_recode_group_plot <-
  function (data, 
            plotname = " ", 
            variables_color = 12, 
            ncol = NULL,
            nrow = NULL, 
            color = Category,
            group = Category) {
    ggplot(
      data,
      aes(x = Date, y = Value, color = {{ color }}, group = {{ group }})
    ) +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", labeller = label_parsed, ncol = ncol, nrow = nrow) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8),
        legend.position = "bottom"
      ) +
      labs(x = "", y = plotname, color = NULL) +
      scale_color_manual(values = pnw_palette("Shuksan2", variables_color))
}


fx_nopivot_plot <-
  function (data, plotname = " ", variables_color = 12, scale = "fixed") {
    ggplot(
      data = data,
      aes(x = Date, y = Value, fill = Series)
    ) +
      geom_area() +
      facet_wrap (. ~ Series, scale = scale, labeller = label_parsed) +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8)
      ) +
      labs(x = "", y = plotname) +
      scale_fill_manual(name = "", values = pnw_palette("Shuksan2", variables_color))
  }
