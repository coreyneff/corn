theme_corn <- function(base_size = 12, base_family = "", ...){
    ggplot2::"%+replace%"(
      ggplot2::theme_bw(base_size = base_size, base_family = base_family),
      ggplot2::theme(line = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                     strip.background = ggplot2::element_rect(fill = "gray95", color = "black"),
                     ...)
    )
}
