library(hexSticker)
library(ggplot2)

# Crear el directorio estándar si aún no existe
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# 1. Definir la estructura: Botánica a la izquierda, Reporte a la derecha
tallo <- data.frame(x = 0, y = 0, xend = 0, yend = 10)

hojas <- data.frame(
  x = c(0, 0, 0), y = c(2, 5, 8),
  xend = c(-2.5, -3, -2), yend = c(4, 7, 9.5)
)

reporte <- data.frame(
  x = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
  y = c(2, 3.5, 5, 6.5, 8, 9.5),
  xend = c(3.5, 2.5, 4, 3, 2.8, 3.8),
  yend = c(2, 3.5, 5, 6.5, 8, 9.5)
)

# 2. Construir el gráfico
p_md <- ggplot() +
  geom_segment(data = tallo, aes(x, y, xend=xend, yend=yend), color = "#24292F", linewidth = 1.2) +
  geom_curve(data = hojas, aes(x, y, xend=xend, yend=yend),
             curvature = 0.3, color = "#1A7431", linewidth = 1.5, lineend = "round") +
  geom_segment(data = reporte, aes(x, y, xend=xend, yend=yend),
               color = "#0969DA", linewidth = 1.5, lineend = "round") +
  theme_void() +
  theme_transparent()

sticker(
  subplot = p_md, package = "pacha",
  p_size = 28, p_color = "#24292F", p_y = 1.5,
  s_x = 1, s_y = 0.75, s_width = 1.2, s_height = 1.2,
  h_fill = "#FFFFFA", h_color = "#2EA043",
  url = "github.com/envinatu/pacha",
  u_color = "#57606A",
  u_size = 4.2,
  filename = "man/figures/logo.png"
)









# Dark theme




p_md <- ggplot() +
  geom_segment(data = tallo, aes(x, y, xend=xend, yend=yend), color = "#8B949E", linewidth = 1.2) +
  geom_curve(data = hojas, aes(x, y, xend=xend, yend=yend),
             curvature = 0.3, color = "#3FB950", linewidth = 1.5, lineend = "round") +
  geom_segment(data = reporte, aes(x, y, xend=xend, yend=yend),
               color = "#58A6FF", linewidth = 1.5, lineend = "round") +
  theme_void() +
  theme_transparent()

sticker(
  subplot = p_md, package = "pacha",
  p_size = 28, p_color = "#F0F6FC", p_y = 1.5,
  s_x = 1, s_y = 0.75, s_width = 1.2, s_height = 1.2,
  h_fill = "#0D1117", h_color = "#00E676",
  url = "github.com/envinatu/pacha",
  u_color = "#79C0FF",
  u_size = 4.2,
  filename = "man/figures/logo.png"
)
