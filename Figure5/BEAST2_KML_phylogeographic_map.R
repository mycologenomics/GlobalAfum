# ============================================================
# BEAST2 phylogeographic KML: world map with great-circle arcs
# ============================================================

# Required packages:
# install.packages(c("sf", "maps", "ggplot2", "geosphere", "dplyr"))

library(sf)
library(maps)
library(ggplot2)
library(geosphere)
library(dplyr)

# ------------------------------------------------------------
# 1. INPUT FILE
# ------------------------------------------------------------
kml_file <- "Afum_TR34_KML.kml"

# ------------------------------------------------------------
# 2. READ KML
# ------------------------------------------------------------
pts <- st_read(kml_file, quiet = TRUE)
n_pts <- nrow(pts)

if (n_pts == 0) stop("No features were found in the KML file.")

# ------------------------------------------------------------
# 3. EXTRACT LONGITUDE AND LATITUDE
# ------------------------------------------------------------
coords_list <- lapply(seq_len(n_pts), function(i) {
  xy <- st_coordinates(pts[i, ])
  if (is.null(dim(xy))) {
    c(xy[1], xy[2])
  } else {
    c(xy[1, 1], xy[1, 2])
  }
})

coords_by_point <- do.call(rbind, coords_list)
colnames(coords_by_point) <- c("lon", "lat")

pts$lon <- coords_by_point[, "lon"]
pts$lat <- coords_by_point[, "lat"]

# ------------------------------------------------------------
# 4. IDENTIFY UK / NETHERLANDS ORIGINS
# ------------------------------------------------------------
lon <- pts$lon
lat <- pts$lat

is_uk <- (lon > -11 & lon < 2) & (lat > 49 & lat < 61)
is_nl <- (lon > 3 & lon < 7.5) & (lat > 50 & lat < 54)

pts$origin_flag <- ifelse(is_uk | is_nl, "UK_NL", "Other")

# ------------------------------------------------------------
# 5. BUILD ORIGIN-DESTINATION GREAT-CIRCLE ARCS
# ------------------------------------------------------------
# Assumes consecutive points form pairs:
# Point 1 -> Point 2, Point 3 -> Point 4, etc.

n_pairs <- floor(n_pts / 2)
arc_segments <- list()

for (k in seq_len(n_pairs)) {

  i <- 2 * k - 1
  j <- 2 * k

  start <- as.numeric(coords_by_point[i, ])
  end   <- as.numeric(coords_by_point[j, ])

  arc_mat <- geosphere::gcIntermediate(
    start,
    end,
    n = 50,
    addStartEnd = TRUE,
    breakAtDateLine = TRUE
  )

  if (is.list(arc_mat)) {
    arc_mat <- do.call(rbind, arc_mat)
  }

  arc_segments[[k]] <- data.frame(
    x = arc_mat[-nrow(arc_mat), 1],
    y = arc_mat[-nrow(arc_mat), 2],
    xend = arc_mat[-1, 1],
    yend = arc_mat[-1, 2],
    progress = seq(0, 1, length.out = nrow(arc_mat) - 1),
    origin_flag = pts$origin_flag[i],
    arc_id = k
  )
}

arc_segments_df <- do.call(rbind, arc_segments)

# ------------------------------------------------------------
# 6. ARC TRANSPARENCY
# ------------------------------------------------------------
# UK/NL-origin arcs are darker. All arcs fade towards destination.

arc_segments_df$alpha <- ifelse(
  arc_segments_df$origin_flag == "UK_NL",
  0.9 * (1 - arc_segments_df$progress),
  0.3 * (1 - arc_segments_df$progress)
)

# ------------------------------------------------------------
# 7. WORLD BASEMAP
# ------------------------------------------------------------
world_df <- map_data("world")

# ------------------------------------------------------------
# 8. CREATE FIGURE
# ------------------------------------------------------------
p <- ggplot() +

  geom_polygon(
    data = world_df,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    colour = "grey70",
    linewidth = 0.25
  ) +

  geom_segment(
    data = arc_segments_df,
    aes(x = x, y = y, xend = xend, yend = yend, alpha = alpha),
    colour = "#2166ac",
    linewidth = 0.4
  ) +

  scale_alpha_identity() +

  geom_point(
    data = filter(pts, origin_flag == "UK_NL"),
    aes(x = lon, y = lat),
    shape = 21,
    fill = "#2b83ba",
    colour = "#2b83ba",
    size = 0.75,
    stroke = 0.2
  ) +

  geom_point(
    data = filter(pts, origin_flag == "Other"),
    aes(x = lon, y = lat),
    shape = 21,
    fill = "#2b83ba",
    colour = "#2b83ba",
    size = 0.75,
    stroke = 0.2
  ) +

  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 85),
    expand = FALSE
  ) +

  theme_minimal() +

  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.08),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, 1, 1, unit = "pt"),
    legend.position = "none"
  )

# ------------------------------------------------------------
# 9. DISPLAY FIGURE
# ------------------------------------------------------------
print(p)

# ------------------------------------------------------------
# 10. OPTIONAL: SAVE FIGURE
# ------------------------------------------------------------

# High-resolution PNG
# ggsave("Afum_TR34_phylogeography.png",
#        plot = p, width = 9, height = 4.5, dpi = 300)

# Vector PDF
# ggsave("Afum_TR34_phylogeography.pdf",
#        plot = p, width = 9, height = 4.5)
