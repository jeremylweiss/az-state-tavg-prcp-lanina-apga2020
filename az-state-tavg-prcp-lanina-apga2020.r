

# This code generates a scatterplot of November-December average temperature 
# versus total precipitation for Arizona-statewide values, highlighting La Nina
# years, for APGA 2020 conference presentation

# Precipitation and temperature data are from: 
# https://www.ncdc.noaa.gov/cag/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("reshape2")
library("ggplot2")
library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Set time span
yr_start <- 1949
yr_end <- 2019

# Set La Nina years, based on list at:
# https://www.climate.gov/news-features/featured-images/temperature-patterns-during-every-la-ni%C3%B1a-winter-1950
yr_la_nina <- c(
  # strong
  1973, 1988, 1999, 1975, 2007, 1949, 1998,
  # moderate
  1970, 2010, 1955, 1984,
  #weak
  1995, 2005, 2008, 2011, 1954, 1971, 2000, 1964, 1983, 1974
  )

# Set La Nina years, based on list at:
# https://www.climate.gov/news-features/featured-images/temperature-patterns-during-every-la-ni%C3%B1a-winter-1950
yr_la_nina <- c(
  # strong
  1973, 1988, 1999, 1975, 2007, 1949, 1998,
  # moderate
  1970, 2010, 1955, 1984,
  #weak
  1995, 2005, 2008, 2011, 1954, 1971, 2000, 1964, 1983, 1974
)

# Set El Nino years, based on list at:
# https://www.climate.gov/news-features/featured-images/us-winter-precipitation-during-every-el-ni%C3%B1o-1950
yr_el_nino <- c(
  # strong
  2015, 1982, 1997, 1957, 1972, 1991,
  # moderate
  2009, 1965, 1986, 1963, 1968, 1994,
  #weak
  2002, 1953, 1987, 1976, 1977, 2006, 1958, 1979, 2004, 2014, 1951, 1969
)


# LOAD AND TRANSFORM MONTHLY DATA --------------------


# Data already are November-December values
read.csv(file = "2-prcp-2-12-1949-2019.csv", header = FALSE, skip = 0, nrows = 4)
prcp <- read.csv(file = "2-prcp-2-12-1949-2019.csv", header = FALSE, skip = 4,
                 sep = ",")
colnames(prcp) <- c("YearMonth", "Value")

read.csv(file = "2-tavg-2-12-1949-2019.csv", header = FALSE, skip = 0, nrows = 4)
tavg <- read.csv(file = "2-tavg-2-12-1949-2019.csv", header = FALSE, skip = 4,
                sep = ",")
colnames(tavg) <- c("YearMonth", "Value")

df4plot <- as.data.frame(seq(from = yr_start, to = yr_end, by = 1))
df4plot <- cbind(df4plot, prcp$Value, tavg$Value)
colnames(df4plot) <- c("Year", "Prcp", "Tavg")

# Mark La Nina, El Nino, and Neutral years in record
df4plot["ENSO"] <- NA
for (yr in 1:length(df4plot$Year)) {
  if (df4plot$Year[yr] %in% yr_la_nina == TRUE) {
    df4plot$ENSO[yr] <- "La Niña years"
  #} else if (df4plot$Year[yr] %in% yr_el_nino == TRUE) {
  #  df4plot$ENSO[yr] <- "El Niño"
  #} else {
  #  df4plot$ENSO[yr] <- "Neutral"
  #}
  } else {
    df4plot$ENSO[yr] <- "non-La Niña years"
  }
}
rm(yr)


# MAKE AND SAVE SCATTERPLOT --------------------

prcp_clim <- mean(filter(df4plot, Year >= 1981 & Year <= 2010)$Prcp)
tavg_clim <- mean(filter(df4plot, Year >= 1981 & Year <= 2010)$Tavg)

df4plot$ENSO <- factor(df4plot$ENSO, 
                       levels = c("La Niña years", "non-La Niña years"))

category_colors <- c("La Niña years" = "#0000FF", 
                     "non-La Niña years" = "gray40")

quadrant_centers <- c(
  # Wet
  mean(c(prcp_clim, max(df4plot$Prcp, na.rm = TRUE))),
  # Dry
  mean(c(prcp_clim, min(df4plot$Prcp, na.rm = TRUE))),
  # Warm
  mean(c(tavg_clim, max(df4plot$Tavg, na.rm = TRUE))),
  # Cool
  mean(c(tavg_clim, min(df4plot$Tavg, na.rm = TRUE)))
)

# Create a ggplot object for graphing daily maximum temperature values
p <- ggplot(data = df4plot, mapping = aes(x = Tavg, y = Prcp, color = ENSO)) +
  
  # Normals -----
  
  # Tavg
  geom_vline(xintercept = tavg_clim, color = "gray80", size = 1.25, 
             show.legend = FALSE) +
  
  geom_text(aes(
    x = tavg_clim + .125, y = max(Prcp, na.rm = TRUE), label = "1981-2010 normal", 
    family = "Source Sans Pro", fontface = "plain", hjust = "right", 
    vjust = "top"
    ),
    size = 10 / .pt, color = "gray80", angle = 90, show.legend = FALSE) +
  
  # Prcp
  geom_hline(yintercept = prcp_clim, color = "gray80", size = 1.25,
             show.legend = FALSE) +
  
  geom_text(aes(
    x = max(Tavg, na.rm = TRUE), y = prcp_clim - .15, label = "1981-2010 normal", 
    family = "Source Sans Pro", fontface = "plain", hjust = "right", 
    vjust = "top"
    ),
    size = 10 / .pt, color = "gray80", angle = 0, show.legend = FALSE) +
  
  # Quadrants ----- 
  
  geom_text(aes(
    x = quadrant_centers[4], y = quadrant_centers[1], label = "WET AND COOL",
    family = "Source Sans Pro", fontface = "bold", hjust = "center", 
    vjust = "center"
    ),
    size = 12 / .pt, color = "gray80", angle = 0, show.legend = FALSE) +
  geom_text(aes(
    x = quadrant_centers[3], y = quadrant_centers[1], label = "WET AND WARM",
    family = "Source Sans Pro", fontface = "bold", hjust = "center", 
    vjust = "center"
  ),
  size = 12 / .pt, color = "gray80", angle = 0, show.legend = FALSE) +
  geom_text(aes(
    x = quadrant_centers[3], y = quadrant_centers[2], label = "DRY AND WARM",
    family = "Source Sans Pro", fontface = "bold", hjust = "center", 
    vjust = "center"
  ),
  size = 12 / .pt, color = "gray80", angle = 0, show.legend = FALSE) +
  geom_text(aes(
    x = quadrant_centers[4], y = quadrant_centers[2], label = "DRY AND COOL",
    family = "Source Sans Pro", fontface = "bold", hjust = "center", 
    vjust = "center"
  ),
  size = 12 / .pt, color = "gray80", angle = 0, show.legend = FALSE) +
  
  # Data points -----
  
  geom_point(alpha = 0.5, size = 4, stroke = 0.1) +
  
  scale_color_manual(values = category_colors, aesthetics = c("color", "fill")) +
  
  # Axes -----
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("November-December Precipitation and Temperature") +
  labs(subtitle = "Arizona statewide values, 1949-2019",
       x = "\nAverage Temperature (°F)",
       y = "Total Precipitation (inches)\n",
       caption = paste0(
         "\ndata source: NOAA NCEI (www.ncdc.noaa.gov/cag)",
         "\nLa Niña years: NOAA (www.climate.gov/enso)")
       ) +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = seq(from = min(df4plot$Tavg, na.rm = TRUE), 
                 to = max(df4plot$Tavg, na.rm = TRUE),
                 by = 1),
    limits = c(min(df4plot$Tavg, na.rm = TRUE), 
               max(df4plot$Tavg, na.rm = TRUE)),
    expand = c(0.01, 0.01)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = max(df4plot$Prcp, na.rm = TRUE), by = 1),
    limits = c(0, max(df4plot$Prcp, na.rm = TRUE)),
    expand = c(0.02, 0.03)
  ) +
  
  # Further customize the figure appearance -----
  
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.spacing = unit(1.0, 'mm'),
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 8),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 12)), 
        plot.title = (
          element_text(face = "bold", family = "Source Serif Pro", size = 16)
        ),
        plot.title.position = "plot")
# -----
p

#  Save the figure as a .png file in the current directory
ggsave(file = paste0("az-state-tavg-prcp-lanina-apga2020-",
                    Sys.Date(),
                    ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 5, units = "in", dpi = 300)

