
# Day 1, points

``` r
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ─────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(raster)
```

    ## Loading required package: sp

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
stops <- read_csv("resources/stops.csv")
```

    ## 
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────
    ## cols(
    ##   stop_id = col_double(),
    ##   stop_name = col_character(),
    ##   stop_lat = col_double(),
    ##   stop_lon = col_double(),
    ##   stop_url = col_logical(),
    ##   location_type = col_logical(),
    ##   parent_station = col_logical(),
    ##   lat_swiss = col_double(),
    ##   long_swiss = col_double()
    ## )

``` r
# read canton borders
canton_geo <- read_sf("resources/g2k15.shp")

# read country borders
country_geo <- read_sf("resources/g2l15.shp")

# read lakes
lake_geo <- read_sf("resources/g2s15.shp")
```

``` r
stops <- stops %>%
  mutate(geometry = map2(long_swiss, lat_swiss, c)) %>%
  mutate(geometry = map(geometry, st_point)) %>%
  mutate(geometry = st_as_sfc(geometry)) %>%
  st_as_sf() %>%
  st_set_crs(21781)
```

``` r
stops_swiss <- st_intersection(stops, country_geo)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
relief <- raster("resources/02-relief-ascii.asc") %>%
  # hide relief outside of Switzerland by masking with country borders
  mask(country_geo) %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame() %>%
  dplyr::rename(value = X02.relief.ascii)
```

    ## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
    ## datum Unknown based on Bessel 1841 ellipsoid in CRS definition

    ## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded datum Unknown based on Bessel 1841 ellipsoid in CRS definition,
    ##  but +towgs84= values preserved

    ## Warning in showSRID(SRS_string, format = "PROJ", multiline = "NO"): Discarded
    ## datum CH1903 in CRS definition

    ## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
    ## datum Unknown based on Bessel 1841 ellipsoid in CRS definition
    
    ## Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
    ## datum Unknown based on Bessel 1841 ellipsoid in CRS definition

``` r
ggplot(data = stops) +
  geom_raster(
    data = relief,
    mapping = aes(
      x = x,
      y = y,
      alpha = value
    )
  ) +
  scale_alpha(
    name = "",
    range = c(0.6, 0),
    guide = F
  ) +
  geom_sf(
    data = canton_geo,
    fill = "transparent",
    color = "lightgrey",
    size = 0.5
  ) +
  # draw lakes in light blue
  geom_sf(
    data = lake_geo,
    fill = "#D6F1FF",
    color = "transparent"
  ) +
  geom_sf(data = stops_swiss, alpha = 0.2, color = "#2a5674", size = 0.7) +
  scale_fill_distiller(palette = "Greys") +
  labs(
    title = "Public transport <span style='color:#2a5674'>stops</span> in Switzerland",
    caption = "<span style='color:#636363'>Source: SBB CFF open data</span>"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 8),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    plot.title = ggtext::element_markdown(),
    plot.caption = ggtext::element_markdown(),
    legend.position = "none"
  )
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# ggsave("public_transport.png")
```
