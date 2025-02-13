library(bslib)
bs_theme_preview(theme = sittp_theme)

sittp_theme <- bs_theme(bootswatch = "cerulean") |>
bs_theme_update(bg = "#FFFFFC", fg = "#576B47", 
                primary = "#353E3D", secondary = "#DFFFC7", success = "#00AFE6", 
                info = "#244C24", base_font = font_google("Signika"), heading_font = font_google("Crete Round"), 
                font_scale = NULL, preset = "cerulean")
