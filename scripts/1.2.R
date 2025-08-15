# R Version: 4.3.1
# Required packages: grid, gridExtra

library(grid)
library(gridExtra)

# Initialize canvas
grid.newpage()

# Function to draw a rounded rectangle with text
draw_box <- function(label, x, y, box_width = 0.2, box_height = 0.08, gp_box, gp_text) {
  grid.roundrect(x = x, y = y,
                 width = box_width, height = box_height,
                 r = unit(0.05, "snpc"),
                 gp = gp_box)
  grid.text(label, x = x, y = y, gp = gp_text)
}

# Coordinate settings
gp_input <- gpar(fill = "#c6dbef", col = "black", lwd = 2)
gp_mid <- gpar(fill = "#fee0d2", col = "black", lwd = 2)
gp_indicator <- gpar(fill = "#c7e9c0", col = "black", lwd = 2)
gp_output <- gpar(fill = "#fcbba1", col = "black", lwd = 2)
gp_text <- gpar(fontsize = 11)

# Draw input boxes
draw_box("Sward Type", x = 0.2, y = 0.85, gp_box = gp_input, gp_text = gp_text)
draw_box("Management", x = 0.2, y = 0.65, gp_box = gp_input, gp_text = gp_text)

# Draw middle layer
draw_box("Ecosystem\nFunctions", x = 0.5, y = 0.75, gp_box = gp_mid, gp_text = gp_text)

# Draw indicator boxes
draw_box("Soil Quality", x = 0.8, y = 0.85, gp_box = gp_indicator, gp_text = gp_text)
draw_box("Biodiversity", x = 0.8, y = 0.65, gp_box = gp_indicator, gp_text = gp_text)

# Draw output
draw_box("DMY\n(Yield)", x = 0.8, y = 0.40, gp_box = gp_output, gp_text = gp_text)

# Draw arrows
arrow_gp <- gpar(col = "black", lwd = 1.5)

grid.lines(x = unit(c(0.3, 0.45), "npc"), y = unit(c(0.85, 0.775), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)
grid.lines(x = unit(c(0.3, 0.45), "npc"), y = unit(c(0.65, 0.725), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)
grid.lines(x = unit(c(0.55, 0.75), "npc"), y = unit(c(0.78, 0.86), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)
grid.lines(x = unit(c(0.55, 0.75), "npc"), y = unit(c(0.72, 0.67), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)
grid.lines(x = unit(c(0.8, 0.8), "npc"), y = unit(c(0.82, 0.44), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)
grid.lines(x = unit(c(0.8, 0.8), "npc"), y = unit(c(0.62, 0.44), "npc"), arrow = arrow(type = "closed"), gp = arrow_gp)

# Title
grid.text("Figure 1.2: Conceptual Framework – Ecosystem Pathways to DMY", 
          x = 0.5, y = 0.96, gp = gpar(fontsize = 14, fontface = "bold"))
