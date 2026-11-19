# Slide background in the style of the tidymodels hex logo:
# the dark background and coloured curves, without the hex frame or the name.
#
# The curves are traced from images/hex/tidymodels.png (2521 x 2911 px):
# each column holds the height of one curve at the x positions in `x`,
# in pixels of the logo. Colours are sampled from the logo.

curves <- data.frame(
  x =         seq(100, 2400, by = 100),
  brown =     c(1368, 1367, 1360, 1342, 1315, 1263, 1195, 1121, 1036,  958,  903,  882,  901,  952, 1023, 1104, 1181, 1245, 1289, 1318, 1340, 1353, 1360, 1363),
  red =       c(1295, 1291, 1279, 1258, 1226, 1183, 1128, 1062,  991,  909,  862,  858,  891,  953, 1036, 1133, 1237, 1336, 1413, 1474, 1519, 1549, 1567, 1579),
  lightblue = c(1300, 1293, 1282, 1268, 1243, 1201, 1143, 1071,  994,  923,  871,  852,  878,  940, 1023, 1112, 1193, 1257, 1332, 1397, 1446, 1477, 1484, 1481),
  green =     c(1639, 1590, 1527, 1453, 1371, 1283, 1190, 1099, 1015,  944,  894,  874,  890,  937, 1003, 1078, 1154, 1221, 1269, 1298, 1315, 1326, 1334, 1342),
  orange =    c(1312, 1306, 1297, 1281, 1255, 1215, 1158, 1085, 1008,  938,  890,  876,  901,  956, 1032, 1117, 1199, 1274, 1335, 1379, 1406, 1420, 1426, 1430),
  purple1 =   c(2186, 2000, 1795, 1606, 1442, 1309, 1194, 1085,  988,  911,  859,  852,  890,  959, 1048, 1140, 1229, 1315, 1393, 1452, 1491, 1515, 1529, 1538),
  yellow =    c(1874, 1783, 1680, 1576, 1471, 1367, 1263, 1160, 1059,  966,  892,  851,  856,  908,  985, 1074, 1155, 1225, 1288, 1335, 1361, 1374, 1378, 1381),
  purple2 =   c(1267, 1267, 1264, 1254, 1231, 1194, 1142, 1076, 1000,  920,  855,  828,  865,  953, 1049, 1142, 1228, 1303, 1363, 1410, 1442, 1465, 1482, 1497),
  navy =      c(1408, 1397, 1379, 1350, 1305, 1247, 1176, 1098, 1016,  937,  876,  851,  872,  928,  999, 1069, 1128, 1173, 1201, 1217, 1226, 1230, 1232, 1234)
)

# Drawn in this order, so later curves lie on top, as in the logo.
colors <- c(
  brown = "#4E2D27",
  red = "#DB1F26",
  lightblue = "#00A5DC",
  green = "#36A85D",
  orange = "#F7A238",
  purple1 = "#B57AB4",
  yellow = "#FFF34D",
  purple2 = "#B57AB4",
  navy = "#164F86"
)
background_color <- "#141227"

# In the logo, most curves are see-through where they cross other curves,
# but show their full colour against the background.
opacity_over_curves <- c(
  brown = 0.75,
  red = 0.75,
  lightblue = 0.75,
  green = 0.75,
  orange = 0.75,
  purple1 = 1,
  yellow = 0.75,
  purple2 = 1,
  navy = 1
)

width <- 1920
height <- 1080
peak_from_top <- 280

# Stretch the traced x range to the full width, with the same scale for y,
# and shift the curves so the highest point sits at peak_from_top.
scale <- width / diff(range(curves$x))
to_x <- function(x) (x - min(curves$x)) * scale
to_y <- function(y) (y - min(curves[names(colors)])) * scale + peak_from_top

# Draw one curve in white on black and read it back: the result is a matrix
# with the share of each pixel covered by the curve.
curve_coverage <- function(curve) {
  file <- tempfile(fileext = ".png")
  png(file, width = width, height = height, bg = "black")
  par(mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(0, width), ylim = c(height, 0), xaxs = "i", yaxs = "i")
  x_grid <- seq(-20, width + 20, length.out = 1000)
  curve_fun <- splinefun(to_x(curves$x), to_y(curves[[curve]]), method = "natural")
  lines(x_grid, curve_fun(x_grid), col = "white", lwd = 19)
  dev.off()
  png::readPNG(file)[, , 1]
}

# Paint the curves one by one onto the background: in full colour where
# nothing has been painted yet, see-through where they cross earlier curves.
image <- array(0, dim = c(height, width, 3))
background_rgb <- col2rgb(background_color)[, 1] / 255
for (channel in 1:3) image[, , channel] <- background_rgb[channel]
painted <- matrix(0, height, width)

for (curve in names(colors)) {
  coverage <- curve_coverage(curve)
  alpha <- coverage * (1 - (1 - opacity_over_curves[[curve]]) * painted)
  curve_rgb <- col2rgb(colors[[curve]])[, 1] / 255
  for (channel in 1:3) {
    image[, , channel] <- alpha * curve_rgb[channel] + (1 - alpha) * image[, , channel]
  }
  painted <- pmax(painted, coverage)
}

png::writePNG(image, here::here("images/tidymodels-background.png"))
