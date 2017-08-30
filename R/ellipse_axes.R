# Working ellipse axes

# Calculate ellipse coords
p <- ggplot() + stat_ellipse(data = freizeitaktivitaeten_16$ind$coord[1:30,] %>% data.frame, aes(Dim.1, Dim.2), segments = 500, type = "norm", level = 0.86)

# Get ellipse coords from plot
pb = ggplot_build(p)
el = pb$data[[1]][c("x","y")]

# Calculate centre of ellipse
ctr = freizeitaktivitaeten_16$ind$coord[1:30,] %>% data.frame %>% summarise_all(funs(mean)) %>% select(x = Dim.1, y = Dim.2) %>% as.matrix %>% as.vector

# Calculate distance to centre from each ellipse pts
dist2center <- sqrt(rowSums(t(t(el)-ctr)^2))

# Identify axes points
df <- data.frame(el, dist2center) %>% arrange(dist2center) %>% slice(c(1, 2, n()-1, n())) %>% mutate(dist2center = round(dist2center, 2))

# Final plot
fviz_mca_ind(freizeitaktivitaeten_16, axes.linetype = "solid") +
  # Ellipse
  stat_ellipse(data = freizeitaktivitaeten_16$ind$coord[1:30,] %>% data.frame, aes(Dim.1, Dim.2), segments = 500, type = "norm", level = 0.86) +
  # Centre
   geom_point(data = freizeitaktivitaeten_16$ind$coord[1:30,] %>% data.frame %>% summarise_all(funs(mean)), aes(Dim.1, Dim.2), size = 5, alpha = 0.7) +
  # Axes
  geom_path(aes(x = x, y = y, group = dist2center), data = df, linetype = "dashed", colour = "red") +
  # fix aspect ratio
  coord_fixed()
