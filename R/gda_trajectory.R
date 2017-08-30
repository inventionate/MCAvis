
# Wolken einfärben um Veränderungen anzuzeigen ----------------------------------------
# Farbverlauf als Spannbreite (Punkte behalten EINE Farbe)

time_point_names <- c("Zeitpunkt 1", "Zeitpunkt 2", "Zeitpunkt 3")

coord_all <- get_gda_trajectory(mca_studienalltag)$coord_all
complete_cases <- .select_trajectory(coord_all = coord_all, list(case = "complete"), time_point_names, 1:2)

coord_complete <- coord_all %>% filter(id %in% complete_cases$id)


ind_mean_coord <- coord_complete %>% select(-time) %>% group_by(id) %>% summarise_all(funs(mean))
ind_mean_coord_id <- data.frame(ind_mean_coord)$id

# "within inertia" berechnen (adaptiert von FactoMineR)
tmp <- array(0, dim = c(dim(ind_mean_coord %>% select(-id)), length(time_point_names)))
for(i in seq_along(time_point_names)) {
  tmp[,,i] <- (coord_complete %>% filter(time == time_point_names[i] & id %in% ind_mean_coord_id) %>% select(-id, -time) - ind_mean_coord %>% select(-id))^2 / length(time_point_names)
}
variab.auxil <- apply(tmp,2,sum)
tmp <- sweep(tmp,2,variab.auxil,FUN="/") * 100
inertie.intra.ind <- apply(tmp,c(1,2),sum)
rownames(inertie.intra.ind) <- ind_mean_coord_id
colnames(inertie.intra.ind) <- colnames(coord_complete %>% select(-id, -time))
ind_within_inertia <- inertie.intra.ind

within_inertia <- ind_within_inertia %>% as_tibble %>% rename_all(funs(paste0(., "_inertia")))

coord_ind <- bind_cols(coord_complete %>% filter(time == time_point_names[1]), within_inertia) %>%
  mutate(inertia_plane_12 = Dim.1_inertia + Dim.2_inertia) %>% arrange(desc(inertia_plane_12)) %>%
  slice(12:nrow(.)) %>% select(id, Dim.1, Dim.2, inertia_plane_12) %>% as_tibble

ggplot()+
  geom_point(data = coord_ind, aes(Dim.1, Dim.2, colour = inertia_plane_12), size = 5) +
  theme_base() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + coord_fixed() +
  #scale_color_gradient(low = "lightgreen", high = "darkgreen")
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = max(coord_ind$inertia_plane_12)/2)


# Intergration in die Konzentrationsellipse

fviz_gda_conc_ellipse(mca_studienalltag) +
  geom_point(data = coord_ind, aes(Dim.1, Dim.2, colour = inertia_plane_12), size = 4) +
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = max(coord_ind$inertia_plane_12)/2)


# Veränderungsellipsen!

coord_ind <- bind_cols(coord_complete %>% filter(time == time_point_names[1]), within_inertia) %>%
  mutate(inertia_plane_12 = Dim.1_inertia + Dim.2_inertia) %>% arrange(desc(inertia_plane_12)) %>%
  select(id, Dim.1, Dim.2, inertia_plane_12) %>% mutate(
    inertia_plane_12_group = cut(inertia_plane_12, include.lowest=TRUE, right=TRUE,
                                 breaks=c(0, 3, 20))
  )


# Ellipsenachsen berechnen

coord_ind_mean <- coord_ind %>% data.frame %>% select(-id, -inertia_plane_12) %>% group_by(inertia_plane_12_group) %>% summarise_all(funs(mean))

ellipse_axes <- NULL
inertia_groups <- levels(coord_ind$inertia_plane_12_group)
for(i in seq_along(inertia_groups))
{
  p_calc <- ggplot() + stat_ellipse(data = coord_ind %>% filter(inertia_plane_12_group == inertia_groups[i]), aes(Dim.1, Dim.2), segments = 500, type = "norm", level = 0.86)

  # Get ellipse coords from plot
  pb = ggplot_build(p_calc)
  el = pb$data[[1]][c("x","y")]

  # Calculate centre of ellipse
  ctr = coord_ind_mean %>% filter(inertia_plane_12_group == inertia_groups[i]) %>% select(x = Dim.1, y = Dim.2) %>% as.matrix %>% as.vector

  # Calculate distance to centre from each ellipse pts
  dist2center <- sqrt(rowSums(t(t(el)-ctr)^2))

  # Identify axes points
  df <- bind_cols(el, dist2center = dist2center, inertia_plane_12_group = rep(inertia_groups[i], length(dist2center))) %>% arrange(dist2center) %>% slice(c(1, 2, n()-1, n())) %>% mutate(dist2center = round(dist2center, 2))

  # Store results
  ellipse_axes <- bind_rows(ellipse_axes, df)
}


ggplot() +
  geom_point(data = mca_studienalltag$ind$coord %>% data.frame, aes(Dim.1, Dim.2), alpha = 0.3) +
  # geom_point(data = coord_ind, aes(Dim.1, Dim.2), size = 2) +
  theme_bw() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(panel.grid = element_blank()) + coord_fixed() +
  # Ellipse
  stat_ellipse(data = coord_ind %>% data.frame, aes(Dim.1, Dim.2, group = inertia_plane_12_group, colour = inertia_plane_12_group), segments = 500, type = "norm", level = 0.86) +
  # Centre
  geom_point(data = coord_ind_mean, aes(Dim.1, Dim.2, group = inertia_plane_12_group, colour = inertia_plane_12_group), size = 5, alpha = 0.7) +
  # Axes
  geom_path(data = ellipse_axes, aes(x = x, y = y, group = dist2center, colour = inertia_plane_12_group), linetype = "dashed")




fviz_gda_trajectory_cloud <- function() {

}
#-> für ellipsen & gesamtgruppe. D.h., die Funktion nimmt auch variablen wie quali entgegen oder aber nicht (NULL)

