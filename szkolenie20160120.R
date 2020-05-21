library(dplyr)

load("dane.RData")

solve(cor(dane[,3:8]))

dane_u <- dane %>%
  mutate(stopa_bezr=1/stopa_bezr,
         zgony_nowotwor=1/zgony_nowotwor)

dane_z <- scale(dane_u[,3:8])
row.names(dane_z) <- dane$nazwa

# miernik bezwzorcowy

miernik_b <- rowMeans(dane_z)
wynik_b <- as.data.frame(miernik_b)

rank(miernik_b)

# miernik wzorcowy

wzorzec <- apply(dane_z,2,max)

dane_w <- rbind(dane_z, wzorzec)
odl_w <- as.matrix(dist(dane_w, method="euclidean"))

d_i0 <- odl_w[1:35,ncol(odl_w)]
d_0 <- mean(d_i0) + 2*sd(d_i0)

miernik_w <- 1-d_i0/d_0

wynik_w <- as.data.frame(miernik_w)

# metoda Warda

odl <- dist(dane_z, method="euclidean")
ward <- hclust(odl, method = "ward.D")
plot(ward)

grupy <- cutree(ward, k=3)
rect.hclust(ward, k=3, border="red")

dane_s <- dane %>%
  mutate(ward3=grupy)

dane_s %>%
  select(-teryt, -nazwa) %>%
  group_by(ward3) %>%
  summarise_each(funs(mean))
  
# metoda k-średnich

set.seed(7)
ksr <- kmeans(dane_z, 3)

dane_s <- dane_s %>%
  mutate(ksr3=ksr$cluster)

table(dane_s$ward3, dane_s$ksr3)

dane_s %>%
  select(-teryt, -nazwa, -ward3) %>%
  group_by(ksr3) %>%
  summarise_each(funs(mean))