library(plot3D)


treats_dave <- read_csv("treats_dave.csv")

treats_dave <- treats_dave[which(treats_dave$Block == 1), ]
treats_dave <- aggregate(treats_dave, list(treats_dave$Sc, treats_dave$Hm, treats_dave$Bb), mean)[, c(5:8)]


z <- matrix(rep(c(10000, 4000, 1000, 400, 100), 4), 5, 4)

z1 <- z
z1[, 3] <- c(9500, 3800, 950, 380, 95)
z1[, 2] <- c(8000, 3200, 800, 320, 80)
z1[, 1] <- c(6666, 2666, 666, 266, 66)

z2 <- z
z2[, 3] <- c(9000, 3600, 900, 360, 90)
z2[, 2] <- c(6000, 2400, 600, 240, 60)
z2[, 1] <- c(3333, 1333, 333, 133, 33)


par(mfrow = c(1, 3))

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z, bty = "g", phi = 1, theta = 30,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, yant = "n")

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z1, phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z2, phi = 20,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z, bty = "g", phi = 1, theta = 30,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, yant = "n")

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z1, phi = 20,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z2, phi = 20,
       col = "chocolate1", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)



hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z, bty = "g", phi = 1, theta = 30,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, yant = "n")

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z1, phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 7, length.out = 4), z2, phi = 20,
       col = "coral1", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, add = TRUE)






zBlu <- matrix(rep(c(10000, 4000, 1000, 400, 100), 10), 5, 10)

zCh <- zBlu
zCh[, c(1:8, 10)] <- NA

zCOR <- zBlu
zCOR[, c(1, 3:4, 6, 7, 9, 10)] <- NA


zCh1 <- matrix(NA, 5, 10)

zCh1[, 5] <- c(9500, 3800, 950, 380, 95)
zCh1[, 6] <- c(9500, 3800, 950, 380, 95)

zCh1[, 2] <- c(8000, 3200, 800, 320, 80)
zCh1[, 3] <- c(8000, 3200, 800, 320, 80)

zCh1[, 1] <- c(6666, 2666, 666, 266, 66)




zCOR1 <- matrix(NA, 5, 10)

zCOR1[, 7] <- c(9500, 3800, 950, 380, 95)
zCOR1[, 4] <- c(8000, 3200, 800, 320, 80)

zCOR1[, 1] <- c(3333, 1333, 333, 133, 33)







zCh2 <- matrix(NA, 5, 10)

zCh2[, 7] <- c(9000, 3600, 900, 360, 90)
zCh2[, 4] <- c(6000, 2400, 600, 240, 60)


zCOR2 <- matrix(NA, 5, 10)

zCOR2[, 6] <- c(9000, 3600, 900, 360, 90)
zCOR2[, 3] <- c(6000, 2400, 600, 240, 60)




zBlu2 <- matrix(NA, 5, 10)

zBlu2[, 5] <- c(9000, 3600, 900, 360, 90)
zBlu2[, 2] <- c(6000, 2400, 600, 240, 60)






hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zBlu), bty = "g", phi = 5, theta = 70,
       border = "black", shade = 0.2, ltheta = 90, col = "#0078B0",
       space = 0.4, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, zlim = c(0, 10500), ylim = c(0, 30))

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), zCh, bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "chocolate",
       space = 0.4, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), zCOR, bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "coral3",
       space = 0.4, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zCh1), phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.4, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zCOR1), phi = 20,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.4, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zCh2), phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.4, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zCOR2), phi = 20,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.4, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 10), (zBlu2), phi = 20,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.4, add = TRUE)











zBluN <- matrix(NA, 5, 13)
zBluN[, c(1, 3:5, 7:9, 11:13)] <- c(10000, 4000, 1000, 400, 100)


zCh <- zBluN
zCh[, c(1:10, 11, 13)] <- NA

zCOR <- zBluN
zCOR[, c(1, 4:5, 8, 9, 12, 13)] <- NA


zCh1 <- matrix(NA, 5, 13)

zCh1[, 7] <- c(9500, 3800, 950, 380, 95)
zCh1[, 8] <- c(9500, 3800, 950, 380, 95)

zCh1[, 3] <- c(8000, 3200, 800, 320, 80)
zCh1[, 4] <- c(8000, 3200, 800, 320, 80)

zCh1[, 1] <- c(6666, 2666, 666, 266, 66)




zCOR1 <- matrix(NA, 5, 13)

zCOR1[, 9] <- c(9500, 3800, 950, 380, 95)
zCOR1[, 5] <- c(8000, 3200, 800, 320, 80)

zCOR1[, 1] <- c(3333, 1333, 333, 133, 33)







zCh2 <- matrix(NA, 5, 13)

zCh2[, 9] <- c(9000, 3600, 900, 360, 90)
zCh2[, 5] <- c(6000, 2400, 600, 240, 60)


zCOR2 <- matrix(NA, 5, 13)

zCOR2[, 8] <- c(9000, 3600, 900, 360, 90)
zCOR2[, 4] <- c(6000, 2400, 600, 240, 60)




zBlu2 <- matrix(NA, 5, 13)

zBlu2[, 7] <- c(9000, 3600, 900, 360, 90)
zBlu2[, 3] <- c(6000, 2400, 600, 240, 60)








hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zBluN), bty = "g", phi = 0, theta = 65,
       border = "black", shade = 0.2, ltheta = 90, col = "#0078B0",
       space = 0.2, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, zlim = c(0, 10500), ylim = c(0, 20))


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), zCh, bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "darkgreen",
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), zCOR, bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "gold",
       space = 0.2, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zCh1), phi = 20,
       col = "darkgreen", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zCOR1), phi = 20,
       col = "gold", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zCh2), phi = 20,
       col = "darkgreen", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zCOR2), phi = 20,
       col = "gold", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), (zBlu2), phi = 20,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)






















hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zBluN), bty = "g", phi = 5, theta = 65,
       border = "black", shade = 0.2, ltheta = 90, col = "#0078B0",
       space = 0.2, ticktype = "detailed", facets = TRUE, opaque.top = TRUE, ylim = c(0, 20), zlim = c(1, 4.2))


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCh), bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "chocolate",
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCOR), bty = "g", phi = 1, theta = 30,
       border = "black", shade = 0.2, ltheta = 90, col = "coral3",
       space = 0.2, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCh1), phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCOR1), phi = 20,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)


hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCh2), phi = 20,
       col = "chocolate", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zCOR2), phi = 20,
       col = "coral3", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)

hist3D(x = seq(1, 8, length.out = 5), y = seq(1, 20, length.out = 13), log10(zBlu2), phi = 20,
       col = "#0078B0", border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, add = TRUE)




