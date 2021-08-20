library(tidyverse)

source("functions.R")

create_art <- with_seed(function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  df <- generate_ribbons(n_ribbons = n_ribbons, n_points = n_points, min_alpha = min_alpha, max_alpha = max_alpha)
  
  ggplot(df, aes(x, ymin = ymin, ymax = ymax, fill = group, alpha = I(alpha))) +
    geom_ribbon() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white")
    )
})


p <- create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 6164)

scale_fill_random <- function(...) {
  max_palette_colors <- c(RColorBrewer:::divnum, RColorBrewer:::qualnum, RColorBrewer:::seqnum)
  palettes <- c(
    imap(max_palette_colors, RColorBrewer::brewer.pal),
    ggplot = list(scales::hue_pal()(12))
  )
  
  palette <- sample(palettes, 1) %>% unlist() %>% unname()
  palette_fn <- function(n) sample(palette, n, replace = TRUE)
  
  discrete_scale(
    aesthetics = "fill",
    scale_name = "random",
    palette_fn,
    ...
  )
}

p + scale_fill_random()

scale_fill_random <- function(...) {
  max_palette_colors <- c(RColorBrewer:::divnum, RColorBrewer:::qualnum, RColorBrewer:::seqnum)
  palettes <- c(
    imap(max_palette_colors, RColorBrewer::brewer.pal),
    ggplot = list(scales::hue_pal()(12))
  )
  
  palette <- sample(palettes, 1) %>% unlist()
  palette_fn <- function(n) sample(palette, n, replace = TRUE)
  palette_fn <- scales::brewer_pal("seq", 1, 1) # TEST
  palette_fn <- function (n) { pal = scales:::pal_name(1, "seq")
    if (n < 3) {
        pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    }
    else {
        pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    pal
}
  
  discrete_scale(
    aesthetics = "fill",
    scale_name = "random",
    palette_fn,
    ...
  )
}

# p + scale_fill_brewer()
p + scale_fill_random()


scale_fill_brewer
scales::brewer_pal
scales::brewer_pal("seq", 1, 1)



discrete_scale(1)


scales::hue_pal()(12)

ggplot2::discrete_scale

  
max_palette_colors <- c(RColorBrewer:::divnum, RColorBrewer:::qualnum, RColorBrewer:::seqnum)


palettes <- c(
  imap(max_palette_colors, RColorBrewer::brewer.pal),
  ggplot = list(scales::hue_pal()(12))
)
scales::hue_pal()(1000)

scales::show_col(scales::hue_pal()(1000))
getAnywhere("brewer_pal")

scales::brewer_pal


pal_f <- RColorBrewer::brewer.pal(12, "Paired") 
pal_f

coul = colorRampPalette(coul)(ngroup)
coul=coul[sample(c(1:length(coul)) , size=length(coul) ) ]

RColorBrewer::display.brewer.all()

RColorBrewer::display.brewer.all

palette_names <- c(RColorBrewer:::divlist, RColorBrewer:::quallist, RColorBrewer:::seqlist)
max_colors <- c(RColorBrewer:::divnum, RColorBrewer:::qualnum, RColorBrewer:::seqnum)

palette_names %>% unname()
max_colors
names(max_colors) == palette_names


names(RColorBrewer:::divlist)
names(RColorBrewer:::divlist) ==

totallist 

# Seed the pallete

RColorBrewer:::gaplist




function (n = NULL, type = "all", select = NULL, exact.n = TRUE, 
    colorblindFriendly = FALSE) 
{
    gaplist <- ""
    totallist <- c(divlist, gaplist, quallist, gaplist, seqlist)
    names(totallist) <- c(names(divlist), "gap1", names(quallist), 
        "gap2", names(seqlist))
    gapnum <- max(c(divnum, qualnum, seqnum))
    totnum <- c(divnum, gapnum, qualnum, gapnum, seqnum)
    names(totnum) <- names(totallist)
    if (!(type %in% c("div", "qual", "seq", 
        "all"))) {
        stop(paste(type, "is not a valid name for a color list\n"))
    }
    colorlist <- switch(type, div = divlist, qual = quallist, 
        seq = seqlist, all = totallist)
    maxnum <- switch(type, div = divnum, qual = qualnum, seq = seqnum, 
        all = totnum)
    if (!is.null(select)) {
        colorlist <- colorlist[select]
        maxnum <- maxnum[select]
        if (any(is.na(colorlist))) 
            stop(paste("Illegal value(s) of select: ", 
                paste(select[is.na(colorlist)], collapse = " ")))
    }
    if (colorblindFriendly) {
        colorlist <- colorlist[names(colorlist) %in% c(colorblindlist, 
            "gap1", "gap2")]
        maxnum <- maxnum[names(maxnum) %in% c(colorblindlist, 
            "gap1", "gap2")]
    }
    palattr <- switch(type, qual = "qualitative", div = "divergent", 
        seq = "sequential", all = "qualitative+divergent+sequential")
    if (is.null(n)) 
        n <- maxnum
    if (length(n) == 1) 
        n <- rep(n, length(colorlist))
    if (exact.n) {
        keep <- n <= maxnum
        colorlist <- colorlist[keep]
        n <- n[keep]
        maxnum <- maxnum[keep]
    }
    if (any(n < 3) | exact.n & any(n > maxnum) | length(n) != 
        length(colorlist)) {
        warning("Illegal vector of color numbers")
        print(paste(n, collapse = " "))
    }
    n[n < 3] <- 3
    n[n > maxnum] <- maxnum[n > maxnum]
    nr <- length(colorlist)
    nc <- max(n)
    ylim <- c(0, nr)
    oldpar <- par(mgp = c(2, 0.25, 0))
    on.exit(par(oldpar))
    plot(1, 1, xlim = c(0, nc), ylim = ylim, type = "n", 
        axes = FALSE, bty = "n", xlab = "", ylab = "")
    for (i in 1:nr) {
        nj <- n[i]
        if (colorlist[i] == "") 
            next
        shadi <- brewer.pal(nj, colorlist[i])
        rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj, 
            ytop = i - 0.2, col = shadi, border = "light grey")
    }
    text(rep(-0.1, nr), (1:nr) - 0.6, labels = colorlist, xpd = TRUE, 
        adj = 1)
}