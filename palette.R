# Attempt at R code that gets run by a button in an HTML web page


# no idea if this will be necessary but keeping as placeholder
#install.packages('tidyverse')
#library(tidyverse)




# First define this helper function - maybe?
rgb255 <- function(col_vec) {
	hex <- rgb(col_vec[1], col_vec[2], col_vec[3], maxColorValue = 255)
}


# From old: set RGB ordered triple

# orig_col <- c(220, 70, 255)
# orig_col <- c(180, 255, 20)
# orig_col <- c(255, 200, 120)




generate_palettes <- function(r, g, b) {

# just defining this since it's what I wrote all this in terms of originally - prob not best way to do it long run but just want to be able to test asap
orig_col <- c(r,g,b)



# Define min,med,max as well as the positions of orig_col holding each of these (needed later)

sorted_vector <- sort(orig_col)
nmin <- sorted_vector[1]
nmed <- sorted_vector[2]
nmax <- sorted_vector[3]

# Define vars for which channels hold the min,med,max values

maxc <- which.max(c(r,g,b))
minc <- which.min(c(r,g,b))
medc = 6 - maxc - minc

# Define these because they will be useful

white_8bit <- c(255,255,255)
max_plus_min <- rep(nmin+nmax, 3)
rotate <- matrix(c(0,1,0, 0,0,1, 1,0,0), nrow=3, ncol=3)




# Make hex code for original color
orig_hex <- rgb255(orig_col)

# Make hex codes for the complementary color per each of the two models
comp1_hex <- rgb255(white_8bit - orig_col)
comp2_hex <- rgb255(max_plus_min - orig_col)

# Make hex codes for the triadic colors, first suggested model
tri1a_hex <- rgb255(orig_col %*% rotate)
tri1b_hex <- rgb255((orig_col %*% rotate) %*% rotate)

# Triadic colors, second suggested model (this is weird and convoluted idk)
orig_col_alt <- orig_col
orig_col_alt[medc] <- nmin+nmax-nmed

if ((maxc-minc) %% 3 == 1) {
	tri2a_hex <- rgb255((orig_col_alt %*% rotate) %*% rotate) # brg first and with alt
	tri2b_hex <- rgb255(orig_col %*% rotate)
} else {
	tri2a_hex <- rgb255(orig_col_alt %*% rotate) # gbr first and with alt
	tri2b_hex <- rgb255((orig_col %*% rotate) %*% rotate)
}

# Make hex codes for analogous colors (it only gave me one model and I'm kind of skeptical of it tbh? Will see how it looks)
orig_col_analog1 <- orig_col
orig_col_analog1[medc] <-0.5*(nmax+nmed)
analog1_hex <- rgb255(orig_col_analog1)

orig_col_analog2 <- orig_col
orig_col_analog2[medc] <-0.5*(nmin+nmed)
analog2_hex <- rgb255(orig_col_analog2)
# yep that's terrible, med was already much closer to max so one of them is barely different
# go and fix

# Make hex codes for 4-color palette - making first one 90 degrees away
orig_col_sq1 <- orig_col
orig_col_sq1[minc] <- 0.5*(nmin+nmed)
orig_col_sq1[medc] <- nmax
orig_col_sq1[maxc] <- nmin + 0.5*(nmax-nmed)

# model 1 version of inverting
sq1a_hex <- rgb255(orig_col_sq1)
sq1b_hex <- rgb255(white_8bit - orig_col)
sq1c_hex <- rgb255(white_8bit - orig_col_sq1)

# model 2 version of inverting
sq2a_hex <- sq1a_hex
sq2b_hex <- rgb255(max_plus_min - orig_col)
sq2c_hex <- rgb255(max_plus_min - orig_col_sq1)



# Make each palette into a string connected with commas

comp1_str <- paste(c(orig_hex, comp1_hex), collapse=", ")
comp2_str <- paste(c(orig_hex, comp2_hex), collapse=", ")
tri1_str <- paste(c(orig_hex, tri1a_hex, tri1b_hex), collapse=", ")
tri2_str <- paste(c(orig_hex, tri2a_hex, tri2b_hex), collapse=", ")
analog_str <- paste(c(orig_hex, analog1_hex, analog2_hex), collapse=", ")
sq1_str <- paste(c(orig_hex, sq1a_hex, sq1b_hex, sq1c_hex), collapse=", ")
sq2_str <- paste(c(orig_hex, sq2a_hex, sq2b_hex, sq2c_hex), collapse=", ")


# Combine them into a single string

output <- paste(c(comp1_str, comp2_str, tri1_str, tri2_str, analog_str, sq1_str, sq2_str), collapse=" / ")

}



#print(rgb255(c(r,g,b)))

















