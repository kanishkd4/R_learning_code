rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)


### Outputting to PDF vector files
# Two ways to do that. open pdf using pdf(), make the plots and close it with dev.off()
pdf("mypdf.pdf", width = 4, height = 4)
plot(mtcars$wt, mtcars$mpg)
print(ggplot(mtcars, aes(wt, mpg)) + geom_point())
dev.off()
getwd()

# If we make more than one plot, it will go to a different page in the pdf.
# we called print in the ggplot object to make sure that it will output even when this code is in the script
# the width and height are in inches so to specify the dimentions, they must be converted manually
# dev.off must be called to be able to open the pdf created. 
# ggsave can be used to save the last plot made using ggplot2 (can't be used on multiple plots



### Outputting to SVG vector files
# scalable vector graphics
# same way as pdf files
svg("myplot.svg", width=4, height=4)
plot(...)
dev.off()
# With ggsave()
ggsave("myplot.svg", width=8, height=8, units="cm")




### Outputting to WMF files
# Windows metafile image of the plot
# Windows programs support WMF files image of the plot better than pdf's
win.metafile("myplot.wmf", width=4, height=4)
plot(...)
dev.off()
# With ggsave()
ggsave("myplot.wmf", width=8, height=8, units="cm")




### Editing a vector output file
# inkspace(free) or Adobe Illustrator(paid)



### Outputting to bitmap files
# There are two ways to output to PNG bitmap files. One method is to open the PDF
# graphics device with png(), make the plots, then close the device with dev.off(). This
# method works for most graphics in R, including base graphics and grid-based graphics
# like those created by ggplot2 and lattice:


# width and height are in pixels
png("myplot.png", width=400, height=400)
# Make plot
plot(mtcars$wt, mtcars$mpg)
dev.off()

# For outputting multiple plots, put %d in the filename. This will be replaced with 1, 2, 3,
# and so on, for each subsequent plot:

# width and height are in pixels
png("myplot-%d.png", width=400, height=400)
plot(mtcars$wt, mtcars$mpg)
print(ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point())
dev.off()

# Notice that we called print() on the ggplot object to make sure that it will be output even when this code is in a script.
# the default is 72 ppi. use at least 300 for a good print

ppi <- 300
# Calculate the height and width (in pixels) for a 4x4-inch image at 300 ppi
png("myplot.png", width=4*ppi, height=4*ppi, res=ppi)
plot(mtcars$wt, mtcars$mpg)
dev.off()

# If you are creating plots from a script and it throws an error while creating one, R might
# not reach the call to dev.off(), and could be left in a state where the PNG device is still
# open. When this happens, the PNG file won't open properly in a viewing program until
# you manually call dev.off().
# If you are creating a graph with ggplot2, using ggsave() can be a little simpler. It simply
# saves the last plot created with ggplot(). You specify the width and height in inches,
# not pixels, and tell it how many pixels per inch to use:

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
# Default dimensions are in inches, but you can specify the unit
ggsave("myplot.png", width=8, height=8, unit="cm", dpi=300)





### Using fonts in PDF files
# # extrafont package can be used to create pdf files with different fonts
# The extrafont package can be used to create PDF files with different fonts.
# There are a number of steps involved, beginning with some one-time setup. Download
# and install Ghostscript, then run the following in R:
install.packages("extrafont")
library(extrafont)
# Find and save information about fonts installed on your system
font_import()
# List the fonts
fonts()
# After the one-time setup is done, there are tasks you need to do in each R session:
# Register the fonts with R
loadfonts()

y
# On Windows, you may need to tell it where Ghostscript is installed
# (adjust the path to match your installation of Ghostscript)
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")
# Finally, you can create a PDF file and embed fonts into it, as in Figure 14-4:

library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Title text goes here") +
  theme(text = element_text(size = 16, family="Impact"))
ggsave("myplot.pdf", width=4, height=4)
embed_fonts("myplot.pdf")