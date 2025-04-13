#Images, Change PDF to Jpg but maintain dpi
# Install if you haven't yet
install.packages("magick")
install.packages("pdftools")

# Load the magick package
library(magick)
library(pdftools)

# Read in the PDF
img <- image_read_pdf("./Images/DSR_Collage.pdf", density = 300)  # <- density controls resolution!

# Save as PNG
image_write(img, path = "./Images/DSR_Collage.png", format = "png")
