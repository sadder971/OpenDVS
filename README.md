# OpenDVS


## Introduction
This is an open-source reimplementation of the Data visualization Saliency (DVS) model cited below. It's designed to help evaluate where viewers are most likely to focus their attention in data visualisations.

```
Matzen, L. E., Haass, M. J., Divis, K. M., Wang, Z., & Wilson, A. T. (2017). Data visualization saliency model: A tool for evaluating abstract data visualizations. IEEE Transactions on Visualization and Computer Graphics, 24(1), 563-573.
```

## Setup Environment
R Environment: Install R from [CRAN](https://cran.r-project.org).

Python Environment: The setup function installs Python and necessary packages or install Python from [python.org](https://www.python.org).

```
# Load and install packages in R
files <- list.files(pattern = "[.]R$")
for (i in files) {
	source(i)
}
	

# Setup Python environment and load Python scripts
setup()
source_python("text_saliency.py")
```

## Usage

```
# Use the dvs() function with the path to your visualisation to generate a DVS saliency map.
map <- dvs("targets/whoQ41_1.png")

# Visualise the Saliency Maps
grid.newpage()
grid.raster(as.matrix(map$master_map_resized), interpolate = FALSE)


# Generate the Itti-Koch saliency map
ittikoch_map <- ittikochmap("targets/whoQ41_1.png")

# Display the Itti-Koch saliency map
grid.newpage()
grid.raster(as.matrix(ittikoch_map$master_map_resized), 
            interpolate = FALSE)

# Generate the text saliency map
textSaiency_map <- textSaliency("targets/whoQ41_1.png")

# Display the text saliency map
grid.newpage()
grid.raster(textSaiency_map, 
            interpolate = FALSE)

# Compare with Eye-Tracking Data
csv_files <- list.files("fixationsByVis/whoQ41_1/enc", 
                        pattern = "[.]csv$", full.names = TRUE)

read_fixations <- function(file_path) {
  fixations <- read.csv(file_path, header = FALSE)
  return(data.frame(X = fixations[,2], Y = fixations[,3]))
}

all_fixations <- do.call(rbind, 
                         lapply(csv_files, read_fixations))

X <- all_fixations$X
Y <- all_fixations$Y

img <- readPNG("targets/whoQ41_1.png")
origimgsize <- dim(img)

score <- rocScoreSaliencyVsFixations(map$master_map_resized, 
                                     X, Y, origimgsize)
score
```
