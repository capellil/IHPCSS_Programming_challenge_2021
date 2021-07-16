#!/bin/bash

files=$(ls -v output/*.ppm)
convert -delay 15 ${files} -resize 512x512 output/result.gif
animate -loop 0 output/result.gif
