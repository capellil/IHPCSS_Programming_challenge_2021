#!/bin/bash

files=$(ls -v outputs/*.ppm)
convert -delay 15 ${files} -resize 512x512 outputs/result.gif
animate -loop 0 outputs/result.gif
