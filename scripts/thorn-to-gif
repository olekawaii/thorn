#! /bin/sh

thorn-to-ppm

mkdir ppms
mv *.ppm ppms

fps=$1

if [ -z "$fps" ]; then
    fps=10
fi

ffmpeg -i ppms/output%05d.ppm -vf palettegen=max_colors=8 palette.png
ffmpeg -framerate $fps -i ppms/output%05d.ppm -i palette.png -lavfi "paletteuse=dither=none" output.gif
rm -r ppms palette.png 

