#!/bin/bash

read -r -d '' render_marker <<'PYTHON'
# by p2or on Blender StackExchange
# https://blender.stackexchange.com/questions/23121
import bpy
import os

# get the scene
scn = bpy.context.scene

# get the output path
output_path = scn.render.filepath

# iterate through markers and render
for k, m in scn.timeline_markers.items():
    frame = m.frame
    marker_name = m.name
    scn.frame_set(frame)
    scn.render.filepath = os.path.join(output_path, marker_name + ".jpg")
    bpy.ops.render.render( write_still=True )

bpy.context.scene.render.filepath = output_path
PYTHON

svg2png () {
    inkscape "$1" --export-filename "${1%.*}".png 2>/dev/null >/dev/null
}; export -f svg2png

mmpz2wav () {
    lmms --format wav --render "$1" >/dev/null 2>/dev/null
}; export -f mmpz2wav

render_blender () {
    src="$1"
    shift 1
    echo "render_blender: $src"...
    blender -b "$src" -a "$@" 2>/dev/null >/dev/null
}; export -f render_blender

render_blender_marker () {
    src="$1"
    shift 1
    echo "render_blender_marker: $src"
    blender -b "$src" "$@" --python <(echo "$render_marker") 2>/dev/null >/dev/null
}; export -f render_blender_marker
