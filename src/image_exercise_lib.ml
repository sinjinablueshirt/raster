open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "edges", Edges.command
    ; "solarize", Solarize.command
    ; "color_dither", Color_dither.command
    ; "mosaic", Mosaic.command
    ]
;;
