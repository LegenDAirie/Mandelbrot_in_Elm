module Main exposing (..)

import Mandelbrot


main =
    Mandelbrot.init 300
        |> Mandelbrot.computeAll
        |> Mandelbrot.view
