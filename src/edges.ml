Open Core




let transform image ~threshold ~radius=
  let grayscale_image = Grayscale.transform image in
  let blurred_grayscale = Blur.transform grayscale_image
;;



let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let filename =
        "../images/meadow.ppm"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~threshold:0.4 ~radius:2 in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;