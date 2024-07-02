open Core

let maybe_change_number value ~max_pix_value ~threshold =
  if value <= Int.of_float (Float.of_int max_pix_value *. threshold)
  then value
  else max_pix_value - value
;;

let transform_pix pix ~max_pix_value ~threshold =
  let r, g, b = pix in
  ( maybe_change_number r ~max_pix_value ~threshold
  , maybe_change_number g ~max_pix_value ~threshold
  , maybe_change_number b ~max_pix_value ~threshold )
;;

let transform image ~threshold =
  let max_pix_value = Image.max_val image in
  Image.map image ~f:(fun pix -> transform_pix pix ~max_pix_value ~threshold)
;;

let command =
  Command.basic
    ~summary:"solarize an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~threshold:0.4 in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;
