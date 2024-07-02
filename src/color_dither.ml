open Core

(* let in_bounds image ~x ~y =
  x >= 0
  && y >= 0
  && x <= Image.width image - 1
  && y <= Image.height image - 1
;;

let distribute_error image ~x ~y ~color ~error =
  match color with
  | "red" ->
    if in_bounds image ~x:(x + 1) ~y
    then
      Image.set
        image
        ~x:(x + 1)
        ~y
        (let r, g, b = Image.get image ~x:(x + 1) ~y in
         r + (error * 7 / 16), g, b);
    if in_bounds image ~x:(x - 1) ~y:(y + 1)
    then
      Image.set
        image
        ~x:(x - 1)
        ~y:(y + 1)
        (let r, g, b = Image.get image ~x:(x - 1) ~y:(y + 1) in
         r + (error * 3 / 16), g, b);
    if in_bounds image ~x ~y:(y + 1)
    then
      Image.set
        image
        ~x
        ~y:(y + 1)
        (let r, g, b = Image.get image ~x ~y:(y + 1) in
         r + (error * 3 / 16), g, b);
    if in_bounds image ~x:(x - 1) ~y:(y + 1)
    then
      Image.set
        image
        ~x:(x - 1)
        ~y:(y + 1)
        (let r, g, b = Image.get image ~x:(x - 1) ~y:(y + 1) in
         r + (error * 3 / 16), g, b)
  | "green" -> ()
  | "blue" -> ()
  | _ -> ()
;; *)

let transform image = image
  (* let max_value = Image.max_val image in
  Image.foldi image ~init:image ~f:(fun ~x ~y img (r, g, b) ->
    Image.set img ~x ~y (new_r, new_g, new_b);
    distribute_error img ~x ~y ~color:"red" ~error:(r - new_r);
    distribute_error img ~x ~y ~color:"green" ~error:(g - new_g);
    distribute_error img ~x ~y ~color:"blue" ~error:(b - new_b);
    img) *)
;;

let command =
  Command.basic
    ~summary:"color dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_color_dither.ppm")]
;;
