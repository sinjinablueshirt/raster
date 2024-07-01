open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x:row ~y:col (r, g, b) ->
    if b > r + g then Image.get background ~x:row ~y:col else r, g, b)
;;

let get_wrong_count returned_image expected_image =
  Image.foldi returned_image ~init:0 ~f:(fun ~x:row ~y:col count pix ->
    if Pixel.equal pix (Image.get expected_image ~x:row ~y:col)
    then count
    else count + 1)
;;

let%expect_test "transform" =
  print_endline
    (Int.to_string
       (get_wrong_count
          (Image.load_ppm ~filename:"../images/oz_bluescreen_vfx.ppm")
          (Image.load_ppm
             ~filename:"../images/reference-oz_bluescreen_vfx.ppm")));
  [%expect {|0|}]
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
