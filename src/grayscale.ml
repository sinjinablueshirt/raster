open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)

let average_pixel r g b =
  let average = (r + g + b) / 3 in
  average, average, average
;;

let transform image =
  Image.map image ~f:(fun (r, g, b) -> average_pixel r g b)
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
          (Image.load_ppm ~filename:"../images/beach_portrait_gray.ppm")
          (Image.load_ppm
             ~filename:"../images/reference-beach_portrait_gray.ppm")));
  [%expect {|0|}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
