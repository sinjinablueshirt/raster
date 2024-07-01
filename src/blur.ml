open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let copied_image = Image.copy image in
  Image.mapi copied_image ~f:(fun ~x:row ~y:col _ ->
    Image.mean_pixel
      (Image.slice
         image
         ~x_start:(if row - radius < 0 then 0 else row - radius)
         ~x_end:
           (if row + radius > Image.width image - 1
            then Image.width image - 1
            else row + radius)
         ~y_start:(if col - radius < 0 then 0 else col - radius)
         ~y_end:
           (if col + radius > Image.height image - 1
            then Image.height image - 1
            else col + radius)))
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
          (Image.load_ppm ~filename:"../images/beach_portrait_blur.ppm")
          (Image.load_ppm
             ~filename:"../images/reference-beach_portrait_blur.ppm")));
  [%expect {|0|}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
