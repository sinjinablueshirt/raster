open Core

let is_on_border image ~x ~y =
  x = 0 || x = Image.width image - 1 || 0 = y || y = Image.height image - 1
;;

let calculate_vertical_gradient image ~x ~y =
  let vertical_gradient_value =
    (-1 * Pixel.red (Image.get image ~x:(x - 1) ~y:(y - 1)))
    + (-2 * Pixel.red (Image.get image ~x:(x - 1) ~y))
    + (-1 * Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1)))
    + Pixel.red (Image.get image ~x:(x + 1) ~y:(y - 1))
    + (2 * Pixel.red (Image.get image ~x:(x + 1) ~y))
    + Pixel.red (Image.get image ~x:(x + 1) ~y:(y + 1))
  in
  vertical_gradient_value
;;

(* ADD CHECK TO MAKE SURE PIXELS IN IMAGE *)
let calculate_horizontal_gradient image ~x ~y =
  let horizontal_gradient_value =
    (-1 * Pixel.red (Image.get image ~x:(x - 1) ~y:(y - 1)))
    + (-2 * Pixel.red (Image.get image ~x ~y:(y - 1)))
    + Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1))
    + (-1 * Pixel.red (Image.get image ~x:(x + 1) ~y:(y - 1)))
    + (2 * Pixel.red (Image.get image ~x ~y:(y + 1)))
    + Pixel.red (Image.get image ~x:(x + 1) ~y:(y + 1))
  in
  horizontal_gradient_value
;;

let apply_kernel_to_pixel image ~x ~y =
  let vert_grad = calculate_vertical_gradient image ~x ~y in
  let horizontal_grad = calculate_horizontal_gradient image ~x ~y in
  Float.sqrt
    (Float.of_int
       ((vert_grad * vert_grad) + (horizontal_grad * horizontal_grad)))
;;

let transform image ~threshold ~radius =
  let grayscale_image = Grayscale.transform image in
  let blurred_grayscale = Blur.transform grayscale_image ~radius in
  let max_value = Image.max_val image in
  Image.mapi blurred_grayscale ~f:(fun ~x ~y pix ->
    if is_on_border image ~x ~y
    then pix
    else if Float.( > )
              (apply_kernel_to_pixel image ~x ~y)
              (threshold *. Float.of_int max_value)
    then Pixel.of_int max_value
    else Pixel.of_int 0)
;;

let command =
  Command.basic
    ~summary:"get edges of an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> transform ~threshold:0.4 ~radius:2
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edges.ppm")]
;;
