open Core

(* This should look familiar by now! *)

let _distribute_error image ~row ~col ~error =
  if col + 1 <= Image.height image - 1
  then
    Image.set
      image
      ~x:row
      ~y:(col + 1)
      (let r, g, b = Image.get image ~x:row ~y:(col + 1) in
       r + (error * 7 / 16), g + (error * 7 / 16), b + (error * 7 / 16));
  if col - 1 >= 0 && row + 1 <= Image.width image - 1
  then
    Image.set
      image
      ~x:(row + 1)
      ~y:(col - 1)
      (let r, g, b = Image.get image ~x:(row + 1) ~y:(col - 1) in
       r + (error * 3 / 16), g + (error * 3 / 16), b + (error * 3 / 16));
  if row + 1 <= Image.width image - 1
  then
    Image.set
      image
      ~x:(row + 1)
      ~y:col
      (let r, g, b = Image.get image ~x:(row + 1) ~y:col in
       r + (error * 5 / 16), g + (error * 5 / 16), b + (error * 5 / 16));
  if col + 1 <= Image.height image - 1 && row + 1 <= Image.width image - 1
  then
    Image.set
      image
      ~x:(row + 1)
      ~y:(col + 1)
      (let r, g, b = Image.get image ~x:(row + 1) ~y:(col + 1) in
       r + (error * 1 / 16), g + (error * 1 / 16), b + (error * 1 / 16));
  image
;;

let diff_distribute_error image ~x ~y ~error =
  if x + 1 <= Image.width image - 1
  then
    Image.set
      image
      ~x:(x + 1)
      ~y
      (let r, g, b = Image.get image ~x:(x + 1) ~y in
       r + (error * 7 / 16), g + (error * 7 / 16), b + (error * 7 / 16));
  if x - 1 >= 0 && y + 1 <= Image.height image - 1
  then
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (let r, g, b = Image.get image ~x:(x - 1) ~y:(y + 1) in
       r + (error * 3 / 16), g + (error * 3 / 16), b + (error * 3 / 16));
  if y + 1 <= Image.height image - 1
  then
    Image.set
      image
      ~x
      ~y:(y + 1)
      (let r, g, b = Image.get image ~x ~y:(y + 1) in
       r + (error * 5 / 16), g + (error * 5 / 16), b + (error * 5 / 16));
  if x + 1 <= Image.width image - 1 && y + 1 <= Image.height image - 1
  then
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (let r, g, b = Image.get image ~x:(x + 1) ~y:(y + 1) in
       r + (error * 1 / 16), g + (error * 1 / 16), b + (error * 1 / 16))
;;

let transform image =
  let grayscale_image = Grayscale.transform image in
  let max_value = Image.max_val grayscale_image in
  Image.foldi grayscale_image ~init:grayscale_image ~f:(fun ~x ~y img pix ->
    if Pixel.red pix > max_value / 2
    then (
      Image.set img ~x ~y (Pixel.of_int max_value);
      diff_distribute_error img ~x ~y ~error:(Pixel.red pix - max_value);
      img)
    else (
      Image.set img ~x ~y (Pixel.of_int 0);
      diff_distribute_error img ~x ~y ~error:(Pixel.red pix);
      img))
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
