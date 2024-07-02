open Core

let mse_compare image1 image2 ~height ~width =
  let mse =
    Image.foldi image1 ~init:0 ~f:(fun ~x ~y mse_count (r1, g1, b1) ->
      let r2, g2, b2 = Image.get image2 ~x ~y in
      mse_count
      + ((r1 - r2) * (r1 - r2))
      + ((g1 - g2) * (g1 - g2))
      + ((b1 - b2) * (b1 - b2)))
    / (height * width)
  in
  if mse = 0 then Int.max_value else mse
;;

let get_targets image ~height ~width =
  let num_in_row = Image.width image / width in
  let num_in_col = Image.height image / height in
  List.init (num_in_col * num_in_row) ~f:(fun target_num ->
    ( Image.slice
        image
        ~x_start:(target_num % num_in_row * width)
        ~x_end:((target_num % num_in_row * width) + width - 1)
        ~y_start:(target_num / num_in_row * height)
        ~y_end:((target_num / num_in_row * height) + height - 1)
    , ( target_num % num_in_row * width
      , (target_num % num_in_row * width) + width - 1 ) ))
;;

let switch_regions ~original_image ~region1_image ~region2_image ~region1_start ~region2_start =
  


let get_smallest_mse mse_list =
  List.fold mse_list ~init:(0,0) ~f:(fun (idx_smallest_so_far, mse_smallest_so_far) (idx, mse_val) -> if mse_val<mse_smallest_so_far then idx,mse_val else (idx_smallest_so_far, mse_smallest_so_far));;

let rec transform image ~moves ~height ~width =
  match moves = 0 with
  | true -> image
  | false ->
    let target_list = get_targets image ~height ~width in
    let region1_image, region1_firstpix = List.random_element_exn target_list in
    let mse_of_targets_list = List.mapi target_list ~f:(fun index (rg2, _) -> (index, mse_compare region1_image rg2 ~height ~width)) in (* list of tuple pairs representing the index of a region in the target list and its corresponding mse value *)
    let index_of_smallest_mse, _ = get_smallest_mse mse_of_targets_list in
    let region2_image, region2_firstpix=List.nth_exn target_list index_of_smallest_mse in
    (* CALL A FUNCTION THAT SETS THE PIXELS FROM REGION1 TO REGION2 AND VICE VERSA *)
    (switch_regions ~original_image:image ~region1_image ~region2_image ~region1_start:region1_firstpix ~region2_start:region2_firstpix);
    image;

;;

let command =
  Command.basic
    ~summary:"mosaic an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename
          |> transform ~moves:10_000 ~height:10 ~width:10
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
