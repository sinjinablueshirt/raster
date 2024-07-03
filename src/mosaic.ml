open Core

let mse_compare image1 image2 =
  let mse =
    Image.foldi image1 ~init:0 ~f:(fun ~x ~y mse_count (r1, g1, b1) ->
      let r2, g2, b2 = Image.get image2 ~x ~y in
      mse_count
      + ((r1 - r2) * (r1 - r2))
      + ((g1 - g2) * (g1 - g2))
      + ((b1 - b2) * (b1 - b2)))
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
        ~x_end:((target_num % num_in_row * width) + width)
        ~y_start:(target_num / num_in_row * height)
        ~y_end:((target_num / num_in_row * height) + height)
    , (target_num % num_in_row * width, target_num / num_in_row * height) ))
;;

let switch_regions
  ~original_image
  ~region1_image
  ~region2_image
  ~region1_start
  ~region2_start
  =
  let _tmp =
    Image.mapi region1_image ~f:(fun ~x ~y region1_pix ->
      let region2_pix = Image.get region2_image ~x ~y in
      let x_region1, y_region1 = region1_start in
      let updated_region1_x = x_region1 + x in
      let updated_region1_y = y_region1 + y in
      let x_region2, y_region2 = region2_start in
      let updated_region2_x = x_region2 + x in
      let updated_region2_y = y_region2 + y in
      Image.set
        original_image
        ~x:updated_region1_x
        ~y:updated_region1_y
        region2_pix;
      Image.set
        original_image
        ~x:updated_region2_x
        ~y:updated_region2_y
        region1_pix;
      region1_pix)
  in
  ()
;;

let get_smallest_mse mse_list =
  List.fold
    mse_list
    ~init:(0, Int.max_value)
    ~f:(fun (idx_smallest_so_far, mse_smallest_so_far) (idx, mse_val) ->
      if mse_val < mse_smallest_so_far
      then idx, mse_val
      else idx_smallest_so_far, mse_smallest_so_far)
;;

let rec transform image ~moves ~height ~width =
  match moves = 0 with
  | true -> image
  | false ->
    let target_list = get_targets image ~height ~width in
    let region1_image, region1_start = List.random_element_exn target_list in
    let mse_of_targets_list =
      List.mapi target_list ~f:(fun index (rg2, _) ->
        index, mse_compare region1_image rg2)
    in
    (* list of tuple pairs representing the index of a region in the target
       list and its corresponding mse value *)
    let index_of_smallest_mse, _ = get_smallest_mse mse_of_targets_list in
    let region2_image, region2_start =
      List.nth_exn target_list index_of_smallest_mse
    in
    switch_regions
      ~original_image:image
      ~region1_image
      ~region2_image
      ~region1_start
      ~region2_start;
    transform image ~moves:(moves - 1) ~height ~width
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
          |> transform ~moves:10000 ~height:10 ~width:10
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
