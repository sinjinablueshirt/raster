open! Core
open! Expect_test_helpers_core
open! Image_exercise_lib

(* let get_wrong_count returned_image expected_image = Image.foldi
   returned_image ~init:0 ~f:(fun ~x:row ~y:col count pix -> if Pixel.equal
   pix (Image.get expected_image ~x:row ~y:col) then count else count + 1) ;;

   let%expect_test "transform" = print_endline (get_wrong_count
   (Image.load_ppm ~filename:"../images/beach_portrait_gray.ppm")
   (Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"))
   [%expect {|0|}] ;; *)
