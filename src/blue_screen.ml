open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    if b > r + g then Image.get background ~x ~y else r, g, b)
;;

let%expect_test "blue_screen" =
  let expected_image =
    Image.load_ppm
      ~filename:"/home/ubuntu/raster/images/reference-oz_bluescreen_vfx.ppm"
  in
  let original_image =
    Image.load_ppm ~filename:"/home/ubuntu/raster/images/oz_bluescreen.ppm"
  in
  let new_background =
    Image.load_ppm ~filename:"/home/ubuntu/raster/images/meadow.ppm"
  in
  let our_image =
    transform ~foreground:original_image ~background:new_background
  in
  Image.compare_two_images ~expected_image ~our_image;
  [%expect]
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
