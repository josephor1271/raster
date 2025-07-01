open Core

(* This should look familiar by now! *)
let normalize_pixel_and_get_error ~max_value ~grayscale_value : Pixel.t * int
  =
  match grayscale_value > max_value / 2 with
  | true -> (max_value, max_value, max_value), grayscale_value - max_value
  | false -> (0, 0, 0), grayscale_value
;;

let update_error ~x ~y ~error ~error_fraction ~height ~width image =
  if x >= 0 && x < width && y >= 0 && y < height
  then (
    let current_grayscale, _, _ = Image.get image ~x ~y in
    let new_grayscale =
      Float.of_int current_grayscale +. (Float.of_int error *. error_fraction)
      |> Float.round
      |> Int.of_float
    in
    Image.set image ~x ~y (new_grayscale, new_grayscale, new_grayscale))
  else ()
;;

let dither_pixel ~max_value ~x ~y ~height ~width image =
  let grayscale_value, _, _ = Image.get image ~x ~y in
  let normalized_pixel, error =
    normalize_pixel_and_get_error ~max_value ~grayscale_value
  in
  update_error
    ~x:(x + 1)
    ~y
    ~error
    ~error_fraction:(7. /. 16.)
    ~height
    ~width
    image;
  update_error
    ~x:(x - 1)
    ~y:(y + 1)
    ~error
    ~error_fraction:(3. /. 16.)
    ~height
    ~width
    image;
  update_error
    ~x
    ~y:(y + 1)
    ~error
    ~error_fraction:(5. /. 16.)
    ~height
    ~width
    image;
  update_error
    ~x:(x + 1)
    ~y:(y + 1)
    ~error
    ~error_fraction:(1. /. 16.)
    ~height
    ~width
    image;
  normalized_pixel
;;

let transform image =
  let height = Image.height image in
  let width = Image.width image in
  let max_value = Image.max_val image in
  let b_w_image = Grayscale.transform image in
  Image.mapi b_w_image ~f:(fun ~x ~y _ ->
    dither_pixel ~max_value ~x ~y ~height ~width b_w_image)
;;

let%expect_test "blue_screen" =
  let expected_image =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_dither.ppm"
  in
  let original_image =
    Image.load_ppm ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm"
  in
  let our_image = transform original_image in
  Image.compare_two_images ~expected_image ~our_image;
  [%expect]
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
