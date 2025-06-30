open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  let height = Image.height image in
  let width = Image.width image in
  let blank_image = Image.make ~max_val:65535 ~height ~width (0, 0, 0) in
  Image.mapi blank_image ~f:(fun ~x ~y _ ->
    let x_start = max 0 (x - radius) in
    let x_end = min (width - 1) (x + radius) in
    let y_start = max 0 (y - radius) in
    let y_end = min (height - 1) (y + radius) in
    Image.slice image ~x_start ~x_end ~y_start ~y_end |> Image.mean_pixel)
;;

let%expect_test "blur" =
  let expected_image =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_blur.ppm"
  in
  let original_image =
    Image.load_ppm ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm"
  in
  let our_output_image = transform original_image ~radius:3 in
  let x_coords = Image.width our_output_image |> List.init ~f:(fun x -> x) in
  let y_coords =
    Image.height our_output_image |> List.init ~f:(fun y -> y)
  in
  List.iter x_coords ~f:(fun x_coord ->
    List.iter y_coords ~f:(fun y_coord ->
      if
        Pixel.equal
          (Image.get our_output_image ~x:x_coord ~y:y_coord)
          (Image.get expected_image ~x:x_coord ~y:y_coord)
      then ()
      else
        print_string
          ("incorrect at pixel at "
           ^ string_of_int x_coord
           ^ ", "
           ^ string_of_int y_coord
           ^ "\n")));
  [%expect]
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
