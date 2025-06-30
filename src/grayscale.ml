open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let average = (r + b + g) / 3 in
    average, average, average)
;;

let%expect_test "grayscale transform" =
  let grayscale_image =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_gray.ppm"
  in
  let original_image =
    Image.load_ppm ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm"
  in
  let our_grayscale_image = transform original_image in
  let x_coords =
    Image.width our_grayscale_image |> List.init ~f:(fun x -> x)
  in
  let y_coords =
    Image.height our_grayscale_image |> List.init ~f:(fun y -> y)
  in
  List.iter x_coords ~f:(fun x_coord ->
    List.iter y_coords ~f:(fun y_coord ->
      if
        Pixel.equal
          (Image.get grayscale_image ~x:x_coord ~y:y_coord)
          (Image.get our_grayscale_image ~x:x_coord ~y:y_coord)
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
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image_load = Image.load_ppm ~filename in
        let image = transform image_load in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
