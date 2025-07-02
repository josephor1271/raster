open Core

let solarize_color_value ~color_value ~threshold_val ~max_val =
  match color_value > threshold_val with
  | true -> max_val - color_value
  | false -> color_value
;;

let solarize_pixel (red, green, blue) ~threshold_val ~max_val =
  ( solarize_color_value ~color_value:red ~threshold_val ~max_val
  , solarize_color_value ~color_value:green ~threshold_val ~max_val
  , solarize_color_value ~color_value:blue ~threshold_val ~max_val )
;;

let transform image =
  let max_val = Image.max_val image in
  let threshold_val =
    Float.of_int max_val *. 0.4 |> Float.round |> int_of_float
  in
  Image.map image ~f:(solarize_pixel ~threshold_val ~max_val)
;;

let command =
  Command.basic
    ~summary:"Convert an image to solarized"
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
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;
