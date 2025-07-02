open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let rec get_value_or_pad ~x ~y ~image ~height ~width =
  match x >= 0 with
  | false -> get_value_or_pad ~x:(x + 2) ~y ~image ~height ~width
  | true ->
    (match x < width with
     | false -> get_value_or_pad ~x:(x - 2) ~y ~image ~height ~width
     | true ->
       (match y >= 0 with
        | false -> get_value_or_pad ~x ~y:(y + 2) ~image ~height ~width
        | true ->
          (match y < height with
           | false -> get_value_or_pad ~x ~y:(y - 2) ~image ~height ~width
           | true -> Image.get image ~x ~y |> Pixel.red)))
;;

let get_list_of_surrounding_values_from_image
      (center_val, _, _)
      ~x
      ~y
      ~image
      ~height
      ~width
  =
  let top_left_val =
    get_value_or_pad ~x:(x - 1) ~y:(y - 1) ~image ~height ~width
  in
  let top_val = get_value_or_pad ~x ~y:(y - 1) ~image ~height ~width in
  let top_right_val =
    get_value_or_pad ~x:(x + 1) ~y:(y - 1) ~image ~height ~width
  in
  let left_val = get_value_or_pad ~x:(x - 1) ~y ~image ~height ~width in
  let right_val = get_value_or_pad ~x:(x + 1) ~y ~image ~height ~width in
  let bottom_left_val =
    get_value_or_pad ~x:(x - 1) ~y:(y + 1) ~image ~height ~width
  in
  let bottom_val = get_value_or_pad ~x ~y:(y + 1) ~image ~height ~width in
  let bottom_right_val =
    get_value_or_pad ~x:(x + 1) ~y:(y + 1) ~image ~height ~width
  in
  [ top_left_val
  ; top_val
  ; top_right_val
  ; left_val
  ; center_val
  ; right_val
  ; bottom_left_val
  ; bottom_val
  ; bottom_right_val
  ]
;;

let horizontal_kernel_list = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ]
let vertical_kernel_list = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ]

let rec pointwise_multiplication_two_lists list1 list2 =
  (* if Int.equal (List.length list1) (List.length list2) |> not
     then ( raise "lists must be of same length") ;
     else *)
  match list1, list2 with
  | [], _ | _, [] -> 0
  | head_int1 :: tail1, head_int2 :: tail2 ->
    (head_int1 * head_int2) + pointwise_multiplication_two_lists tail1 tail2
;;

let get_horizontal_gradient_of_pixel ~pixel ~x ~y ~image ~height ~width =
  pointwise_multiplication_two_lists
    (get_list_of_surrounding_values_from_image
       pixel
       ~x
       ~y
       ~image
       ~height
       ~width)
    horizontal_kernel_list
;;

let get_vertical_gradient_of_pixel ~pixel ~x ~y ~image ~height ~width =
  pointwise_multiplication_two_lists
    (get_list_of_surrounding_values_from_image
       pixel
       ~x
       ~y
       ~image
       ~height
       ~width)
    vertical_kernel_list
;;

let get_gradient_magnitude_of_pixel pixel ~x ~y ~image ~height ~width =
  let vertical_grad_float =
    get_vertical_gradient_of_pixel ~pixel ~x ~y ~image ~height ~width
    |> Float.of_int
  in
  let horizontal_grad_float =
    get_horizontal_gradient_of_pixel ~pixel ~x ~y ~image ~height ~width
    |> Float.of_int
  in
  ((vertical_grad_float ** 2.) +. (horizontal_grad_float ** 2.)) ** 0.5
  |> Float.round
;;

let edge_detect_pixel
      pixel
      ~x
      ~y
      ~image
      ~height
      ~width
      ~(threshold : float)
      ~max_val
  =
  let gradient_magnitude =
    get_gradient_magnitude_of_pixel pixel ~x ~y ~image ~height ~width
  in
  match Float.compare gradient_magnitude threshold < 0 with
  | true -> 0, 0, 0
  | false -> max_val, max_val, max_val
;;

let transform image ~blur_radius ~threshold_fraction =
  let grayscale_image = Grayscale.transform image in
  let blurred_grayscale_image =
    Blur.transform grayscale_image ~radius:blur_radius
  in
  let height = Image.height image in
  let width = Image.width image in
  let max_val = Image.max_val image in
  let threshold = Float.of_int max_val *. threshold_fraction in
  let canvas =
    Image.make ~max_val ~width ~height (max_val, max_val, max_val)
  in
  Image.mapi canvas ~f:(fun ~x ~y pixel ->
    edge_detect_pixel
      pixel
      ~x
      ~y
      ~image:blurred_grayscale_image
      ~height
      ~width
      ~threshold
      ~max_val)
;;

let command =
  Command.basic
    ~summary:"Convert an image to edge-detect image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image_load = Image.load_ppm ~filename in
        let image =
          transform image_load ~blur_radius:2 ~threshold_fraction:0.4
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_edge_detect.ppm")]
;;
