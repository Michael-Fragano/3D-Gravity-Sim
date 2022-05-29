open Graphics

let init () =
  open_graph " 800x600";
  set_window_title "CS3110 Final: Gravity!";
  auto_synchronize false

let rec draw_coords status =
  clear_graph ();
  moveto 0 0;
  draw_string
    (Printf.sprintf "x : %n, y : %n" status.mouse_x status.mouse_y);
  draw_coords (wait_next_event [ Mouse_motion ])


  let rel_pos (camera : Camera.camera) (body : Gravity.body) = (
    let ccosx = cos (-.Camera.camrotx camera) in
  let csinx = sin (-.Camera.camrotx camera) in
  let ccosy = cos (-.Camera.camroty camera) in
  let csiny = sin(-.Camera.camroty camera) in
  let ccosz = cos (-.Camera.camrotz camera) in
  let csinz = sin (-.Camera.camrotz camera) in

    (**Find position relative to camera's global positioin*)
    let cpos = Gravity.make_p (Gravity.x_pos body -. Camera.camposx camera) (Gravity.y_pos body -. Camera.camposy camera) (Gravity.z_pos body -. Camera.camposz camera) in
  
    (**Find position relative to camera's rotation*)
    let crotx = Gravity.make_p (Gravity.px cpos) (Gravity.py cpos *. ccosx -. Gravity.pz cpos *. csinx) (Gravity.pz cpos *. ccosx +. Gravity.py cpos  *. csinx) in
    let croty = Gravity.make_p (Gravity.px crotx *. ccosy -. Gravity.pz crotx *. csiny) (Gravity.py crotx) (Gravity.pz crotx *. ccosy +. Gravity.px crotx *. csiny) in
    Gravity.make_p (Gravity.px croty *. ccosz -. Gravity.py croty *. csinz) (Gravity.py croty *. ccosz +. Gravity.px croty *. csinz) (Gravity.pz croty)
  )
  



let draw_focus (status : Status.t) =
  moveto 0 0;
  set_color background;
  fill_rect 0 0 (size_x ()) 10;
  set_color black;
  draw_string (Printf.sprintf "  speed : %f" (Status.speed status))

let pdist x1 y1 z1 x2 y2 z2=
  let x = x2 -. x1 in
  let y = y2 -. y1 in
  let z = z2 -. z1 in 
  sqrt ((x *. x) +. (y *. y) +. (z *. z)) 
  
let rec sortbods c sorted bodies :  Gravity.body list =
  let rec sort (sorted : Gravity.body list) b =
    match sorted with
    | [] -> [b]
    | [h] -> if ((pdist (Camera.camposx c) (Camera.camposy c) (Camera.camposz c) (Gravity.x_pos b) (Gravity.y_pos b)  (Gravity.z_pos b)) > (pdist (Camera.camposx c) (Camera.camposy c) (Camera.camposz c) (Gravity.x_pos h) (Gravity.y_pos h) (Gravity.z_pos h))) then [b; h] else [h; b]
    | h :: t -> if ((pdist (Camera.camposx c) (Camera.camposy c) (Camera.camposz c) (Gravity.x_pos b) (Gravity.y_pos b)  (Gravity.z_pos b)) > (pdist (Camera.camposx c) (Camera.camposy c) (Camera.camposz c) (Gravity.x_pos h) (Gravity.y_pos h) (Gravity.z_pos h))) then [b; h] @ t else h :: sort t b
  in
  match bodies with
  | [] -> sorted
  | h :: t -> sortbods c (sort sorted h) t

let rec draw_bodies camera clear bodies = 
  let sbods = sortbods camera [] bodies in
  match sbods with
  | [] -> ()
  | h :: t -> (
      if clear then set_color background
      else set_color (Gravity.color h);
      let maxr = (Camera.camfov camera /. 2.) in
      let rpos = rel_pos camera h in
      if Gravity.py rpos < 0. then  
        draw_bodies camera clear t
      else 
      let y = atan (Gravity.pz rpos /. Gravity.py rpos) in
      let x = atan (Gravity.px rpos/. Gravity.py rpos) in
      if (y > maxr) || (x > maxr) then
        draw_bodies camera clear t
      else
        fill_circle (int_of_float ((400. *.  (x /. maxr)) +. 400.)) (int_of_float ((300. *.  (y /. maxr)) +. 300.)) (int_of_float (( (800. /. Camera.camfov camera) /. (sqrt ((Gravity.px rpos ** 2. ) +.(Gravity.py rpos ** 2. ) +. (Gravity.pz rpos ** 2. ) ))) *. Gravity.rad h ));
          draw_bodies camera clear t)

let clear_screen camera system status =
  draw_bodies camera true (Gravity.bods system)


let render
    (camera : Camera.camera)
    (system : Gravity.system)
    (status : Status.t) : unit =
  draw_bodies camera false (Gravity.bods system);
  draw_focus status;
  synchronize ();
  clear_screen camera system status

let update_status (status : Status.t) (system : Gravity.system) :
    Status.t =
  status |> Status.poll_input
  |> Status.update_body_num system
  |> Status.bind_mouse Pressed Status.toggle_pause
  |> Status.bind_key ',' Pressed (Status.update_speed false)
  |> Status.bind_key '.' Pressed (Status.update_speed true)
  |> Status.bind_key 'q' Pressed (fun _ ->
         raise @@ Graphics.Graphic_failure "Quit Window")

let seconds_per_frame : float = 1. /. 60.

let update_system (system : Gravity.system) (status : Status.t) :
    Gravity.system =
  Gravity.frame system
    (int_of_float
       (seconds_per_frame *. Status.speed status
      /. Gravity.timestep system))
(* <- gives ticks per frame*)

let adjust
    (cam : Camera.camera)
    (status : Status.t) : Camera.camera = 
    if (Status.key_state 'a' status = Pressed) || (Status.key_state 'a' status = Held)  then (Camera.set_all_camera (Camera.camposx cam -. 5.) (Camera.camposy cam) (Camera.camposz cam) (Camera.camrotx cam) (Camera.camroty cam) (Camera.camrotz cam) (Camera.camfov cam))
    else if (Status.key_state 'd' status = Pressed) || (Status.key_state 'd' status = Held)  then Camera.(set_all_camera (camposx cam +. 5.) (camposy cam) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam) (camfov cam))
    else if (Status.key_state 'w' status = Pressed) || (Status.key_state 'w' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam +. 5.) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam) (camfov cam))
    else if (Status.key_state 's' status = Pressed) || (Status.key_state 's' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam -. 5.) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam) (camfov cam))
    else if (Status.key_state ' ' status = Pressed) || (Status.key_state ' ' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam +. 5.) (camrotx cam) (camroty cam) (camrotz cam) (camfov cam))
    else if (Status.key_state 'z' status = Pressed) || (Status.key_state 'z' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam -. 5.) (camrotx cam) (camroty cam) (camrotz cam) (camfov cam))
    else if (Status.key_state 'j' status = Pressed) || (Status.key_state 'j' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam +. 0.03) (camfov cam))
    else if (Status.key_state 'l' status = Pressed) || (Status.key_state 'l' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam -. 0.03) (camfov cam))
    else if (Status.key_state 'i' status = Pressed) || (Status.key_state 'i' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam +. 0.03) (camroty cam) (camrotz cam ) (camfov cam))
    else if (Status.key_state 'k' status = Pressed) || (Status.key_state 'k' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam -. 0.03) (camroty cam) (camrotz cam ) (camfov cam))
    else if (Status.key_state 'u' status = Pressed) || (Status.key_state 'u' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam +. 0.03) (camrotz cam ) (camfov cam))
    else if (Status.key_state 'o' status = Pressed) || (Status.key_state 'o' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam -. 0.03) (camrotz cam ) (camfov cam))
    else if (Status.key_state '1' status = Pressed) || (Status.key_state '1' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam ) (camfov cam -. 0.05))
    else if (Status.key_state '2' status = Pressed) || (Status.key_state '2' status = Held)  then Camera.(set_all_camera (camposx cam) (camposy cam ) (camposz cam) (camrotx cam) (camroty cam) (camrotz cam ) (camfov cam +. 0.05))  
    else cam

let rec main_loop
    (camera : Camera.camera)
    (system : Gravity.system)
    (status : Status.t)
    (time : float) : unit =
  render camera system status;
  let new_system =
    if Status.is_paused status then system
    else update_system system status
  in
  let new_status =
    update_status status new_system
  in
  let new_camera = adjust camera new_status in
  let new_time = Unix.gettimeofday () in
  let time_left = seconds_per_frame -. new_time +. time in
  if time_left > 0. then Unix.sleepf time_left;
  main_loop new_camera new_system new_status (new_time +. time_left)

let start_window () =
  init ();
  draw_coords (wait_next_event [ Mouse_motion ])

let start_window_preset json =
  let system =
    "data/" ^ json ^ ".json"
    |> Yojson.Basic.from_file |> Gravity.from_json
  in
  try
    init ();
    main_loop Camera.default_camera system (Status.default ())
      (Unix.gettimeofday ())
  with Graphics.Graphic_failure _ -> Graphics.close_graph ()

let start_window_from_create system =
  try
    init ();
    main_loop Camera.default_camera system (Status.default ())
      (Unix.gettimeofday ())
  with Graphics.Graphic_failure _ -> Graphics.close_graph ()
