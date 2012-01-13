exception Init_error
exception Print_error

module rec CursesOutput :
        sig
                val init : unit -> unit
                val kill : unit -> unit
                val clear : unit -> unit
                val print_char : int -> int -> Snake.segment -> unit
                val refresh : unit -> unit
        end =
        struct
                (*
                 * Helper function for initialising curses
                 * f: Initialisation function
                 *)
                let init_func f =
                        if f = false then
                                raise Init_error
                        else
                                ()

                let init () =
                        let _ = Curses.initscr () in

                        init_func (Curses.cbreak ());
                        init_func (Curses.keypad (Curses.stdscr ()) true);
                        init_func (Curses.nodelay (Curses.stdscr()) false);
                        init_func (Curses.curs_set 0);
                        init_func (Curses.noecho ());

                        Curses.timeout (-1)
                        
                let kill () =
                        Curses.endwin ()

                let clear () =
                        Curses.clear ()

                let print_char x y char' =
                        let print char =
                                if Curses.mvaddch y x char = false then
                                        raise Print_error
                                else
                                        ()
                        in
                        match char' with
                        | Snake.Segment(position) -> print (int_of_char '#')

                let refresh () =
                        ignore (Curses.refresh ())
        end
and Snake :
        sig
                type position = {x: int; y: int}
                type segment =
                        | Segment of position
                type snake = segment list

                val init : int -> int -> snake
                val print : snake -> unit
                val move_left : snake -> snake
                val move_down : snake -> snake
                val move_up : snake -> snake
                val move_right : snake -> snake
        end =
        struct
                module Display = CursesOutput

                type position = {x: int; y: int}
                type segment =
                        | Segment of position
                type snake = segment list

                let init maxx maxy = [Segment({x = maxx / 2; y = maxy / 2})]

                let print snake' =
                        List.iter (fun (Segment(segment) as s) ->
                                Display.print_char segment.x segment.y s
                        ) snake'

                let move x' y' snake =
                        List.map (fun (Segment(segment)) ->
                                Segment({x = segment.x + x'; y = segment.y + y'})
                        ) snake

                let move_left snake = move (0 - 1) 0 snake

                let move_down snake = move 0 (0 + 1) snake

                let move_up snake = move 0 (0 - 1) snake

                let move_right snake = move (0 + 1) 0 snake
        end
;;
module Display = CursesOutput;;

module CursesInput =
        struct
                type key =
                        | Key of int

                type direction =
                        | Up
                        | Right
                        | Down
                        | Left

                type event =
                        | Direction of direction
                        | Quit
                        | Continue

                type key_binding = {
                        key: key;
                        event: event;
                }

                let key_bindings = Hashtbl.create 10;;

                let bind_key key' event' =
                        Hashtbl.add key_bindings key' ({key = Key(key'); event = event'})

                let get_key () =
                        let k = Curses.getch () in

                        try
                                (Hashtbl.find key_bindings k).event
                        with
                        | Not_found -> Continue
        end
;;
module KeyboardInput = CursesInput;;

open KeyboardInput
module Controller =
        struct
                let init () =
                        Display.init ()

                let kill () =
                        Display.kill ()

                let run state' =
                        let rec get_input state =
                                match KeyboardInput.get_key () with
                                | Quit -> ()
                                | Direction(Up) -> next_state (Snake.move_up state)
                                | Direction(Right) -> next_state (Snake.move_right state)
                                | Direction(Down) -> next_state (Snake.move_down state)
                                | Direction(Left) -> next_state (Snake.move_left state)
                                | Continue -> get_input state
                        and next_state f =
                                display f
                        and display state =
                                Display.clear ();
                                Snake.print state;
                                Display.refresh ();
                                get_input state
                        in
                        display state'
        end
;;

let () =
        try
                Controller.init ();

                (* XXX *)
                let y, x = Curses.getmaxyx (Curses.stdscr ()) in

                CursesInput.bind_key (Char.code 'q') Quit;
                CursesInput.bind_key Curses.Key.left (Direction(Left));
                CursesInput.bind_key Curses.Key.down (Direction(Down));
                CursesInput.bind_key Curses.Key.right (Direction(Right));
                CursesInput.bind_key Curses.Key.up (Direction(Up));
                (* --- *)

                Controller.run (Snake.init x y);

                Controller.kill ();
                Printf.printf "Quit\n";
        with
        | Print_error -> Controller.kill (); Printf.printf "Print_error\n"
        | Init_error -> Printf.printf "Init_error\n"
        | e -> Controller.kill (); Printf.eprintf "Unexpected exception : %s\n" (Printexc.to_string e)
