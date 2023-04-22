(** Transforms the abstract syntax tree of a MiniRust program back into a MiniRust source file. *)

(** [print_program out prog] transforms the abstract syntax tree [prog] back into Rust and write the result on the [out] channel.
   We can use this to check the result of parsing. We can use this too to indent a program. *)
val print_program : out_channel -> MJ.program -> unit
