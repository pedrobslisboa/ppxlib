(** This module implements pretty printers for the OCaml AST's version used by
    ppxlib.

    Those pretty printers show the AST as its OCaml representation and do not
    pretty print the corresponding source code. For printing ASTs as source code
    use the {!Ppxlib.Pprintast} module instead.

    For example, calling [Pp_ast.expression Format.std_formatter [%expr x + 2]]
    will print:
    {v
   Pexp_apply
     ( Pexp_ident (Lident "+")
     , [ ( Nolabel, Pexp_ident (Lident "x"))
       ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
       ]
     )
    v}

    To keep the output easily readable, records with [_desc] fields such as
    {!Ppxlib.Ast.type-expression} or {!Ppxlib.Ast.type-pattern} are not printed
    as such and only the value of the corresponding [_desc] field is printed
    instead. This prevents AST nodes metadata, such as locations or attributes,
    from polluting the output, keeping it relatively concise and clean. The same
    goes for {!Location.type-loc} values which are printed as the value of their
    [txt] field.

    {!Location.t} and {!Ppxlib.Ast.attributes} are not displayed by default even
    outside of the records mentioned above.

    The {!Config} module below allows to override part or all of this behaviour.
    When configured to display locations or attributes, the entire record will
    be displayed, not only its [_desc] field. *)

open! Import

module Config : sig
  type t
  (** Type for AST pretty-printing config *)

  val make :
    ?show_attrs:bool ->
    ?show_locs:bool ->
    ?loc_mode:[ `Short | `Full ] ->
    unit ->
    t
  (** Create a custom pretty-printing config. Default values are the ones that
      are used when no configuration is passed to the pretty-printers defined in
      {!Pp_ast}.
      @param ?show_attrs
        controls whether attributes are shown or hidden. Defaults to [false].
      @param ?show_loc
        controls whether locations are shown or hidden. Defaults to [false].
      @param ?loc_mode
        controls how locations are shown if they are shown at all. Defaults to
        [`Short].
        - When set to [`Short], locations are displayed as ["l1c6..l2c2"] for
          multiline locations and as ["l1c6..12"] for single line locations.
          Ghost locations are suffixed with a ["(g)"].
        - When set to [`Full], locations are displayed as any other record would
          be. *)
end

type simple_val =
  | Unit
  | Int of int
  | String of string
  | Bool of bool
  | Char of char
  | Array of simple_val list
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Record of (string * simple_val) list
  | Constr of string * simple_val list
  | Tuple of simple_val list
  | List of simple_val list
  | Special of string

type printer = Format.formatter -> simple_val -> unit
type 'a pp = Format.formatter -> 'a -> unit
type 'a configurable = ?config:Config.t -> ?printer:printer -> 'a pp
type 'a configured = 'a pp

module type S = sig
  type 'a ast_printer

  val structure : structure ast_printer
  val structure_item : structure_item ast_printer
  val signature : signature ast_printer
  val signature_item : signature_item ast_printer
  val expression : expression ast_printer
  val pattern : pattern ast_printer
  val core_type : core_type ast_printer
end

module type Conf = sig
  val config : Config.t
end

module type Configured = S with type 'a ast_printer = 'a configured
module type Configurable = S with type 'a ast_printer = 'a configurable

module Make (Conf : Conf) : Configured [@@ocaml.warning "-67"]

val make : Config.t -> (module Configured)

module Default : Configured
include Configurable
