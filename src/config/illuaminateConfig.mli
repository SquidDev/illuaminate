(** {!IlluaminateConfig} provides a very basic way of declaring configuration options using
    combinators.

    Configuration files are built from a collection of key-value pairs, or fields (see
    {!Term.field}), which may be placed into sub-categories known as groups ({!Term.group}). Both
    fields and groups are named, and have an associated comment, which describes its purpose.

    Each field is built from a primitive type, which is read and written using a {!Converter}. More
    complex types should be built up using multiple fields and the various {!Term} combinators.

    Note that the storage representation is left entirely internal. While obviously there is a
    decided way of storing config files, it's not something which one should really be concerned
    about. *)

module Term = Term
module Parser = Parser

include module type of struct
  include Category
end
