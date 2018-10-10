-- Uwe R. Zimmer, Australia, 2018

with Ada.Streams; use Ada.Streams;

package Base_64 is

   -----------------------------------------------------------------------------
   -- RFC 4648 base 64 encoding of binary streams into 6 bit ASCII characters
   -- Pads with '='.
   -- Does not consider separators (LF, CR).
   -- Assumes that Stream_Element is a byte.
   -----------------------------------------------------------------------------

   function Encode (From : Stream_Element_Array) return String;

   function Decode (From : String) return Stream_Element_Array;

end Base_64;
