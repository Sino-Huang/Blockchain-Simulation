-- Uwe R. Zimmer, Australia, 2018

package body Base_64 is

   subtype Six_Bits is Ada.Streams.Stream_Element range 0 .. 63;

   From_Code_64 : constant array (Character) of Six_Bits
     := ('A' =>  0, 'B' =>  1, 'C' =>  2, 'D' =>  3, 'E' =>  4, 'F' =>  5,
         'G' =>  6, 'H' =>  7, 'I' =>  8, 'J' =>  9, 'K' => 10, 'L' => 11,
         'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16, 'R' => 17,
         'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23,
         'Y' => 24, 'Z' => 25, 'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29,
         'e' => 30, 'f' => 31, 'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35,
         'k' => 36, 'l' => 37, 'm' => 38, 'n' => 39, 'o' => 40, 'p' => 41,
         'q' => 42, 'r' => 43, 's' => 44, 't' => 45, 'u' => 46, 'v' => 47,
         'w' => 48, 'x' => 49, 'y' => 50, 'z' => 51, '0' => 52, '1' => 53,
         '2' => 54, '3' => 55, '4' => 56, '5' => 57, '6' => 58, '7' => 59,
         '8' => 60, '9' => 61, '+' => 62, '/' => 63,
         others => Six_Bits'Invalid_Value);

   To_Code_64 : constant array (Six_Bits) of Character
     := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   Padding : constant Character := '=';

   function Encode (From : Stream_Element_Array) return String is

      type Byte_Position is mod 3;

      To : String (1 .. 4 * (1 + (From'Length - 1) / 3));

      Position   : Byte_Position := Byte_Position'First;
      To_Ix      : Positive      := To'First;
      Complete,
      Incomplete : Six_Bits;

   begin
      for b of From loop
         case Position is
            when 0 =>
               Incomplete := (b mod 4) * 16;
               To (To_Ix     .. To_Ix + 3) := (To_Code_64 (b / 4), To_Code_64 (Incomplete), Padding, Padding);
            when 1 =>
               Complete   := Incomplete + b / 16;
               Incomplete := (b mod 16) * 4;
               To (To_Ix + 1 .. To_Ix + 2) := (To_Code_64 (Complete), To_Code_64 (Incomplete));
            when 2 =>
               Complete   := Incomplete + b / 64;
               To (To_Ix + 2 .. To_Ix + 3) := (To_Code_64 (Complete), To_Code_64 (b mod 64));
               To_Ix := To_Ix + 4;
         end case;
         Position := Byte_Position'Succ (Position);
      end loop;

      return To;
   end Encode;

   function Decode (From : String) return Stream_Element_Array is

      type Char_Position is mod 4;

      To : Stream_Element_Array (1 .. 3 * (From'Length / 4) -
                                 (if  From (From'Last - 1) = Padding then 2
                                    elsif From (From'Last) = Padding then 1
                                    else 0));

      Position : Char_Position         := Char_Position'First;
      To_Ix    : Stream_Element_Offset := To'First;

   begin
      for a of From loop
         if a /= Padding then
            declare
               sb : constant Six_Bits := From_Code_64 (a);
            begin
               case Position is
                  when 0 =>
                     To (To_Ix)     := 4 * sb;
                  when 1 =>
                     To (To_Ix)     := To (To_Ix) + sb / 16;
                     exit when To_Ix = To'Last;
                     To (To_Ix + 1) := (sb mod 16) * 16;
                  when 2 =>
                     To (To_Ix + 1) := To (To_Ix + 1) + sb /  4;
                     exit when To_Ix + 1 = To'Last;
                     To (To_Ix + 2) :=  (sb mod  4) * 64;
                  when 3 =>
                     To (To_Ix + 2) := (To (To_Ix + 2) + sb);
                     To_Ix := To_Ix + 3;
               end case;
               Position := Char_Position'Succ (Position);
            end;
         end if;
      end loop;

      return To;
   end Decode;

end Base_64;
