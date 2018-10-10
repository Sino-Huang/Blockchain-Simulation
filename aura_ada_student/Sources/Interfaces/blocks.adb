-- Uwe R. Zimmer, Australia, 2018

with Base_64; use Base_64;

package body Blocks is

   function Sign (b : Block; a : Authorities) return Block is
     (Signature     => b.Header & ':' & Authorities'Image (a),
      Header        => b.Header,
      Parent_Header => b.Parent_Header,
      Height        => b.Height,
      Time_Stamp    => b.Time_Stamp);

   function Authority_From_Signature (b : Block) return Authorities is
      (Authorities'Value (To_String (Tail (b.Signature, Length (b.Signature) - Index (b.Signature, ":")))));

   function Genesis_Block return Block is

      Genesis_Byte_Stream    : constant Stream_Element_Array (1 .. 32) := (others => 0);
      Genesis_Base_64_String : constant Bounded_String                 := To_Bounded_String (Encode (Genesis_Byte_Stream));

   begin
      return (Signature     => Genesis_Base_64_String,
              Header        => Genesis_Base_64_String,
              Parent_Header => Genesis_Base_64_String,
              Height        => 0,
              Time_Stamp    => Seconds_Count'First);
   end Genesis_Block;

   function Random_Block_Header return Bounded_String is

      Byte_Generator : Generator;

   begin
      Reset (Byte_Generator);
      declare
         Random_Byte_Stream    : constant Stream_Element_Array (1 .. 32) := (others => Random (Byte_Generator));
         Random_Base_64_String : constant Bounded_String                 := To_Bounded_String (Encode (Random_Byte_Stream));
      begin
         return Random_Base_64_String;
      end;
   end Random_Block_Header;

   function Current_Block_Time_Stamp return Seconds_Count is

      Clock_Seconds       : Seconds_Count := Seconds_Count'Invalid_Value;
      Remaining_Time_Span : Time_Span     := Time_Span_First;

   begin
      Split (Clock, Clock_Seconds, Remaining_Time_Span);
      return Clock_Seconds;
   end Current_Block_Time_Stamp;

   function Longest_Blockchain (bcs : Blockchains) return Blockchain is

      b  : Block := bcs.End_of_Longest_Blockchain;
      bc : Blockchain;

   begin
      for i in 0 .. b.Height loop
         bc.Prepend (b);
         begin
            b  := Element (Container => bcs.Blocks,
                           Key       => b.Parent_Header);
         exception
            when Constraint_Error => exit;
         end;
      end loop;
      return bc;
   end Longest_Blockchain;

end Blocks;
