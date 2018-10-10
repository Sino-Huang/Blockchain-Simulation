-- Uwe R. Zimmer, Australia, 2018

with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;           use Ada.Numerics;
with Ada.Real_Time;                          use Ada.Real_Time;
with Ada.Streams;                            use Ada.Streams;
with Ada.Strings.Bounded;                    use Ada.Strings.Bounded;
with Authorized_Nodes;                       use Authorized_Nodes;

package Blocks is

   pragma Elaborate_Body;

   package Random_Byte is new Discrete_Random (Result_Subtype => Stream_Element);
   use Random_Byte;

   package Block_Strings is new Generic_Bounded_Length (Max => 80);
   use Block_Strings;
   subtype Block_String is Bounded_String;

   type Block is record
      Signature,
      Header,
      Parent_Header : Block_String;
      Height        : Natural;
      Time_Stamp    : Seconds_Count;
   end record;

   function Sign (b : Block; a : Authorities) return Block;
   function Authority_From_Signature (b : Block) return Authorities;

   function Genesis_Block       return Block;
   function Random_Block_Header return Block_String;

   function Current_Block_Time_Stamp return Seconds_Count;

   package Blocks_Containers is new Indefinite_Ordered_Maps (Key_Type     => Block_String,
                                                             Element_Type => Block);
   use Blocks_Containers;

   package Block_Vectors is new Vectors (Index_Type   => Natural,
                                         Element_Type => Block);
   use Block_Vectors;
   subtype Blockchain is Vector;

   type Blockchains is record
      Blocks                    : Map   := Empty_Map;
      End_of_Longest_Blockchain : Block := Genesis_Block;
   end record;

   function Longest_Blockchain (bcs : Blockchains) return Blockchain;

   type Block_Query is record
      Start_Height,
      End_Height    : Natural;
   end record;
   -- (Start_Height => Natural'First, End_Height => Natural'Last) requests the full blockchain.

end Blocks;
