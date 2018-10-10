with Ada.Real_Time; use Ada.Real_Time;

package Aura_Configuration_Structures is

   subtype Zero_to_One is Float range 0.0 .. 1.0;

   type Command_Line_Options is record
      Attack_Rate  : Zero_to_One   := 0.0;
      Step         : Seconds_Count := 3;
      Connectivity : Zero_to_One   := 1.0;
      Time_Out     : Duration      := 0.1;
   end record;

   Command_Line_Parameters : Command_Line_Options;

end Aura_Configuration_Structures;
