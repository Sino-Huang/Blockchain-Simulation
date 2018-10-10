-- Uwe R. Zimmer, Australia, 2018

with Ada.Float_Text_IO;             use Ada.Float_Text_IO;
with Ada.Long_Integer_Text_IO;      use Ada.Long_Integer_Text_IO;
with Ada.Real_Time;                 use Ada.Real_Time;
with Ada.Text_IO;                   use Ada.Text_IO;
with Aura_Configuration_Structures; use Aura_Configuration_Structures;
--  with Authorized_Nodes;              use Authorized_Nodes;
--  with Blocks;                        use Blocks;
with GNAT.Command_Line;             use GNAT.Command_Line;
with Network_Internal;              use Network_Internal;

procedure Aura is

   Options_Ok : Boolean := True;

   procedure Print_Options is

   begin
      New_Line; Put ("accepted options:");
      New_Line; Put ("   [-a {Attack_Rate  : Zero_to_One }]   -> "); Put (Command_Line_Parameters.Attack_Rate, 2, 2, 0);
      New_Line; Put ("   [-s {Step         : Seconds_Count }] -> "); Put (Long_Integer (Command_Line_Parameters.Step), 2);
      New_Line; Put ("   [-c {Connectivity : Zero_to_One }]   -> "); Put (Command_Line_Parameters.Connectivity, 2, 2, 0);
      New_Line; Put ("   [-t {Time_Out     : Duration }]      -> "); Put (Float (Command_Line_Parameters.Time_Out), 2, 2, 0);
      New_Line;
      New_Line;
   end Print_Options;

begin
   Initialize_Option_Scan;
   loop
      declare
         Option : constant Character := Getopt ("a: s: c: t:");
      begin
         case Option is
            when ASCII.NUL => exit;
            when 'a' => Command_Line_Parameters.Attack_Rate  := Zero_to_One'Value   (Parameter);
            when 's' => Command_Line_Parameters.Step         := Seconds_Count'Value (Parameter);
            when 'c' => Command_Line_Parameters.Connectivity := Zero_to_One'Value   (Parameter);
            when 't' => Command_Line_Parameters.Time_Out     := Duration'Value      (Parameter);
            when others => raise Program_Error;
         end case;
      exception
         when others =>
            New_Line; Put ("---> Error in option -"); Put (Option); New_Line;
            Options_Ok := False;
      end;
   end loop;

   Print_Options;

   if Options_Ok then
      Initialise_Network;
      Open_Connections;
      Identify_Replicas;
   end if;

end Aura;
