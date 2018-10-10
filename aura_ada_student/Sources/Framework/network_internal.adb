-- Uwe R. Zimmer, Australia, 2018

with Ada.Containers;                use Ada.Containers;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;  use Ada.Numerics;
with Ada.Numerics.Float_Random;     use Ada.Numerics.Float_Random;
with Ada.Real_Time;                 use Ada.Real_Time;
with Ada.Task_Identification;       use Ada.Task_Identification;
with Ada.Text_IO;                   use Ada.Text_IO;
with Aura_Configuration_Structures; use Aura_Configuration_Structures;
with Replicas;                      use Replicas;
with Task_Id_Mapping;

package body Network_Internal is

   use Block_Strings;
   use Block_Vectors;

   Rep : array (Authorities) of Replica;

   package Task_Authorities_Mapping is new Task_Id_Mapping (Task_Index => Authorities);
   use Task_Authorities_Mapping;

   Authorities_First_Pos : constant Natural := Authorities'Pos (Authorities'First);
   Authorities_Last_Pos  : constant Natural := Authorities'Pos (Authorities'Last);
   Authorities_Number    : constant Natural := Authorities_Last_Pos - Authorities_First_Pos + 1;

   subtype Connection_Code is Natural range 0 .. (Authorities_Last_Pos - Authorities_First_Pos + 1) ** 2 - 1;

   function Encode_Connections (From, To : Authorities) return Connection_Code is
     (Authorities'Pos (From) - Authorities_First_Pos
      + Authorities_Number *
        (Authorities'Pos (To) - Authorities_First_Pos));

   function From_Connection (c : Connection_Code) return Authorities is
     (Authorities'Val (c mod Authorities_Number));

   function To_Connection (c : Connection_Code) return Authorities is
     (Authorities'Val (c / Authorities_Number));

   type Connection_Matrix is array (Authorities, Authorities) of Boolean;

   type Attack_Types is (Changed_Signature, Changed_Parent_Header, Future_Timestamp, Genesis_Block);

   package Random_Attacks is new Discrete_Random (Result_Subtype => Attack_Types);
   use Random_Attacks;

   package Random_Authorities is new Discrete_Random (Result_Subtype => Authorities);
   use Random_Authorities;

   Zero_to_One_Generator : Float_Random.Generator;
   Attack_Generator      : Random_Attacks.Generator;
   Authorities_Generator : Random_Authorities.Generator;

   task type Buffer is
      entry Identify    (a : Authorities);
      entry Transmitter (b : Block);
      entry Query       (q : Block_Query; r : out Blockchain);
      entry Attack      (b : Block);
   end Buffer;

   task body Buffer is

      type Message_Type is (Sending, Querying);

      Operation : Message_Type := Message_Type'Invalid_Value;

      To      : Authorities;
      Message : Block;

   begin
      accept Identify (a : Authorities) do
         To := a;
      end Identify;

      loop
         select
            accept Transmitter (b : Block) do
               Message := b;
               Operation := Sending;
            end Transmitter;
         or
            accept Attack (b : Block) do
               Put_Line ("ATTACK!!");
               Message := b;
               case Random (Attack_Generator) is
                  when Changed_Signature     => Message               := Sign (Message, Random (Authorities_Generator));
                  when Changed_Parent_Header => Message.Parent_Header := To_Bounded_String ("Bad header");
                  when Future_Timestamp      => Message.Time_Stamp    := Message.Time_Stamp + Seconds_Count (Authorities_Number);
                  when Genesis_Block         => Message               := Genesis_Block;
               end case;

               Message.Header := To_Bounded_String ("HACKED!");
               Operation := Sending;
            end Attack;
         or
            accept Query (q : Block_Query; r : out Blockchain) do
               select
                  Rep (To).Block_Query_Receiver (q, r);
               or delay Command_Line_Parameters.Time_Out;
                  Put_Line ("Replica " & Authorities'Image (To) & " unresponsive on Block_Query_Receiver!");
               end select;

               Operation := Querying;
            end Query;
         or
            terminate;
         end select;

         case Operation is
            when Sending =>
               select
                  Rep (To).Block_Receiver (Message);
               or delay Command_Line_Parameters.Time_Out;
                  Put_Line ("Replica " & Authorities'Image (To) & " unresponsive on Block_Receiver!");
               end select;
            when Querying =>
               null;
         end case;
      end loop;
   exception
      when E : others => Put_Line (Image (Current_Task) & ": " & Exception_Information (E));
   end Buffer;

   Buffers : array (Authorities) of Buffer;

   protected Internal_Connections is
      entry Send  (Connection_Code) (b : Block);
      entry Query (Connection_Code) (q : Block_Query; r : out Blockchain);
      procedure Open_Conns;
   private
      Connections : Connection_Matrix := (others => (others => False));
   end Internal_Connections;

   protected body Internal_Connections is

      entry Send (for c in Connection_Code) (b : Block) when Connections (From_Connection (c), To_Connection (c)) is

      begin
         case Random (Zero_to_One_Generator) < Command_Line_Parameters.Attack_Rate is
            when True  => requeue Buffers (To_Connection (c)).Attack;
            when False => requeue Buffers (To_Connection (c)).Transmitter;
         end case;
      end Send;

      entry Query (for c in Connection_Code) (q : Block_Query; r : out Blockchain) when Connections (From_Connection (c), To_Connection (c)) is

      begin
         requeue Buffers (To_Connection (c)).Query;
      end Query;

      procedure Open_Conns is

      begin
         for i in Connections'Range (1) loop
            for j in Connections'Range (2) loop
               Connections (i, j) := Random (Zero_to_One_Generator) <= Command_Line_Parameters.Connectivity;
            end loop;
         end loop;
      end Open_Conns;

   end Internal_Connections;

   task type Trigger is
      entry Identify    (a : Authorities);
      entry Transmitter (s : Seconds_Count);
   end Trigger;

   task body Trigger is

      To      : Authorities;
      Message : Seconds_Count;

   begin
      accept Identify (a : Authorities) do
         To := a;
      end Identify;

      loop
         select
            accept Transmitter (s : Seconds_Count) do
               Message := s;
            end Transmitter;
         or
            terminate;
         end select;
         select
            Rep (To).Block_Generator (Message);
         or delay Command_Line_Parameters.Time_Out;
            Put_Line ("Replica " & Authorities'Image (To) & " unresponsive on Block_Generator!");
         end select;
      end loop;
   end Trigger;

   Triggers : array (Authorities) of Trigger;

   task Generator_Trigger;
   task body Generator_Trigger is

      Start_Time : constant Time     := Clock;
      Time_Slot  :          Positive := Positive'First;

   begin
      loop
         delay until Start_Time + Seconds (Time_Slot * Integer (Command_Line_Parameters.Step));
         declare
            a : constant Authorities :=
              Authorities'Val (Long_Integer (Current_Block_Time_Stamp / Command_Line_Parameters.Step) mod Long_Integer (Authorities_Number));
         begin
            Triggers (a).Transmitter (Current_Block_Time_Stamp);
            Time_Slot := Positive'Succ (Time_Slot);
         end;
      end loop;
   exception
      when E : others => Put_Line (Image (Current_Task) & ": " & Exception_Information (E));
   end Generator_Trigger;

   task Monitor;
   task body Monitor is

      Start_Time : constant Time      := Clock + Seconds (Integer (Command_Line_Parameters.Step)) / 2;
      Interval   : constant Time_Span := Seconds (Integer (Command_Line_Parameters.Step));
      Slot       :          Natural   := 0;

      Chains : array (Authorities) of Blockchain;

   begin
      loop
         delay until Start_Time + Slot * Interval;
         Slot := Natural'Succ (Slot);

         for a in Authorities loop
            select
               Rep (a).Block_Query_Receiver ((Natural'First, Natural'Last), Chains (a));
            or delay Command_Line_Parameters.Time_Out;
               Put_Line ("Replica " & Authorities'Image (a) & " unresponsive on Block_Query_Receiver!");
               Chains (a) := Empty_Vector;
            end select;
         end loop;

         declare
            Common_Height : Natural := Natural'First;
         begin
            Find_Common_Height : loop
               begin
                  declare
                     b : constant Block := Chains (Authorities'First).Element (Common_Height);
                  begin
                     for a in Authorities loop
                        if b /= Chains (a).Element (Common_Height) then
                           exit Find_Common_Height;
                        end if;
                     end loop;
                  end;
               exception
                  when Constraint_Error => exit Find_Common_Height;
               end;
               Common_Height := Natural'Succ (Common_Height);
            end loop Find_Common_Height;
            if Common_Height > Natural'First then
               Common_Height := Natural'Pred (Common_Height);
            end if;

            declare
               Longest_Height : Natural := Natural'First;
            begin
               Put_Line ("--------------------------------------------------------------------------");
               Put_Line ("Tails of last signature in each blockchain (authority name at the end):");
               New_Line;
               for a in Authorities loop

                  if Chains (a) = Empty_Vector then
                     Put_Line ("No response from " & Authorities'Image (a));
                  else
                     if Chains (a).Element (0) /= Genesis_Block then
                        Put ("-- Corrupted! -- ");
                     else
                        while Chains (a).Length >= 2 loop
                           if Chains (a).Element (0).Header = Chains (a).Element (1).Parent_Header
                             and then Chains (a).Element (0).Height + 1 = Chains (a).Element (1).Height
                             and then Chains (a).Element (0).Time_Stamp <= Current_Block_Time_Stamp
                             and then Chains (a).Element (0).Time_Stamp <= Chains (a).Element (1).Time_Stamp
                           then
                              Chains (a).Delete_First;
                           else
                              Put ("-- Corrupted! -- ");
                              exit;
                           end if;
                        end loop;
                     end if;

                     Longest_Height := Natural'Max (Longest_Height, Chains (a).Last_Element.Height);

                     Put ("Height:");
                     Put (Chains (a).Last_Element.Height, 4);
                     Put_Line (" Time:"
                               & Long_Integer'Image (Long_Integer (Chains (a).Last_Element.Time_Stamp))
                               & " Sign: "
                               & To_String (Tail (Chains (a).Last_Element.Signature, 20))
                               & " : "
                               & Authorities'Image (a));
                  end if;
               end loop;
               New_Line;
               Put ("Common blockchain height  :");
               Put (Common_Height, 4);
               New_Line;
               Put ("Longest blockchain height :");
               Put (Longest_Height, 4);
               New_Line;
               Put_Line ("--------------------------------------------------------------------------");
            end;
         end;

         New_Line;
      end loop;
   exception
      when E : others => Put_Line (Image (Current_Task) & ": " & Exception_Information (E));
   end Monitor;

   procedure Register_Task_Internal (Id : Authorities) is

   begin
      Task_Id_Map.Add_Task_Id (Current_Task, Id);
   end Register_Task_Internal;

   procedure Send_Internal (To : Authorities; b : Block) is

   begin
      Internal_Connections.Send (Encode_Connections (From => Task_Id_Map.Id_Task (Current_Task),
                                                     To   => To)) (b);
   end Send_Internal;

   procedure Query_Internal (To : Authorities; q : Block_Query; r : out Blockchain) is

   begin
      Internal_Connections.Query (Encode_Connections (From => Task_Id_Map.Id_Task (Current_Task),
                                                      To   => To)) (q, r);
   end Query_Internal;

   procedure Initialise_Network is

   begin
      Reset (Zero_to_One_Generator);
      Reset (Attack_Generator);
      Reset (Authorities_Generator);

      for a in Authorities loop
         Buffers  (a).Identify (a);
         Triggers (a).Identify (a);
      end loop;
   end Initialise_Network;

   procedure Open_Connections is

   begin
      Internal_Connections.Open_Conns;
   end Open_Connections;

   procedure Identify_Replicas is

   begin
      for a in Authorities loop
         declare
            Replica_Task_Id : Task_Id := Null_Task_Id;
         begin
            Rep (a).Identify (a, Replica_Task_Id);
            Task_Id_Map.Add_Task_Id (Replica_Task_Id, a);
         end;
      end loop;
   end Identify_Replicas;

end Network_Internal;
