-- Uwe R. Zimmer, Australia, 2018

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Network;        use Network;

package body Replicas is

   use Block_Strings;

   task body Replica is

      Id : Authorities := Authorities'Invalid_Value;
      Connection_Loss_Timeout : constant Duration := 0.01; -- 10 ms

   begin
      accept Identify (Replica_Id : Authorities; Replica_Task_Id : out Task_Id) do
         Id              := Replica_Id;
         Replica_Task_Id := Current_Task;
      end Identify;

      declare

         -- Your basic declarations go here.

         Chains : Blockchains;

      begin
         Chains.Blocks.Include (Genesis_Block.Header, Genesis_Block);
         Chains.End_of_Longest_Blockchain := Genesis_Block;

         loop

            null; -- Your code goes here.

         end loop;
      end;
   exception
      when E : others => Put_Line (Image (Current_Task) & ": " & Exception_Information (E));
   end Replica;

end Replicas;
