-- Uwe R. Zimmer, Australia, 2018

with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Task_Identification; use Ada.Task_Identification;
with Authorized_Nodes;        use Authorized_Nodes;
with Blocks;                  use Blocks;

package Replicas is

   task type Replica is
      entry Identify (Replica_Id : Authorities; Replica_Task_Id : out Task_Id);

      entry Block_Generator      (s : Seconds_Count);
      entry Block_Receiver       (b : Block);
      entry Block_Query_Receiver (q : Block_Query; r : out Blockchain);
      -- a Block_Query of (Natural'First, Natural'Last) will request the full blockchain
   end Replica;

end Replicas;
