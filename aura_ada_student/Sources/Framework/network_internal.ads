-- Uwe R. Zimmer, Australia, 2018

with Authorized_Nodes;              use Authorized_Nodes;
with Blocks;                        use Blocks;

package Network_Internal is

   pragma Elaborate_Body;

   procedure Register_Task_Internal (Id : Authorities);

   procedure Send_Internal (To : Authorities; b : Block);

   procedure Query_Internal (To : Authorities; q : Block_Query; r : out Blockchain);

   procedure Initialise_Network;
   procedure Open_Connections;
   procedure Identify_Replicas;

end Network_Internal;
