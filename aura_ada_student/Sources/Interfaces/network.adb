with Network_Internal; use Network_Internal;

package body Network is

   procedure Register_Task (Id : Authorities) is

   begin
      Register_Task_Internal (Id);
   end Register_Task;

   procedure Send (To : Authorities; b : Block) is

   begin
      Send_Internal (To, b);
   end Send;

   procedure Query (To : Authorities; q : Block_Query; r : out Blockchain) is

   begin
      Query_Internal (To, q, r);
   end Query;

end Network;
