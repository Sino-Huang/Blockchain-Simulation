-- Uwe R. Zimmer, Australia, 2018

with Authorized_Nodes; use Authorized_Nodes;
with Blocks;           use Blocks;

package Network is

   procedure Register_Task (Id : Authorities);
   -- Your main Replica task registers automatically via its Identify entry,
   -- yet other tasks which you may wish to spin off from there will need to
   -- register with this call, _before_ they can use the procedures below.

   procedure Send (To : Authorities; b : Block);

   procedure Query (To : Authorities; q : Block_Query; r : out Blockchain);
   -- a Block_Query of (Natural'First, Natural'Last) will request the full blockchain

   -- Warning !!!
   --
   -- If Connectivity is less than 1.0 then Send and Query may never complete.
   --
   -- Use time-out structures to handle this, e.g.
   --
   --      select
   --         delay Connection_Loss_Timeout;
   --         Put_Line ("No connection from " & Authorities'Image (Id) & " to " & Authorities'Image (To));
   --      then abort
   --         Send (To, New_Block);
   --      end select;
   --
   -- assuming sensible values for Connection_Loss_Timeout, To and New_Block.

end Network;
