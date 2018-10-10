with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers;
with Ada.Task_Identification;                use Ada.Task_Identification;

generic
   type Task_Index is (<>);

package Task_Id_Mapping is

   pragma Elaborate_Body;

   package Task_Lookup is new Indefinite_Ordered_Maps (Key_Type     => String,
                                                       Element_Type => Task_Index);
   use Task_Lookup;

   protected Task_Id_Map is
      procedure Add_Task_Id (Id : Task_Id; i : Task_Index);
      function Id_Task (Id : Task_Id) return Task_Index;
   private
      Lookup : Map := Empty_Map;
   end Task_Id_Map;

end Task_Id_Mapping;
