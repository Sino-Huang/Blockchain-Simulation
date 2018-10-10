package body Task_Id_Mapping is

   protected body Task_Id_Map is

      procedure Add_Task_Id (Id : Task_Id; i : Task_Index) is

      begin
         Lookup.Include (Key      => Image (Id),
                         New_Item => i);
      end Add_Task_Id;

      function Id_Task (Id : Task_Id) return Task_Index is
        (Lookup.Element (Key => Image (Id)));

   end Task_Id_Map;

end Task_Id_Mapping;
