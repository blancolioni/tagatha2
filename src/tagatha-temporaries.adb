package body Tagatha.Temporaries is

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Container : in out Temporary_Source;
      T         : Temporary)
   is
   begin
      Container (T).Active := True;
   end Activate;

   ------------
   -- Create --
   ------------

   function Create
     (Source     : in out Temporary_Source;
      Is_Address : Boolean := False)
      return Temporary
   is
   begin
      Source.Append (Temporary_State_Record'
                       (Is_Address => Is_Address,
                        others     => <>));
      return Source.Last_Index;
   end Create;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Container : in out Temporary_Source;
      T         : Temporary)
   is
   begin
      Container (T).Active := False;
   end Deactivate;

   --------------------
   -- Set_Assignment --
   --------------------

   procedure Set_Assignment
     (Container  : in out Temporary_Source;
      T          : Temporary;
      Group      : Register_Group'Class;
      Assignment : Natural)
   is
   begin
      Container (T).Group := Register_Holders.To_Holder (Group);
      Container (T).Assignment := Assignment;
   end Set_Assignment;

   ---------------------
   -- Set_Initialized --
   ---------------------

   procedure Set_Initialized
     (Container : in out Temporary_Source;
      T         : Temporary)
   is
   begin
      Container (T).Initialized := True;
   end Set_Initialized;

end Tagatha.Temporaries;
