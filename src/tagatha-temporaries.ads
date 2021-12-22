private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Vectors;

package Tagatha.Temporaries is

   type Temporary is private;

   function Image (T : Temporary) return String;

   type Temporary_Source is tagged private;

   function Create
     (Source     : in out Temporary_Source;
      Is_Address : Boolean := False)
      return Temporary;

   function Exists
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean;

   function Is_Initialized
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean;

   function Is_Active
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean;

   --  function Group
   --    (Container : Temporary_Source;
   --     T         : Temporary)
   --     return Register_Group'Class
   --    with Pre => Container.Is_Active (T);

   function Assignment
     (Container : Temporary_Source;
      T         : Temporary)
      return Natural
     with Pre => Container.Is_Active (T);

   procedure Activate
     (Container : in out Temporary_Source;
      T         : Temporary)
     with Pre => Container.Exists (T)
     and then not Container.Is_Active (T),
       Post => Container.Is_Active (T);

   procedure Deactivate
     (Container : in out Temporary_Source;
      T         : Temporary)
     with Pre => Container.Exists (T)
     and then Container.Is_Active (T),
     Post => not Container.Is_Active (T);

   procedure Set_Initialized
     (Container : in out Temporary_Source;
      T         : Temporary)
     with Pre => Container.Exists (T)
     and then Container.Is_Active (T)
     and then not Container.Is_Initialized (T),
     Post => Container.Is_Initialized (T);

   --  procedure Set_Assignment
   --    (Container  : in out Temporary_Source;
   --     T          : Temporary;
   --     Group      : Register_Group'Class;
   --     Assignment : Natural)
   --    with Pre => Container.Exists (T)
     --  and then Container.Is_Active (T)
     --  and then not Container.Is_Initialized (T),
     --  Post => Container.Is_Initialized (T);

private

   type Temporary is new Positive;

   function Image (T : Temporary) return String
   is ("t" & Integer'Image (-Integer (T)));

   type Register_Group is abstract tagged null record;

   package Register_Holders is
     new Ada.Containers.Indefinite_Holders (Register_Group'Class);

   type Temporary_State_Record is
      record
         Active      : Boolean := False;
         Initialized : Boolean := False;
         Is_Address  : Boolean := False;
         Group       : Register_Holders.Holder;
         Assignment  : Natural := 0;
      end record;

   package State_Vectors is
     new Ada.Containers.Vectors (Temporary, Temporary_State_Record);

   type Temporary_Source is new State_Vectors.Vector with null record;

   function Exists
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean
   is (T <= Container.Last_Index);

   function Is_Initialized
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean
   is (Container.Exists (T) and then Container (T).Initialized);

   function Is_Active
     (Container : Temporary_Source;
      T         : Temporary)
      return Boolean
   is (Container.Exists (T) and then Container (T).Active);

   --  function Group
   --    (Container : Temporary_Source;
   --     T         : Temporary)
   --     return Register_Group'Class
   --  is (Container (T).Group.Element);

   function Assignment
     (Container : Temporary_Source;
      T         : Temporary)
      return Natural
   is (Container (T).Assignment);

end Tagatha.Temporaries;
