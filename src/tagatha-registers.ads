package Tagatha.Registers is

   type Register_Group is abstract tagged private;

   function Name
     (Group : Register_Group) return String
      is abstract;

   type Register is new Natural;

   function Last_Register
     (Group : Register_Group)
      return Register
      is abstract;

private

   type Register_Group is abstract tagged null record;

end Tagatha.Registers;
