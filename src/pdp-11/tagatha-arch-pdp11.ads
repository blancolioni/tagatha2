private package Tagatha.Arch.Pdp11 is

   function Get return Any_Instance;

private

   type Register_Type is
     (General_Register, Floating_Point_Register);

   type Register_Group is
     new Tagatha.Registers.Register_Group with
      record
         Group_Type  : Register_Type;
         First, Last : Tagatha.Registers.Register;
      end record;

   overriding function Name
     (Group : Register_Group)
      return String
   is (case Group.Group_Type is
          when General_Register        => "general",
          when Floating_Point_Register => "floating point");

   overriding function Last_Register
     (Group : Register_Group)
      return Tagatha.Registers.Register
   is (Tagatha.Registers."-" (Group.Last, Group.First));

end Tagatha.Arch.Pdp11;
