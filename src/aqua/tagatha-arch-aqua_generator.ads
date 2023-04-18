private with Ada.Containers.Doubly_Linked_Lists;

private package Tagatha.Arch.Aqua_Generator is

   function Get return Any_Instance;

private

   package Label_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Positive);

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

   type Command_Operand is
      record
         R   : Natural := 0;
         Imm : Boolean := False;
         Def : Boolean := False;
      end record;

   type General_Command is
     (G_None, G_Operate, G_Branch, G_Call, G_Return, G_Set, G_Begin);

   type Label_Converter is access
     function (Label : String) return Positive;

   type Operation_Context is
      record
         First_Argument  : Natural := 0;
         Argument_Count  : Natural := 0;
         Return_Address  : Natural := 0;
         First_Temporary : Natural := 0;
         Temporary_Count : Natural := 0;
         First_Stack     : Natural := 0;
         Stack           : Natural := 0;
         To_Integer      : Label_Converter;
      end record;

   type Command is
      record
         General         : General_Command := G_None;
         Byte            : Boolean := False;
         Negated         : Boolean := False;
         Set_High        : Boolean := False;
         Clear_Other     : Boolean := False;
         Label_List      : Label_Lists.List;
         Operator        : Tagatha_Operator := Op_Nop;
         Condition       : Tagatha_Condition := C_Always;
         X, Y, Z         : Command_Operand;
         Immediate       : Natural := 0;
         Branch_Label    : Natural := 0;
         Save_J          : Natural := 0;
      end record;

   package Command_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Command);

end Tagatha.Arch.Aqua_Generator;
