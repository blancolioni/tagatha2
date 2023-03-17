private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Pdp11.ISA;

private package Tagatha.Arch.Pdp11_Generator is

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

   package Operand_Holders is
     new Ada.Containers.Indefinite_Holders
       (Tagatha.Operands.Operand_Type,
        Tagatha.Operands."=");

   type Command_Operand is
      record
         Operand : Pdp11.ISA.Operand_Type;
         Label   : Natural := 0;
         Offset  : Frame_Offset := 0;
         Value   : Tagatha_Integer := 0;
      end record;

   type Command is
      record
         Has_Instruction : Boolean := True;
         Label_List      : Label_Lists.List;
         Byte            : Boolean := False;
         Instruction     : Pdp11.ISA.Instruction_Type :=
                             Pdp11.ISA.I_CCC;
         Src, Dst        : Command_Operand;
         Branch_Label    : Natural := 0;
         --  Src_Operand     : Pdp11.ISA.Operand_Type := (others => <>);
         --  Dst_Operand     : Pdp11.ISA.Operand_Type := (others => <>);
      end record;

   function To_Assembly (This : Command) return String;

   package Command_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Command);

end Tagatha.Arch.Pdp11_Generator;
