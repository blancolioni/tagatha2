private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Fixed;

private package Tagatha.Arch.M6502 is

   function Get return Any_Instance;

private

   Stack_Base     : constant := 16#0100#;
   Max_Frame_Size : constant := 16;

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

   type Argument_Type is (Source_1, Source_2, Destination);

   package Operand_Holders is
     new Ada.Containers.Indefinite_Holders
       (Tagatha.Operands.Operand_Type, Tagatha.Operands."=");

   type Argument_Record is
      record
         Active  : Boolean := False;
         Byte    : Natural := 0;
         Operand : Operand_Holders.Holder;
      end record;

   type Argument_Array is
     array (Argument_Type) of Argument_Record;

   subtype Parent is Tagatha.Arch.Instance;

   type Instance is new Parent with
      record
         Last_Op   : Tagatha_Operator := Op_Nop;
         Have_FP   : Boolean := False;
         --  True when Y register contains FP
         Arguments : Argument_Array;
      end record;

   subtype Any_Instance is Instance'Class;

   overriding function Name
     (This : Instance)
      return String
   is ("6502");

   overriding procedure Start_Routine
     (This   : in out Instance;
      Name   : String;
      Args   : Natural;
      Global : Boolean);

   overriding procedure End_Routine
     (This   : in out Instance;
      Name   : String);

   overriding procedure Move
     (This        : in out Instance;
      Source      : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type);

   overriding procedure Operate
     (This        : in out Instance;
      Operator    : Tagatha_Operator;
      Source_1    : Tagatha.Operands.Operand_Type;
      Source_2    : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type);

   overriding procedure Call
     (This           : in out Instance;
      Destination    : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural);

   overriding procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label);

   overriding procedure Change_Stack
     (This   : in out Instance;
      Change : Integer);

   overriding procedure Label
     (This : in out Instance;
      Name : String);

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Tagatha.Labels.Label);

   overriding procedure Put
     (This        : in out Instance;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "");

   overriding function Get_Group
     (This : Instance;
      Data : Tagatha_Data_Type;
      Size : Tagatha_Size)
      return Tagatha.Registers.Register_Group'Class;

   overriding function Address_Size (This : Instance) return Natural
   is (16);

   function Hex (X : Natural) return String;
   function Imm (X : Integer) return String
   is ("#" & Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Left));

end Tagatha.Arch.M6502;
