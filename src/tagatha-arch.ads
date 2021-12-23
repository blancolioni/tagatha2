private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

with Tagatha.Assembler;

with Tagatha.Labels;
with Tagatha.Operands;
with Tagatha.Registers;

package Tagatha.Arch is

   subtype Parent is Tagatha.Assembler.Instance;

   type Instance (Word_Size : Positive) is abstract new Parent with private;
   subtype Any_Instance is Instance'Class;

   function Name (This : Instance) return String is abstract;

   function Address_Size (This : Instance) return Natural;
   function Floating_Point_Size (This : Instance) return Natural;
   function Integer_Size (This : Instance) return Natural;

   function Size_Bits
     (This : Instance;
      Size : Tagatha_Size)
      return Natural;

   procedure Move
     (This        : in out Instance;
      Source      : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type)
   is abstract;

   procedure Operate
     (This        : in out Instance;
      Operator    : Tagatha_Operator;
      Source_1    : Tagatha.Operands.Operand_Type;
      Source_2    : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type)
   is abstract;

   procedure Call
     (This           : in out Instance;
      Destination    : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural)
   is abstract;

   procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label)
   is abstract;

   procedure Change_Stack
     (This   : in out Instance;
      Change : Integer)
   is abstract;

   procedure Label
     (This : in out Instance;
      Name : String)
   is abstract;

   procedure Local_Label
     (This  : in out Instance;
      Label : Tagatha.Labels.Label)
   is abstract;

   procedure Begin_Generation
     (This             : in out Instance;
      Source_File_Name : String);

   procedure End_Generation
     (This             : in out Instance);

   procedure Start_Routine
     (This   : in out Instance;
      Name   : String;
      Global : Boolean)
   is abstract;

   procedure End_Routine
     (This   : in out Instance;
      Name   : String)
   is abstract;

   function Get
     (Name : String)
      return Any_Instance;

   function Get_Group
     (This : Instance;
      Data : Tagatha_Data_Type;
      Size : Tagatha_Size)
      return Tagatha.Registers.Register_Group'Class
      is abstract;

   function Allocate
     (This  : in out Instance;
      Group : Tagatha.Registers.Register_Group'Class)
      return Tagatha.Registers.Register;

   procedure Deallocate
     (This     : in out Instance;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register);

   type Operand_Process is
     abstract new Operands.Operand_Process_Interface with private;

   procedure Put
     (This    : in out Any_Instance;
      Process : in out Operand_Process'Class;
      Operand : Operands.Operand_Type);

private

   subtype Dispatch is Instance'Class;

   package Active_Register_Vectors is
     new Ada.Containers.Vectors (Tagatha.Registers.Register, Boolean);

   package Register_Group_Maps is
     new WL.String_Maps (Active_Register_Vectors.Vector,
                         Active_Register_Vectors."=");

   type Instance (Word_Size : Positive) is abstract new Parent with
      record
         Source_File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Registers        : Register_Group_Maps.Map;
      end record;

   function Address_Size (This : Instance) return Natural
   is (This.Word_Size);

   function Floating_Point_Size (This : Instance) return Natural
   is (32);

   function Integer_Size (This : Instance) return Natural
   is (This.Word_Size);

   function Size_Bits
     (This : Instance;
      Size : Tagatha_Size)
      return Natural
   is (case Size.Category is
          when Tagatha_Default_Size => This.Word_Size,
          when Tagatha_Integer_Size => Dispatch (This).Integer_Size,
          when Tagatha_Address_Size => Dispatch (This).Address_Size,
          when Tagatha_Floating_Point_Size =>
             Dispatch (This).Floating_Point_Size,
          when Tagatha_Custom_Size  => Size.Octets * 8);

   type Operand_Process is
     abstract new Operands.Operand_Process_Interface with
      record
         Lines : Tagatha.Assembler.Assembler_Line;
      end record;

   procedure Put
     (This        : in out Operand_Process;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "");

end Tagatha.Arch;
