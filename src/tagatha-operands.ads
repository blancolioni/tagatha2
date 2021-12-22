private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Unbounded;

with Tagatha.Registers;

package Tagatha.Operands is

   type Operand_Type (<>) is private;

   function Is_Address
     (This : Operand_Type)
      return Boolean;

   function Is_Reference
     (This : Operand_Type)
      return Boolean;

   function Take_Address
     (This : Operand_Type)
      return Operand_Type
     with Pre => not Is_Address (This),
     Post => Is_Address (Take_Address'Result);

   function Dereference
     (This : Operand_Type)
      return Operand_Type
     with Pre => Is_Address (This),
       Post => not Is_Address (Dereference'Result);

   function Indirect
     (This : Operand_Type)
      return Operand_Type
     with Pre => not Is_Reference (This),
       Post => Is_Reference (Indirect'Result);

   function Size
     (This : Operand_Type)
      return Tagatha_Size;

   function Data_Type
     (This : Operand_Type)
      return Tagatha_Data_Type;

   function Set_Size
     (Size : Tagatha_Size;
      This : Operand_Type)
      return Operand_Type;

   function Set_Data_Type
     (Data : Tagatha_Data_Type;
      This : Operand_Type)
      return Operand_Type;

   function Register_Assignment_Operand
     (Operand  : Operand_Type;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register)
      return Operand_Type;

   function Register_Operand
     (Operand  : Operand_Type;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register)
      return Operand_Type;

   procedure Update_Registers
     (Operand : Operand_Type;
      Update  : not null access
        procedure (Group : Tagatha.Registers.Register_Group'Class;
                   Register : Tagatha.Registers.Register));

   function No_Operand
     return Operand_Type;

   function Stack_Operand
     return Operand_Type;

   function Frame_Operand
     (Offset : Frame_Offset)
      return Operand_Type;

   function Local_Operand
     (Offset : Local_Offset)
      return Operand_Type;

   function Argument_Operand
     (Offset : Argument_Offset)
      return Operand_Type;

   function External_Operand
     (Name : String)
      return Operand_Type;

   function Constant_Operand
     (Value : Tagatha_Integer)
      return Operand_Type;

   function Constant_Operand
     (Value : Tagatha_Floating_Point)
      return Operand_Type;

   type Operand_Context is
      record
         Data           : Tagatha_Data_Type := Untyped_Data;
         Size           : Tagatha_Size := Default_Size;
         Is_Address     : Boolean := False;
         Is_Initialized : Boolean := False;
         Word_Index     : Natural := 0;
      end record;

   type Operand_Process_Interface is interface;

   procedure No_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context)
   is null;

   procedure Stack_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context)
   is null;

   procedure Frame_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context;
      Offset  : Frame_Offset)
   is null;

   procedure External_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context;
      Name    : String)
   is null;

   procedure Integer_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context;
      Value   : Tagatha_Integer)
   is null;

   procedure Floating_Point_Operand
     (Process : in out Operand_Process_Interface;
      Context : Operand_Context;
      Value   : Tagatha_Floating_Point)
   is null;

   procedure Register_Operand
     (Process   : in out Operand_Process_Interface;
      Context   : Operand_Context;
      Group     : Tagatha.Registers.Register_Group'Class;
      Register  : Tagatha.Registers.Register)
   is null;

   procedure Process
     (This    : in out Operand_Process_Interface'Class;
      Operand : Operand_Type);

   type Operand_Image_Interface is interface;

   function No_Operand
     (Image   : Operand_Image_Interface;
      Context : Operand_Context)
      return String
      is abstract;

   function Stack_Operand
     (Image   : Operand_Image_Interface;
      Context : Operand_Context)
      return String
      is abstract;

   function Frame_Operand
     (Image   : Operand_Image_Interface;
      Context : Operand_Context;
      Offset  : Frame_Offset)
      return String
      is abstract;

   function External_Operand
     (Image      : Operand_Image_Interface;
      Context    : Operand_Context;
      Name       : String)
      return String
      is abstract;

   function Integer_Operand
     (Image      : Operand_Image_Interface;
      Context    : Operand_Context;
      Value      : Tagatha_Integer)
      return String
      is abstract;

   function Floating_Point_Operand
     (Image      : Operand_Image_Interface;
      Context    : Operand_Context;
      Value      : Tagatha_Floating_Point)
      return String
      is abstract;

   function Register_Operand
     (Image      : Operand_Image_Interface;
      Context    : Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register)
      return String
      is abstract;

   function Image
     (This    : Operand_Image_Interface'Class;
      Operand : Operand_Type)
      return String;

   function Image
     (This        : Operand_Image_Interface'Class;
      Operand     : Operand_Type;
      Word_Index  : Natural)
      return String;

   function Standard_Image return Operand_Image_Interface'Class;

   function Image (Operand : Operand_Type) return String;

private

   type Operand_Class is
     (No_Operand,
      Stack_Operand,
      Frame_Operand,
      External_Operand,
      Constant_Operand,
      Register_Operand);

   type Constant_Class is (Integer_Constant, Floating_Point_Constant);

   type Constant_Type (Class : Constant_Class := Integer_Constant) is
      record
         case Class is
            when Integer_Constant =>
               Integer_Value : Tagatha_Integer;
            when Floating_Point_Constant =>
               Float_Value   : Tagatha_Floating_Point;
         end case;
      end record;

   package Register_Group_Holders is
     new Ada.Containers.Indefinite_Holders
       (Tagatha.Registers.Register_Group'Class,
        Tagatha.Registers."=");

   type Operand_Type (Class : Operand_Class) is
      record
         Data         : Tagatha_Data_Type := Untyped_Data;
         Size         : Tagatha_Size := Default_Size;
         Is_Address   : Boolean := False;
         Is_Reference : Boolean := False;
         case Class is
            when No_Operand =>
               null;
            when Stack_Operand =>
               null;
            when Frame_Operand =>
               Offset : Frame_Offset;
            when External_Operand =>
               Name   : Ada.Strings.Unbounded.Unbounded_String;
            when Constant_Operand =>
               Value  : Constant_Type;
            when Register_Operand =>
               Group       : Register_Group_Holders.Holder;
               Register    : Tagatha.Registers.Register;
               Initialized : Boolean;
         end case;
      end record;

   function No_Operand
     return Operand_Type
   is (Class => No_Operand, others => <>);

   function Stack_Operand
     return Operand_Type
   is (Class => Stack_Operand, others => <>);

   function Is_Address
     (This : Operand_Type)
      return Boolean
   is (This.Is_Address);

   function Is_Reference
     (This : Operand_Type)
      return Boolean
   is (This.Is_Reference);

   function Take_Address
     (This : Operand_Type)
      return Operand_Type
   is ((This with delta Is_Address => True));

   function Indirect
     (This : Operand_Type)
      return Operand_Type
   is ((This with delta Is_Reference => True));

   function Dereference
     (This : Operand_Type)
      return Operand_Type
   is ((This with delta Is_Address => False));

   function Size
     (This : Operand_Type)
      return Tagatha_Size
   is (This.Size);

   function Data_Type
     (This : Operand_Type)
      return Tagatha_Data_Type
   is (This.Data);

   function Set_Size
     (Size : Tagatha_Size;
      This : Operand_Type)
      return Operand_Type
   is ((This with delta Size => Size));

   function Set_Data_Type
     (Data : Tagatha_Data_Type;
      This : Operand_Type)
      return Operand_Type
   is ((This with delta Data => Data));

   function Constant_Operand
     (Value : Tagatha_Integer)
      return Operand_Type
   is (Operand_Type'
         (Class      => Constant_Operand,
          Value      => (Integer_Constant, Value),
          others     => <>));

   function Constant_Operand
     (Value : Tagatha_Floating_Point)
      return Operand_Type
   is (Operand_Type'
         (Class      => Constant_Operand,
          Value      => (Floating_Point_Constant, Value),
          Data       => Floating_Point_Data,
          Size       => Default_Floating_Point_Size,
          others     => <>));

   function Frame_Operand
     (Offset : Frame_Offset)
      return Operand_Type
   is (Operand_Type'
         (Class  => Frame_Operand,
          Offset => Offset,
          others => <>));

   function Local_Operand
     (Offset : Local_Offset)
      return Operand_Type
   is (Frame_Operand (-Frame_Offset (Offset)));

   function Argument_Operand
     (Offset : Argument_Offset)
      return Operand_Type
   is (Frame_Operand (Frame_Offset (Offset)));

   function External_Operand
     (Name : String)
      return Operand_Type
   is (Operand_Type'
         (Class      => External_Operand,
          Name       => Ada.Strings.Unbounded.To_Unbounded_String (Name),
          others     => <>));

   function Register_Assignment_Operand
     (Operand  : Operand_Type;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register)
      return Operand_Type
   is (Operand_Type'
         (Class        => Register_Operand,
          Data         => Operand.Data,
          Size         => (if Operand.Is_Address
                           then Default_Address_Size
                           else Operand.Size),
          Is_Address   => Operand.Is_Address,
          Is_Reference => False,
          Group        => Register_Group_Holders.To_Holder (Group),
          Register     => Register,
          Initialized  => False));

   function Register_Operand
     (Operand  : Operand_Type;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register)
      return Operand_Type
   is (Operand_Type'
         (Class        => Register_Operand,
          Data         => Operand.Data,
          Size         => (if Operand.Is_Address
                           then Default_Address_Size
                           else Operand.Size),
          Is_Address   => Operand.Is_Address,
          Is_Reference => False,
          Group        => Register_Group_Holders.To_Holder (Group),
          Register     => Register,
          Initialized  => True));

   Default_Operand_Context : constant Operand_Context := (others => <>);

   function Image
     (This    : Operand_Image_Interface'Class;
      Operand : Operand_Type)
      return String
   is (This.Image (Operand, Word_Index => 0));

   function Image (Operand : Operand_Type) return String
   is (Standard_Image.Image (Operand));

end Tagatha.Operands;
