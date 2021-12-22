package Tagatha is

   pragma Pure (Tagatha);

   type Tagatha_Size is private;

   Default_Size                : constant Tagatha_Size;
   Default_Integer_Size        : constant Tagatha_Size;
   Default_Address_Size        : constant Tagatha_Size;
   Default_Floating_Point_Size : constant Tagatha_Size;

   Size_8  : constant Tagatha_Size;
   Size_16 : constant Tagatha_Size;
   Size_32 : constant Tagatha_Size;
   Size_64 : constant Tagatha_Size;

   function Size_Bits (Size : Tagatha_Size) return Natural;
   function Size_Octets (Size : Tagatha_Size) return Natural;
   function Bits_To_Size (Bits : Natural) return Tagatha_Size;

   type Tagatha_Data_Type is
     (Untyped_Data, Address_Data, Floating_Point_Data);

   function Data_Image (Data : Tagatha_Data_Type) return String;

   type Local_Offset is new Positive;
   type Argument_Offset is new Positive;
   type Frame_Offset is new Integer;

   type Tagatha_Segment is (Executable, Read_Only, Read_Write);

   type Tagatha_Operator is
     (Op_Nop,
      Op_Add, Op_Sub, Op_Mul, Op_Div, Op_Mod,
      Op_And, Op_Or, Op_Xor,
      Op_Equal, Op_Not_Equal,
      Op_Greater, Op_Less,
      Op_Greater_Equal, Op_Less_Equal,
      Op_Bit_Test, Op_Bit_Clear, Op_Bit_Set, Op_Bit_Slice,
      Op_Logical_Shift,
      Op_Compare, Op_Change_Size,
      Op_Negate, Op_Not, Op_Complement, Op_Test);

   subtype Multiplication_Operator is
     Tagatha_Operator range Op_Mul .. Op_Mod;

   subtype Zero_Argument_Operator is
     Tagatha_Operator range Op_Nop .. Op_Nop;

   subtype One_Argument_Operator is
     Tagatha_Operator range Op_Negate .. Op_Test;

   subtype Two_Argument_Operator is
     Tagatha_Operator range Op_Add .. Op_Change_Size;

   subtype Condition_Operator is
     Tagatha_Operator range Op_Equal .. Op_Less_Equal;

   type Tagatha_Condition is
     (C_Always,
      C_Equal, C_Not_Equal,
      C_Greater, C_Less,
      C_At_Least, C_At_Most);

   function Negate (Cond : Tagatha_Condition) return Tagatha_Condition;
   function To_Condition (Op : Condition_Operator) return Tagatha_Condition;

   type Tagatha_Integer is mod 2 ** 64;
   type Tagatha_Integer_Array is array (Positive range <>) of Tagatha_Integer;

   type Tagatha_Floating_Point is new Float;
   type Tagatha_Floating_Point_Array is
     array (Positive range <>) of Tagatha_Floating_Point;

   type Floating_Point_Integer is mod 2 ** 32;

   type Tagatha_Source_Position is
      record
         Line, Column : Positive;
      end record;

private

   type Tagatha_Size_Category is
     (Tagatha_Default_Size,
      Tagatha_Integer_Size,
      Tagatha_Address_Size,
      Tagatha_Floating_Point_Size,
      Tagatha_Custom_Size);

   type Tagatha_Size (Category : Tagatha_Size_Category :=
                        Tagatha_Default_Size) is
      record
         case Category is
            when Tagatha_Custom_Size =>
               Octets : Natural;
            when others =>
               null;
         end case;
      end record;

   Default_Size         : constant Tagatha_Size :=
                            Tagatha_Size'(Category => Tagatha_Default_Size);

   Default_Integer_Size : constant Tagatha_Size :=
                            Tagatha_Size'(Category => Tagatha_Integer_Size);

   Default_Address_Size : constant Tagatha_Size :=
                            Tagatha_Size'(Category => Tagatha_Address_Size);

   Default_Floating_Point_Size : constant Tagatha_Size :=
                                   Tagatha_Size'
                                     (Category =>
                                         Tagatha_Floating_Point_Size);

   Size_8  : constant Tagatha_Size := (Tagatha_Custom_Size, 1);
   Size_16 : constant Tagatha_Size := (Tagatha_Custom_Size, 2);
   Size_32 : constant Tagatha_Size := (Tagatha_Custom_Size, 4);
   Size_64 : constant Tagatha_Size := (Tagatha_Custom_Size, 8);

end Tagatha;
