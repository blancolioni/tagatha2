package body Tagatha is

   ------------------
   -- Bits_To_Size --
   ------------------

   function Bits_To_Size (Bits : Natural) return Tagatha_Size is
   begin
      return (Tagatha_Custom_Size, (Bits + 7) / 8);
   end Bits_To_Size;

   ----------------
   -- Data_Image --
   ----------------

   function Data_Image (Data : Tagatha_Data_Type) return String is
   begin
      return (case Data is
                 when Untyped_Data => "",
                 when Address_Data => "a",
                 when Floating_Point_Data => "f");
   end Data_Image;

   ------------
   -- Negate --
   ------------

   function Negate (Cond : Tagatha_Condition) return Tagatha_Condition is
   begin
      case Cond is
         when C_Always =>
            raise Constraint_Error with
              "Can't negate always condition";
         when C_Equal =>
            return C_Not_Equal;
         when C_Not_Equal =>
            return C_Equal;
         when C_Greater =>
            return C_At_Most;
         when C_Less =>
            return C_At_Least;
         when C_At_Most =>
            return C_Less;
         when C_At_Least =>
            return C_Greater;
      end case;
   end Negate;

   ---------------
   -- Size_Bits --
   ---------------

   function Size_Bits (Size : Tagatha_Size) return Natural is
   begin
      return Size_Octets (Size) * 8;
   end Size_Bits;

   -----------------
   -- Size_Octets --
   -----------------

   function Size_Octets (Size : Tagatha_Size) return Natural is
   begin
      case Size.Category is
         when Tagatha_Default_Size =>
            return 4;
         when Tagatha_Integer_Size =>
            return 4;
         when Tagatha_Address_Size =>
            return 4;
         when Tagatha_Floating_Point_Size =>
            return 4;
         when Tagatha_Custom_Size =>
            return Size.Octets;
      end case;
   end Size_Octets;

   ------------------
   -- To_Condition --
   ------------------

   function To_Condition (Op : Condition_Operator) return Tagatha_Condition is
   begin
      return (case Op is
                 when Op_Equal => C_Equal,
                 when Op_Not_Equal => C_Not_Equal,
                 when Op_Greater   => C_Greater,
                 when Op_Less      => C_Less,
                 when Op_Greater_Equal => C_At_Least,
                 when Op_Less_Equal => C_At_Most);
   end To_Condition;

end Tagatha;
