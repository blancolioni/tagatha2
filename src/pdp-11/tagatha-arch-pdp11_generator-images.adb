with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;

package body Tagatha.Arch.Pdp11_Generator.Images is

   type Operand_Image_Type is
     new Tagatha.Operands.Operand_Image_Interface with
      record
         Source    : Boolean;
         Multiword : Boolean;
      end record;

   overriding function No_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context)
      return String
   is ("");

   overriding function Stack_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context)
      return String
   is (if Image.Source
       then "(sp)+"
       else "-(sp)");

   overriding function Frame_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Offset     : Frame_Offset)
      return String;

   overriding function External_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Name       : String;
      Absolute   : Boolean)
      return String
   is ((if Context.Is_Address or else Absolute then "@#" else "")
       & Name
       & (if Context.Word_Index /= 0
         then " +" & Natural'Image (Context.Word_Index * 2)
         else ""));

   overriding function Integer_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Value      : Tagatha_Integer)
      return String
   is ("#" & Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left));

   overriding function Floating_Point_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Value      : Tagatha_Floating_Point)
      return String;

   overriding function Register_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register)
      return String;

   -------------------------------
   -- Destination_Operand_Image --
   -------------------------------

   function Destination_Operand_Image
     (Multiword : Boolean := False)
      return Tagatha.Operands.Operand_Image_Interface'Class
   is
   begin
      return Image : constant Operand_Image_Type :=
        Operand_Image_Type'(Source => False, Multiword => Multiword);
   end Destination_Operand_Image;

   ----------------------------
   -- Floating_Point_Operand --
   ----------------------------

   overriding function Floating_Point_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Value      : Tagatha_Floating_Point)
      return String
   is
      function To_Integer is
        new Ada.Unchecked_Conversion (Tagatha_Floating_Point,
                                      Floating_Point_Integer);
      Int_Value : constant Floating_Point_Integer :=
                    To_Integer (Value);
      Slice     : constant Floating_Point_Integer :=
                    (if Size_Bits (Context.Size) = 32
                     then Int_Value
                     else (Int_Value / 2 ** (16 * Context.Word_Index))
                     mod 2 ** 16);
   begin
      return "#" & Ada.Strings.Fixed.Trim (Slice'Image, Ada.Strings.Left);
   end Floating_Point_Operand;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding function Frame_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Offset     : Frame_Offset)
      return String
   is
      Base_Offset : constant Frame_Offset :=
                      2 * (Offset + (if Offset > 0 then 1 else 0));
      Adjusted_Offset : constant Frame_Offset :=
                          Base_Offset + Frame_Offset (Context.Word_Index) * 2;
   begin
      return Ada.Strings.Fixed.Trim (Adjusted_Offset'Image, Ada.Strings.Left)
        & "(r5)";
   end Frame_Operand;

   ----------------------
   -- Register_Operand --
   ----------------------

   overriding function Register_Operand
     (Image      : Operand_Image_Type;
      Context    : Tagatha.Operands.Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register)
      return String
   is
      use type Tagatha.Registers.Register;
      G : Register_Group'Class renames Register_Group'Class (Group);
      R : constant String :=
            (case G.Group_Type is
                when General_Register =>
               ('r', Character'Val (48 + G.First + Register)),
                when Floating_Point_Register =>
               ('a', 'c', Character'Val (48 + G.First + Register)));
   begin
      if not Context.Is_Initialized
        or else not Context.Is_Address
      then
         return R;
      elsif Image.Multiword then
         return "(" & R & ")+";
      else
         return "(" & R & ")";
      end if;
   end Register_Operand;

   --------------------------
   -- Source_Operand_Image --
   --------------------------

   function Source_Operand_Image
     (Multiword : Boolean := False)
      return Tagatha.Operands.Operand_Image_Interface'Class
   is
   begin
      return Image : constant Operand_Image_Type :=
        Operand_Image_Type'(Source => True, Multiword => Multiword);
   end Source_Operand_Image;

end Tagatha.Arch.Pdp11_Generator.Images;
