with Ada.Strings.Fixed;

package body Tagatha.Operands.Images is

   function Context_Image (Context : Operand_Context) return String
   is ((if Context.Is_Address then "&" else "")
       & Data_Image (Context.Data)
       & (if Context.Size /= Default_Size
         then "/" & Size_Image (Context.Size)
         else ""));

   type Standard_Image_Type is
     new Tagatha.Operands.Operand_Image_Interface with null record;

   overriding function No_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context)
      return String
   is ("<>" & Context_Image (Context));

   overriding function Stack_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context)
      return String
   is ("stack" & Context_Image (Context));

   overriding function Frame_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context;
      Offset     : Frame_Offset)
      return String
   is ((if Offset < 0 then "local" else "arg")
       & Frame_Offset'Image (-(abs Offset))
       & Context_Image (Context));

   overriding function External_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context;
      Name       : String;
      Absolute   : Boolean)
      return String
   is ((if Absolute then "@#" else "") & Name & Context_Image (Context));

   overriding function Integer_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context;
      Value      : Tagatha_Integer)
      return String
   is (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left)
       & Context_Image (Context));

   overriding function Floating_Point_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context;
      Value      : Tagatha_Floating_Point)
      return String
   is (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left)
       & Context_Image (Context));

   overriding function Register_Operand
     (Image      : Standard_Image_Type;
      Context    : Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register)
      return String
   is (Group.Name
       & "-"
       & Ada.Strings.Fixed.Trim (Register'Image, Ada.Strings.Left)
       & Context_Image (Context));

   --------------------
   -- Standard_Image --
   --------------------

   function Standard_Image return Operand_Image_Interface'Class is
   begin
      return Img : Standard_Image_Type;
   end Standard_Image;

end Tagatha.Operands.Images;
