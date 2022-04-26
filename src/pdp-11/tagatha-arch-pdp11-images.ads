private package Tagatha.Arch.Pdp11.Images is

   function Source_Operand_Image
     (Multiword : Boolean := False)
     return Tagatha.Operands.Operand_Image_Interface'Class;

   function Destination_Operand_Image
     (Multiword : Boolean := False)
      return Tagatha.Operands.Operand_Image_Interface'Class;

end Tagatha.Arch.Pdp11.Images;
