with Tagatha.Operands.Images;

package body Tagatha.Operands is

   -----------
   -- Image --
   -----------

   function Image
     (This       : Operand_Image_Interface'Class;
      Operand    : Operand_Type)
      return String
   is
      Context : constant Operand_Context := Operand_Context'
        (Data       => Operand.Data,
         Size       => Operand.Size,
         Is_Address => Operand.Is_Address,
         Is_Initialized =>
           Operand.Class /= Register_Operand
         or else Operand.Initialized,
         Is_Indexed => Operand.Is_Indexed,
         Word_Index => Operand.Word_Index);
   begin
      case Operand.Class is
         when No_Operand =>
            return This.No_Operand (Context);
         when Stack_Operand =>
            return This.Stack_Operand (Context);
         when Frame_Operand =>
            return This.Frame_Operand (Context, Operand.Offset);
         when External_Operand =>
            return This.External_Operand
              (Context, Ada.Strings.Unbounded.To_String (Operand.Name),
               Operand.Absolute);
         when Constant_Operand =>
            case Operand.Value.Class is
               when Integer_Constant =>
                  return This.Integer_Operand
                    (Context, Operand.Value.Integer_Value);
               when Floating_Point_Constant =>
                  return This.Floating_Point_Operand
                    (Context, Operand.Value.Float_Value);
            end case;
         when Register_Operand =>
            return This.Register_Operand
              (Context, Operand.Group.Element, Operand.Register);
      end case;
   end Image;

   -------------------
   -- Index_Operand --
   -------------------

   function Index_Operand
     (Op    : Operand_Type;
      Index : Natural)
      return Operand_Type
   is
   begin
      return Result : Operand_Type := Op do
         Result.Word_Index := Index;
      end return;
   end Index_Operand;

   -------------
   -- Process --
   -------------

   procedure Process
     (This    : in out Operand_Process_Interface'Class;
      Operand : Operand_Type)
   is
      Context : constant Operand_Context := Operand_Context'
        (Data           => Operand.Data,
         Size           => Operand.Size,
         Is_Address     => Operand.Is_Address,
         Is_Initialized =>
           Operand.Class /= Register_Operand
         or else Operand.Initialized,
         Is_Indexed     => Operand.Is_Indexed,
         Word_Index     => Operand.Word_Index);
   begin
      case Operand.Class is
         when No_Operand =>
            This.No_Operand (Context);
         when Stack_Operand =>
            This.Stack_Operand (Context);
         when Frame_Operand =>
            This.Frame_Operand (Context, Operand.Offset);
         when External_Operand =>
            This.External_Operand
              (Context,
               Ada.Strings.Unbounded.To_String (Operand.Name),
               Operand.Absolute);
         when Constant_Operand =>
            case Operand.Value.Class is
               when Integer_Constant =>
                  This.Integer_Operand
                    (Context, Operand.Value.Integer_Value);
               when Floating_Point_Constant =>
                  This.Floating_Point_Operand
                    (Context, Operand.Value.Float_Value);
            end case;
         when Register_Operand =>
            This.Register_Operand
              (Context, Operand.Group.Element, Operand.Register);
      end case;
   end Process;

   --------------------
   -- Standard_Image --
   --------------------

   function Standard_Image return Operand_Image_Interface'Class is
   begin
      return Images.Standard_Image;
   end Standard_Image;

   ----------------------
   -- Update_Registers --
   ----------------------

   procedure Update_Registers
     (Operand : Operand_Type;
      Update  : not null access
        procedure (Group : Tagatha.Registers.Register_Group'Class;
                   Register : Tagatha.Registers.Register))
   is
   begin
      if Operand.Class = Register_Operand then
         Update (Operand.Group.Element, Operand.Register);
      end if;
   end Update_Registers;

end Tagatha.Operands;
