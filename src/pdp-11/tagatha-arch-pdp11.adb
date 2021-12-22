with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
--  with Ada.Text_IO;

with Tagatha.Arch.Pdp11.Images;

package body Tagatha.Arch.Pdp11 is

   function Imm (X : Integer) return String
   is ("#" & Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Left));

   function Get_Instruction
     (Op : Tagatha_Operator)
     return String;

   function Get_Branch
     (Cond    : Tagatha_Condition;
      Negated : Boolean)
      return String;

   function Get_Label_Image
     (Label : Tagatha.Labels.Label)
      return String
   is ("L" & Tagatha.Labels.Image (Label));

   subtype Parent is Tagatha.Arch.Instance;

   type Instance is new Parent with
      record
         Last_Op : Tagatha_Operator := Op_Nop;
      end record;

   overriding function Name
     (This : Instance)
      return String
   is ("pdp-11");

   overriding procedure Start_Routine
     (This   : in out Instance;
      Name   : String;
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

   procedure Long_Operation
     (Left, Right : Tagatha.Operands.Operand_Type;
      Bits        : Natural;
      MSB_First   : Boolean;
      Operation   : not null access
        procedure (Left, Right : String));

   type Move_Operand is
     new Tagatha.Operands.Operand_Process_Interface with
      record
         Dst  : Ada.Strings.Unbounded.Unbounded_String;
         Line : Tagatha.Assembler.Assembler_Line;
      end record;

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset);

   ------------
   -- Branch --
   ------------

   overriding procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label)
   is
   begin
      if This.Last_Op in Condition_Operator
        and then Condition in C_Equal | C_Not_Equal
      then
         This.Put (Get_Branch
                   (To_Condition (This.Last_Op), Condition = C_Equal),
                   Get_Label_Image (Destination));
      else
         This.Put (Get_Branch (Condition, False),
                   Get_Label_Image (Destination));
      end if;
      This.Last_Op := Op_Nop;
   end Branch;

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (This           : in out Instance;
      Destination    : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural)
   is
   begin
      This.Put ("jsr", "pc", Images.Source_Operand_Image.Image (Destination));
   end Call;

   ------------------
   -- Change_Stack --
   ------------------

   overriding procedure Change_Stack
     (This   : in out Instance;
      Change : Integer)
   is
   begin
      if Change = 0 then
         null;
      elsif Change = 1 then
         This.Put ("tst", "-(sp)");
      elsif Change = -1 then
         This.Put ("tst", "(sp)+");
      else
         This.Put
           ((if Change > 0 then "sub" else "add"),
            Imm (abs Change * 2),
            "sp");
      end if;
   end Change_Stack;

   -----------------
   -- End_Routine --
   -----------------

   overriding procedure End_Routine
     (This   : in out Instance;
      Name   : String)
   is
   begin
      This.Put ("mov", "(sp)+", "r5");
      This.Put ("rts", "pc");
   end End_Routine;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset)
   is
      use Ada.Strings.Unbounded;
   begin
      if Context.Is_Address then
         declare
            Base_Offset     : constant Frame_Offset :=
                                2 * (Offset + (if Offset > 0 then 1 else 0));
            Adjusted_Offset : constant Frame_Offset :=
                                abs
                                  (Base_Offset
                                   + Frame_Offset (Context.Word_Index) * 2);
         begin
            Assembler.Put
              (This.Line, "mov", "r5", To_String (This.Dst));
            Assembler.Put
              (This.Line,
               (if Offset > 0 then "add" else "sub"),
               "#" & Ada.Strings.Fixed.Trim
                 (Frame_Offset'Image (Adjusted_Offset), Ada.Strings.Left),
               To_String (This.Dst));
         end;
      end if;
   end Frame_Operand;

   ---------
   -- Get --
   ---------

   function Get return Any_Instance is
   begin
      return Arch : Instance (16);
   end Get;

   ----------------
   -- Get_Branch --
   ----------------

   function Get_Branch
     (Cond    : Tagatha_Condition;
      Negated : Boolean)
      return String
   is
      function Actual_Condition return Tagatha_Condition;

      ----------------------
      -- Actual_Condition --
      ----------------------

      function Actual_Condition return Tagatha_Condition is
      begin
         if Negated then
            case Cond is
               when C_Always =>
                  raise Constraint_Error with
                    "Cannot negate ""always"" condition";
               when C_Equal =>
                  return C_Not_Equal;
               when C_Not_Equal =>
                  return C_Equal;
               when C_Greater =>
                  return C_At_Most;
               when C_Less =>
                  return C_At_Least;
               when C_At_Most =>
                  return C_Greater;
               when C_At_Least =>
                  return C_Less;
            end case;
         else
            return Cond;
         end if;
      end Actual_Condition;

   begin
      case Actual_Condition is
         when C_Always =>
            return "br";
         when C_Equal =>
            return "beq";
         when C_Not_Equal =>
            return "bne";
         when C_Greater =>
            return "bgt";
         when C_Less =>
            return "blt";
         when C_At_Least =>
            return "bge";
         when C_At_Most =>
            return "ble";
      end case;
   end Get_Branch;

   ---------------
   -- Get_Group --
   ---------------

   overriding function Get_Group
     (This : Instance;
      Data : Tagatha_Data_Type;
      Size : Tagatha_Size)
      return Tagatha.Registers.Register_Group'Class
   is
      Result : Register_Group;
   begin
      if Data = Floating_Point_Data then
         Result := (Tagatha.Registers.Register_Group with
                    Floating_Point_Register, 1, 3);
      else
         Result := (Tagatha.Registers.Register_Group with
                    General_Register, 1, 4);
      end if;
      return Result;
   end Get_Group;

   ---------------------
   -- Get_Instruction --
   ---------------------

   function Get_Instruction
     (Op : Tagatha_Operator)
      return String
   is
   begin
      case Op is
         when Op_Nop =>
            return "mov";
         when Op_Add =>
            return "add";
         when Op_Sub =>
            return "sub";
         when Op_Mul | Op_Div | Op_Mod =>
            raise Constraint_Error with
              "no native multiplication/division on the pdp-11";
         when Op_And | Op_Or | Op_Xor | Op_Not =>
            raise Constraint_Error with
              "whoops, and/or/xor/not more complicated on pdp-11";
         when Op_Bit_Test =>
            return "bit";
         when Op_Bit_Clear =>
            return "bic";
         when Op_Bit_Set =>
            return "bis";
         when Op_Equal .. Op_Less_Equal =>
            return "cmp";
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the pdp-11";
         when Op_Compare =>
            return "cmp";
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the pdp-11";
         when Op_Negate =>
            return "neg";
         when Op_Complement =>
            return "not";
         when Op_Test =>
            return "tst";
         when Op_Logical_Shift =>
            raise Constraint_Error with
              "we didn't implement logical shifts yet";
      end case;
   end Get_Instruction;

   -----------
   -- Label --
   -----------

   overriding procedure Label
     (This : in out Instance;
      Name : String)
   is
   begin
      This.Put_Label (Name);
   end Label;

   -----------------
   -- Local_Label --
   -----------------

   overriding procedure Local_Label
     (This  : in out Instance;
      Label : Tagatha.Labels.Label)
   is
   begin
      Dispatch (This).Label (Get_Label_Image (Label));
   end Local_Label;

   --------------------
   -- Long_Operation --
   --------------------

   procedure Long_Operation
     (Left, Right : Tagatha.Operands.Operand_Type;
      Bits        : Natural;
      MSB_First   : Boolean;
      Operation   : not null access
        procedure (Left, Right : String))
   is
      Word_Count : constant Natural := (Bits + 15) / 16;
   begin
      for I in 1 .. Word_Count loop
         declare
            Offset : constant Natural :=
                       (if MSB_First
                        then Word_Count - I
                        else I - 1);
         begin
            Operation
              (Images.Source_Operand_Image.Image (Left, Offset),
               Images.Destination_Operand_Image.Image (Right, Offset));
         end;
      end loop;
   end Long_Operation;

   ----------
   -- Move --
   ----------

   overriding procedure Move
     (This        : in out Instance;
      Source      : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type)
   is
      Src_Bits : constant Natural :=
                   This.Size_Bits (Operands.Size (Source))
        with Unreferenced;
      Dst_Bits : constant Natural :=
                   This.Size_Bits (Operands.Size (Destination));
      Src_Image : constant String :=
                    Images.Source_Operand_Image.Image (Source);
      Dst_Image : constant String :=
                    Images.Destination_Operand_Image.Image (Destination);
      Instruction : constant String :=
                      (if Dst_Bits <= 8
                       then "movb"
                       else "mov");
      Before_Move : Move_Operand :=
                      Move_Operand'
                        (Dst  => Ada.Strings.Unbounded.To_Unbounded_String
                           (Dst_Image),
                         Line => <>);
   begin
      --  Ada.Text_IO.Put_Line
      --    (Operands.Image (Destination)
      --     & " <- "
      --     & Operands.Image (Source));

      Before_Move.Process (Source);

      This.Put (Before_Move.Line);

      if Assembler.Is_Empty (Before_Move.Line) then
         if Dst_Bits <= 16 then
            This.Put (Instruction, Src_Image, Dst_Image);
         else
            declare
               Start    : Natural := 0;
               Src_Part : constant Operands.Operand_Type :=
                            Operands.Set_Size (Tagatha.Size_16, Source);
               Dst_Part : constant Operands.Operand_Type :=
                            Operands.Set_Size (Tagatha.Size_16, Destination);

            begin
               while Start * 16 < Dst_Bits loop
                  This.Put (Instruction,
                            Images.Source_Operand_Image.Image
                              (Src_Part, Start),
                            Images.Destination_Operand_Image.Image
                              (Dst_Part, Start));
                  Start := Start + 1;
               end loop;
            end;
         end if;
      end if;
   end Move;

   -------------
   -- Operate --
   -------------

   overriding procedure Operate
     (This        : in out Instance;
      Operator    : Tagatha_Operator;
      Source_1    : Tagatha.Operands.Operand_Type;
      Source_2    : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type)
   is
      use type Tagatha.Operands.Operand_Type;
      Src_1_Bits  : constant Natural :=
                      This.Size_Bits (Operands.Size (Source_1));
      Src_2_Bits  : constant Natural :=
                      This.Size_Bits (Operands.Size (Source_2))
        with Unreferenced;
      Dst_Bits    : constant Natural :=
                      This.Size_Bits (Operands.Size (Destination));
      Instr_Bits  : constant Natural :=
                      (if Operator = Op_Test
                       or else Operator in Condition_Operator
                       then Src_1_Bits
                       else Dst_Bits);
      Data : constant Tagatha_Data_Type :=
               (if Operator = Op_Test
                or else Operator in Condition_Operator
                then Operands.Data_Type (Source_1)
                else Operands.Data_Type (Destination));
      Byte_Suffix : constant String :=
                      (if Instr_Bits <= 8
                       and then Operator /= Op_Add
                       and then Operator /= Op_Sub
                       then "b" else "");
      Op          : constant String :=
                      Get_Instruction (Operator)
                    & Byte_Suffix;
   begin
      --  Ada.Text_IO.Put_Line
      --    (Operands.Image (Destination)
      --     & " <- "
      --     & Operands.Image (Source_1)
      --     & " "
      --     & Operator'Image
      --     & " "
      --     & Operands.Image (Source_2));
      This.Last_Op := Operator;
      if Operator in Zero_Argument_Operator then
         Dispatch (This).Move (Source_1, Destination);
      elsif Operator in One_Argument_Operator then
         This.Put (Op,
                   Images.Source_Operand_Image.Image (Source_1),
                   Images.Destination_Operand_Image.Image (Destination));
      else
         if Operator in Condition_Operator then
            if Destination = Tagatha.Operands.No_Operand then
               This.Put
                 (Op,
                  Images.Source_Operand_Image.Image (Source_1),
                  Images.Destination_Operand_Image.Image (Source_2));
            else
               This.Put ("clr", "r0");

               declare
                  Br : constant String :=
                         Get_Branch (To_Condition (Operator), True);
                  procedure Compare (Src_1, Src_2 : String);

                  -------------
                  -- Compare --
                  -------------

                  procedure Compare (Src_1, Src_2 : String) is
                  begin
                     This.Put ("cmp", Src_1, Src_2);
                     This.Put (Br, "1");
                     if Br /= "bne" then
                        This.Put ("bne", "2");
                     end if;
                  end Compare;

               begin
                  if Data = Floating_Point_Data then
                     This.Put ("ldf", "ac0",
                               Images.Source_Operand_Image.Image
                                 (Source_1));
                     This.Put ("ldf", "ac1",
                               Images.Source_Operand_Image.Image
                                 (Source_2));
                     This.Put ("cmpf", "ac0", "ac1");
                     This.Put (Br, "1");
                  else
                     Long_Operation (Source_1, Source_2, Instr_Bits, True,
                                     Compare'Access);
                     This.Label ("2");
                  end if;
               end;

               This.Put ("inc", "r0");
               This.Label ("1");
               This.Put ((if Dst_Bits <= 8 then "movb" else "mov"),
                         "r0",
                         Images.Destination_Operand_Image.Image (Destination));
            end if;
         else
            if not Operands.Is_Address (Destination)
              or else Source_1 /= Operands.Dereference (Destination)
            then
               Dispatch (This).Move (Source_1, Destination);
            end if;
            This.Put
              (Op,
               Images.Source_Operand_Image.Image (Source_2),
               Images.Destination_Operand_Image.Image (Destination));
         end if;
      end if;
   end Operate;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (This        : in out Instance;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
   is
   begin
      if Instruction = "mov" and then Arg_1 = "#0" then
         Parent (This).Put ("clr", Arg_2);
      elsif Instruction = "add" and then Arg_1 = "#0" then
         null;
      elsif Instruction = "sub" and then Arg_1 = "#0" then
         null;
      else
         Parent (This).Put (Instruction, Arg_1, Arg_2, Arg_3);
      end if;
   end Put;

   -------------------
   -- Start_Routine --
   -------------------

   overriding procedure Start_Routine
     (This   : in out Instance;
      Name   : String;
      Global : Boolean)
   is
   begin
      This.Label (Name);
      This.Put ("mov", "r5", "-(sp)");
      This.Put ("mov", "sp", "r5");
   end Start_Routine;

end Tagatha.Arch.Pdp11;
