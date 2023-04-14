with Tagatha.Arch.M6502.Modes;

package body Tagatha.Arch.M6502 is

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

   procedure Long_Operation
     (This        : in out Any_Instance;
      Left, Right : Tagatha.Operands.Operand_Type;
      Bits        : Natural;
      MSB_First   : Boolean;
      Operation   : not null access procedure)
   is null;

   type Move_Register is (Move_Src_1, Move_Src_2, Move_Dst);

   type Move_Operand is
     new Tagatha.Operands.Operand_Process_Interface with
      record
         Register : Move_Register;
         Line     : Tagatha.Assembler.Assembler_Line;
      end record;

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset);

   type Conditional_Branch_Record is
      record
         Primary_Test   : String (1 .. 3);
         Secondary_Test : String (1 .. 3);
         Default_Value  : Boolean;
      end record;

   Conditional_Branch_Matrix : constant array (Condition_Operator)
     of Conditional_Branch_Record :=
       (Op_Equal         => ("bne", "   ", True),
        Op_Not_Equal     => ("bne", "   ", False),
        Op_Greater_Equal => ("bmi", "   ", True),
        Op_Less          => ("bpl", "   ", False),
        Op_Greater       => ("bmi", "beq", True),
        Op_Less_Equal    => ("bmi", "beq", False));

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
      Mode : Operand_Process'Class := Modes.Jsr_Mode;
   begin
      This.Put (Mode, Destination);
      This.Change_Stack (-Argument_Count);
   end Call;

   ------------------
   -- Change_Stack --
   ------------------

   overriding procedure Change_Stack
     (This   : in out Instance;
      Change : Integer)
   is
      Pop      : constant Boolean := Change < 0;
      PxA      : constant String :=
                   (if Change < 0 then "pla" else "pha");
      Change_X : constant String :=
                   (if Change < 0 then "inx" else "dex");
      Size     : constant Natural := abs Change;
   begin

      --   stack   memory / cycles
      --  change  pxa   inx   adc
      --      1   1/3   3/6   7/12
      --      2   2/6   4/8   7/12
      --      3   3/9   5/10  7/12
      --      4   4/12  6/12  7/12
      --      5   5/15  7/14  7/12
      --      6   6/18  8/16  7/12
      --      7   7/21  9/18
      --      8   8/24
      --  Conclusion: prefer pha/pla when change < 5, otherwise adc/sbc
      --  If memory very tight (you probably shouldn't be using Tagatha)
      --  you could use pxa for size 5 to trade 3 clock cycles for 2
      --  saved bytes; or even for size 6 to trade 6 clock cycles for 1
      --  byte saved.
      if Size = 0 then
         null;
      elsif Size < 5 then
         for I in 1 .. Size loop
            This.Put (PxA);  --  size * 3 cycles
         end loop;
      elsif False then
         --  4 + size * 2 cycles, 2 + size bytes
         This.Put ("tsx");
         for I in 1 .. Size loop
            This.Put (Change_X);
         end loop;
         This.Put ("txs");
      else
         --  7 bytes, 12 cycles
         This.Put ("tsx");
         This.Put ("txa");
         if Pop then
            This.Put ("clc");
            This.Put ("adc", Imm (Size));
         else
            This.Put ("sec");
            This.Put ("sbc", Imm (Size));
         end if;
         This.Put ("tax");
         This.Put ("txs");
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
      This.Put ("pla");
      This.Put ("sta", "fp");
      This.Put ("rts");
   end End_Routine;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset)
   is null;
   ---------
   -- Get --
   ---------

   function Get return Tagatha.Arch.Any_Instance is
   begin
      return Arch : Instance (8);
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
            return "bpl";
         when C_Less =>
            return "bmi";
         when C_At_Least =>
            return "bpl";
         when C_At_Most =>
            return "bmi";
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

   ---------
   -- Hex --
   ---------

   function Hex (X : Natural) return String is
      Hex_Digit : constant String := "0123456789ABCDEF";
   begin
      if X < 256 then
         return ('$', Hex_Digit (X / 16 + 1), Hex_Digit (X mod 16 + 1));
      else
         return ('$', Hex_Digit (X / 4096 + 1), Hex_Digit (X / 256 mod 16 + 1),
                 Hex_Digit (X / 16 mod 16 + 1), Hex_Digit (X mod 16 + 1));
      end if;
   end Hex;

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

   --  procedure Long_Operation
   --    (This        : in out Any_Instance;
   --     Left, Right : Tagatha.Operands.Operand_Type;
   --     Bits        : Natural;
   --     MSB_First   : Boolean;
   --     Operation   : not null access procedure)
   --  is
   --     Byte_Count : constant Natural := (Bits + 7) / 8;
   --  begin
   --     for I in 1 .. Byte_Count loop
   --        declare
   --           Offset : constant Natural :=
   --                      (if MSB_First
   --                       then Byte_Count - I
   --                       else I - 1);
   --        begin
   --           This.Put (Modes.Source_Mode (Offset), Left);
   --           Operation.all;
   --           This.Put (Modes.Destination_Mode (Offset), Right);
   --           if I < Byte_Count then
   --              This.Put (Modes.Update_Source_Mode (Offset), Left);
   --              This.Put (Modes.Update_Destination_Mode (Offset), Right);
   --           end if;
   --        end;
   --     end loop;
   --  end Long_Operation;

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
      Byte_Count : constant Natural := (Dst_Bits + 7) / 8;
   begin
      Modes.Start (This, M6502.Source_1, Source);
      Modes.Start (This, M6502.Destination, Destination);

      for I in 1 .. Byte_Count loop
         Modes.Command (This, "lda", M6502.Source_1);
         Modes.Command (This, "sta", M6502.Destination);
         if I < Byte_Count then
            Modes.Increment (This, M6502.Source_1);
            Modes.Increment (This, M6502.Destination);
         end if;
      end loop;
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
      Byte_Count : constant Natural := (Instr_Bits + 7) / 8;
      Data : constant Tagatha_Data_Type :=
               (if Operator = Op_Test
                or else Operator in Condition_Operator
                then Operands.Data_Type (Source_1)
                else Operands.Data_Type (Destination))
        with Unreferenced;

      procedure Operate;

      -------------
      -- Operate --
      -------------

      procedure Operate is
      begin
         case One_Argument_Operator (Operator) is
            when Op_Negate =>
               null;
            when Op_Not =>
               null;
            when Op_Complement =>
               null;
            when Op_Test =>
               null;
         end case;
      end Operate;

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
         Long_Operation
           (This      => This,
            Left      => Source_1,
            Right     => Destination,
            Bits      => Instr_Bits,
            MSB_First => Operator in Condition_Operator,
            Operation => Operate'Access);
      else
         if Operator in Condition_Operator then
            if Destination = Tagatha.Operands.No_Operand then
               Long_Operation
                 (This      => This,
                  Left      => Source_1,
                  Right     => Source_2,
                  Bits      => Instr_Bits,
                  MSB_First => True,
                  Operation => Operate'Access);
            else

               declare
                  Condition : constant Condition_Operator :=
                                Condition_Operator (Operator);
                  Row       : constant Conditional_Branch_Record :=
                                Conditional_Branch_Matrix (Condition);
               begin
                  Modes.Start (This, M6502.Source_1, Source_1);
                  Modes.Start (This, M6502.Source_2, Source_2);

                  This.Put ("lda", (if Row.Default_Value then "#1" else "#0"));
                  This.Put ("sta", "r0");

                  for I in 1 .. Byte_Count loop
                     Modes.Command (This, "lda", M6502.Source_1);
                     Modes.Command (This, "cmp", M6502.Source_2);

                     This.Put (Row.Primary_Test, "1");
                     if Row.Secondary_Test /= "   " then
                        This.Put (Row.Secondary_Test, "1");
                     end if;

                     if I < Byte_Count then
                        Modes.Increment (This, M6502.Source_1);
                        Modes.Increment (This, M6502.Source_2);
                     end if;
                  end loop;

                  This.Put ((if Row.Default_Value then "dec" else "inc"),
                            "r0");
               end;

               This.Label ("1");

               Modes.Start (This, M6502.Destination, Destination);
               This.Put ("lda", "r0");
               Modes.Command (This, "sta", M6502.Destination);

            end if;
         else
            declare
               Op : constant String := Get_Instruction (Operator);
            begin
               Modes.Start (This, M6502.Source_1, Source_1);
               Modes.Start (This, M6502.Source_2, Source_2);
               Modes.Start (This, M6502.Destination, Destination);

               for I in 1 .. Byte_Count loop
                  Modes.Command (This, "lda", M6502.Source_1);
                  Modes.Command (This, Op, M6502.Source_2);
                  Modes.Command (This, "sta", M6502.Destination);
                  if I < Byte_Count then
                     Modes.Increment (This, M6502.Source_1);
                     Modes.Increment (This, M6502.Source_2);
                     Modes.Increment (This, M6502.Destination);
                  end if;
               end loop;

            end;
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
      Args   : Natural;
      Global : Boolean)
   is
   begin
      This.Label (Name);
      This.Put ("lda", "fp");
      This.Put ("pha");
      This.Put ("tsx");
      This.Put ("tsa");
      This.Put ("sec");
      This.Put ("sbc", Imm (Max_Frame_Size));
      This.Put ("sta", "fp");
      This.Have_FP := False;
   end Start_Routine;

end Tagatha.Arch.M6502;
