with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
--  with Ada.Text_IO;

with Tagatha.Arch.Pdp11_Generator.Inspect;

package body Tagatha.Arch.Pdp11_Generator is

   package Label_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Label_Maps is
     new WL.String_Maps (Positive);

   Label_Vector : Label_Vectors.Vector;
   Label_Map    : Label_Maps.Map;

   Next_Temporary_Index : Natural := 0;

   function Next_Temporary_Label return String;

   type Command_Operand_Process is
     new Tagatha.Operands.Operand_Process_Interface with
      record
         Result     : Command_Operand;
         Source     : Boolean;
         Word_Index : Natural;
         Multiword  : Boolean := False;
      end record;

   overriding procedure Stack_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context);

   overriding procedure External_Operand
     (Process    : in out Command_Operand_Process;
      Context    : Operands.Operand_Context;
      Name       : String;
      Absolute   : Boolean);

   overriding procedure Frame_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Offset  : Frame_Offset);

   overriding procedure Integer_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Integer);

   overriding procedure Floating_Point_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Floating_Point);

   overriding procedure Register_Operand
     (Process    : in out Command_Operand_Process;
      Context    : Tagatha.Operands.Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register);

   function Get_Instruction
     (Op : Tagatha_Operator)
     return Pdp11.ISA.Instruction_Type;

   function Get_Branch
     (Cond    : Tagatha_Condition;
      Negated : Boolean)
      return Pdp11.ISA.Branch_Instruction;

   function Get_Label_Image
     (Label : Tagatha.Labels.Label)
      return String
   is ("L" & Tagatha.Labels.Image (Label));

   subtype Parent is Tagatha.Arch.Instance;

   type Instance is new Parent with
      record
         Last_Op  : Tagatha_Operator := Op_Nop;
         Commands : Command_Lists.List;
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

   overriding procedure End_Generation
     (This   : in out Instance);

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

   procedure Append
     (This : in out Instance'Class;
      Cmd  : Command);

   procedure Append
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Instruction_Type;
      Src         : Command_Operand := (others => <>);
      Dst         : Command_Operand := (others => <>);
      Byte        : Boolean         := False);

   procedure Branch_To_Label
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Label       : String);

   procedure Branch
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Destination : Tagatha.Labels.Label);

   procedure Label
     (This  : in out Instance'Class;
      Index : Positive)
     with Unreferenced;

   procedure Put
     (This : in out Instance'Class;
      Cmd  : Command);

   function Source_Operand
     (Operand : Tagatha.Operands.Operand_Type;
      Offset  : Natural := 0)
      return Command_Operand;

   function Destination_Operand
     (Operand : Tagatha.Operands.Operand_Type;
      Offset  : Natural := 0)
      return Command_Operand;

   function Label_Reference
     (Name : String)
      return Positive;

   procedure Long_Operation
     (Left, Right : Tagatha.Operands.Operand_Type;
      Bits        : Natural;
      MSB_First   : Boolean;
      Operation   : not null access
        procedure (Left, Right : Command_Operand));

   type Move_Operand is
     new Tagatha.Operands.Operand_Process_Interface with
      record
         Dst      : Operand_Holders.Holder;
         Commands : Command_Lists.List;
      end record;

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset);

   FP       : constant Command_Operand :=
                Command_Operand'
                  (Operand => (Register => 5, others => <>),
                   others  => <>);

   SP       : constant Command_Operand :=
                Command_Operand'
                  (Operand => (Register => 6, others => <>),
                   others  => <>);

   Minus_SP : constant Command_Operand :=
                Command_Operand'
                  (Operand =>
                     (Register => 6,
                      Mode     => Pdp11.ISA.Autodecrement_Mode,
                      Deferred => False),
                   others  => <>);

   SP_Plus : constant Command_Operand :=
                Command_Operand'
                  (Operand =>
                     (Register => 6,
                      Mode     => Pdp11.ISA.Autoincrement_Mode,
                      Deferred => False),
                   others  => <>);

   PC       : constant Command_Operand :=
                Command_Operand'
                  (Operand => (Register => 7, others => <>),
                   others  => <>);

   function Immediate
     (Value : Tagatha_Integer)
      return Command_Operand
   is (Command_Operand'
         (Operand =>
            (Register => 7,
             Mode     => Pdp11.ISA.Autoincrement_Mode,
             Deferred => False),
          Value   => Value,
          others  => <>));

   procedure Peephole
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean);

   procedure Register_State
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean);

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Instance'Class;
      Cmd  : Command)
   is
   begin
      This.Commands.Append (Cmd);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Instruction_Type;
      Src         : Command_Operand := (others => <>);
      Dst         : Command_Operand := (others => <>);
      Byte        : Boolean         := False)
   is
      New_Item    : constant Command :=
                      Command'
                        (Has_Instruction => True,
                         Label_List      => <>,
                         Src             => Src,
                         Byte            => Byte,
                         Dst             => Dst,
                         Branch_Label    => 0,
                         Instruction     => Instruction);
   begin
      This.Append (New_Item);
   end Append;

   ------------
   -- Branch --
   ------------

   procedure Branch
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Destination : Tagatha.Labels.Label)
   is
   begin
      This.Append
        (Command'
           (Has_Instruction => True,
            Instruction     => Instruction,
            Branch_Label    =>
              Label_Reference (Get_Label_Image (Destination)),
            others          => <>));
   end Branch;

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
         This.Branch
           (Get_Branch
              (To_Condition (This.Last_Op), Condition = C_Equal),
            Destination);
      else
         This.Branch_To_Label
           (Get_Branch (Condition, False),
            Get_Label_Image (Destination));
      end if;
      This.Last_Op := Op_Nop;
   end Branch;

   ---------------------
   -- Branch_To_Label --
   ---------------------

   procedure Branch_To_Label
     (This        : in out Instance'Class;
      Instruction : Pdp11.ISA.Branch_Instruction;
      Label       : String)
   is
   begin
      This.Append
        (Command'
           (Has_Instruction => True,
            Branch_Label    => Label_Reference (Label),
            Instruction     => Instruction,
            others          => <>));
   end Branch_To_Label;

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (This           : in out Instance;
      Destination    : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural)
   is
   begin
      This.Append (Pdp11.ISA.I_JSR, PC,
                   Source_Operand (Destination));
   end Call;

   ------------------
   -- Change_Stack --
   ------------------

   overriding procedure Change_Stack
     (This   : in out Instance;
      Change : Integer)
   is
      SP : constant Command_Operand :=
             Command_Operand'
               (Operand => (Register => 6, others => <>),
                others  => <>);
   begin
      if Change = 0 then
         null;
      elsif Change = 1 then
         This.Append (Pdp11.ISA.I_TST, Minus_SP);
      elsif Change = -1 then
         This.Append (Pdp11.ISA.I_TST, SP_Plus);
      else
         This.Append
           (Instruction => (if Change > 0
                            then Pdp11.ISA.I_SUB
                            else Pdp11.ISA.I_ADD),
            Src         => Immediate (Tagatha_Integer (abs Change * 2)),
            Dst         => SP);
      end if;
   end Change_Stack;

   -------------------------
   -- Destination_Operand --
   -------------------------

   function Destination_Operand
     (Operand : Tagatha.Operands.Operand_Type;
      Offset  : Natural := 0)
      return Command_Operand
   is
      To_Operand : Command_Operand_Process :=
                     (Source => False, Word_Index => Offset, others => <>);
   begin
      To_Operand.Process (Operand);
      return To_Operand.Result;
   end Destination_Operand;

   --------------------
   -- End_Generation --
   --------------------

   overriding procedure End_Generation
     (This   : in out Instance)
   is
      Changed : Boolean := True;
   begin
      if True then
         while Changed loop
            Peephole (This.Commands, Changed);
            if not Changed then
               Register_State (This.Commands, Changed);
            end if;
         end loop;
      end if;

      for Command of This.Commands loop
--         This.Put (Command'Image);
         This.Put (Command);
      end loop;

      for I in 1 .. Label_Vector.Last_Index loop
         This.Put_Label (";" & I'Img & " " & Label_Vector.Element (I));
      end loop;

   end End_Generation;

   -----------------
   -- End_Routine --
   -----------------

   overriding procedure End_Routine
     (This   : in out Instance;
      Name   : String)
   is
   begin
      This.Append (Pdp11.ISA.I_MOV,
                   SP_Plus,
                   Command_Operand'
                     (Operand => (Register => 5, others => <>),
                      others  => <>));
      This.Append
        (Pdp11.ISA.I_RTS, PC);
   end End_Routine;

   ----------------------
   -- External_Operand --
   ----------------------

   overriding procedure External_Operand
     (Process    : in out Command_Operand_Process;
      Context    : Operands.Operand_Context;
      Name       : String;
      Absolute   : Boolean)
   is
      use Pdp11.ISA;
   begin
      if Absolute then
         Process.Result := Command_Operand'
           (Operand => Operand_Type'
              (Mode     => Autoincrement_Mode,
               Deferred => True,
               Register => 7),
            Label   => Label_Reference (Name),
            others  => <>);
      else
         Process.Result := Command_Operand'
           (Operand => Operand_Type'
              (Mode     => Index_Mode,
               Deferred => False,
               Register => 7),
            Label   => Label_Reference (Name),
            others  => <>);
      end if;
   end External_Operand;

   ----------------------------
   -- Floating_Point_Operand --
   ----------------------------

   overriding procedure Floating_Point_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Floating_Point)
   is
      function To_Integer is
        new Ada.Unchecked_Conversion (Tagatha_Floating_Point,
                                      Floating_Point_Integer);
      Int_Value : constant Floating_Point_Integer :=
                    To_Integer (Value);
      Slice     : constant Floating_Point_Integer :=
                    (Int_Value / 2 ** (16 * Process.Word_Index)) mod 2 ** 16;
   begin
      if Size_Bits (Context.Size) <= 16 then
         Process.Result := Immediate (Tagatha_Integer (Slice));
      else
         Process.Result := Immediate (Tagatha_Integer (Int_Value));
      end if;
   end Floating_Point_Operand;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Offset  : Frame_Offset)
   is
      use Pdp11.ISA;
   begin
      Process.Result := Command_Operand'
        (Operand => Operand_Type'
           (Mode     => Index_Mode,
            Deferred => False,
            Register => 5),
         Offset  => Offset * 2
         + Frame_Offset (Process.Word_Index * 2)
         + (if Offset > 0 then 2 else 0),
         others  => <>);
   end Frame_Operand;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (This    : in out Move_Operand;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset)
   is
   begin
      if Context.Is_Address then
         declare
            Base_Offset     : constant Frame_Offset :=
                                2 * (Offset + (if Offset > 0 then 1 else 0));
            Adjusted_Offset : constant Frame_Offset :=
                                abs
                                  (Base_Offset
                                   + Frame_Offset (Context.Word_Index) * 2);
            Dst             : constant Command_Operand :=
                                Destination_Operand (This.Dst.Element);
            Imm             : constant Command_Operand :=
                                Immediate (Tagatha_Integer (Adjusted_Offset));
         begin
            This.Commands.Append
              (Command'
                 (Has_Instruction => True,
                  Label_List      => <>,
                  Src             => FP,
                  Byte            => False,
                  Dst             => Dst,
                  Branch_Label    => 0,
                  Instruction     => Pdp11.ISA.I_MOV));
            if Adjusted_Offset /= 0 then
               This.Commands.Append
                 (Command'
                    (Has_Instruction => True,
                     Label_List      => <>,
                     Src             => Imm,
                     Byte            => False,
                     Dst             => Dst,
                     Branch_Label    => 0,
                     Instruction     => (if Offset > 0
                                         then Pdp11.ISA.I_ADD
                                         else Pdp11.ISA.I_SUB)));
            end if;
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
      return Pdp11.ISA.Branch_Instruction
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

      use all type Pdp11.ISA.Instruction_Type;

   begin
      case Actual_Condition is
         when C_Always =>
            return I_BR;
         when C_Equal =>
            return I_BEQ;
         when C_Not_Equal =>
            return I_BNE;
         when C_Greater =>
            return I_BGT;
         when C_Less =>
            return I_BLT;
         when C_At_Least =>
            return I_BGE;
         when C_At_Most =>
            return I_BLE;
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
      return Pdp11.ISA.Instruction_Type
   is
      use all type Pdp11.ISA.Instruction_Type;
   begin
      case Op is
         when Op_Nop =>
            return I_MOV;
         when Op_Add =>
            return I_ADD;
         when Op_Sub =>
            return I_SUB;
         when Op_Mul | Op_Div | Op_Mod =>
            raise Constraint_Error with
              "no native multiplication/division on the pdp-11";
         when Op_And | Op_Or | Op_Xor | Op_Not =>
            raise Constraint_Error with
              "whoops, and/or/xor/not more complicated on pdp-11";
         when Op_Bit_Test =>
            return I_BIT;
         when Op_Bit_Clear =>
            return I_BIC;
         when Op_Bit_Set =>
            return I_BIS;
         when Op_Equal .. Op_Less_Equal =>
            return I_CMP;
         when Op_Bit_Slice =>
            raise Constraint_Error with
              "no native slicing on the pdp-11";
         when Op_Compare =>
            return I_CMP;
         when Op_Change_Size =>
            raise Constraint_Error with
              "no native size changing on the pdp-11";
         when Op_Negate =>
            return I_NEG;
         when Op_Complement =>
            return I_COM;
         when Op_Test =>
            return I_TST;
         when Op_Logical_Shift =>
            raise Constraint_Error with
              "we didn't implement logical shifts yet";
      end case;
   end Get_Instruction;

   ---------------------
   -- Integer_Operand --
   ---------------------

   overriding procedure Integer_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Integer)
   is
   begin
      Process.Result := Immediate (Value);
   end Integer_Operand;

   -----------
   -- Label --
   -----------

   overriding procedure Label
     (This : in out Instance;
      Name : String)
   is
      List : Label_Lists.List;
   begin
      List.Append (Label_Reference (Name));
      This.Append
        (Command'
           (Label_List => List,
            Has_Instruction => False,
            others     => <>));
   end Label;

   -----------
   -- Label --
   -----------

   procedure Label
     (This  : in out Instance'Class;
      Index : Positive)
   is
      List : Label_Lists.List;
      Image : constant String := Index'Image;
   begin
      List.Append (Label_Reference (Image (2 .. Image'Last)));
      This.Append
        (Command'
           (Label_List => List,
            Has_Instruction => False,
            others     => <>));
   end Label;

   ---------------------
   -- Label_Reference --
   ---------------------

   function Label_Reference
     (Name : String)
      return Positive
   is
   begin
      if not Label_Map.Contains (Name) then
         Label_Vector.Append (Name);
         Label_Map.Insert (Name, Label_Vector.Last_Index);
      end if;
      return Label_Map (Name);
   end Label_Reference;

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
        procedure (Left, Right : Command_Operand))
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
              (Source_Operand (Left, Offset),
               Destination_Operand (Right, Offset));
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
--      Src_Image : constant String :=
--                    Images.Source_Operand_Image.Image (Source);
      Before_Move : Move_Operand :=
                      Move_Operand'
                        (Dst      => Operand_Holders.To_Holder (Destination),
                         Commands => <>);
   begin
      --  Ada.Text_IO.Put_Line
      --    (Operands.Image (Destination)
      --     & " <- "
      --     & Operands.Image (Source));

      Before_Move.Process (Source);

      for Command of Before_Move.Commands loop
         This.Append (Command);
      end loop;

      if Before_Move.Commands.Is_Empty then
         if Dst_Bits <= 16 then
            This.Append (Pdp11.ISA.I_MOV,
                         Source_Operand (Source),
                         Destination_Operand (Destination),
                         Dst_Bits <= 8);
         else
            declare
               Start    : Natural := 0;
               Src_Part : constant Operands.Operand_Type :=
                            Operands.Set_Size (Tagatha.Size_16, Source);
               Dst_Part : constant Operands.Operand_Type :=
                            Operands.Set_Size (Tagatha.Size_16, Destination);

            begin
               while Start * 16 < Dst_Bits loop
                  This.Append (Pdp11.ISA.I_MOV,
                               Source_Operand (Src_Part, Start),
                               Destination_Operand (Dst_Part, Start));
                  Start := Start + 1;
               end loop;
            end;
         end if;
      end if;
   end Move;

   --------------------------
   -- Next_Temporary_Label --
   --------------------------

   function Next_Temporary_Label return String is
   begin
      Next_Temporary_Index := Next_Temporary_Index + 1;
      return Label : String := Next_Temporary_Index'Image do
         Label (Label'First) := '_';
      end return;
   end Next_Temporary_Label;

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
      use all type Pdp11.ISA.Instruction_Type;
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
      Byte : constant Boolean :=
               Instr_Bits <= 8
                   and then Operator /= Op_Add
                       and then Operator /= Op_Sub;
      Instruction : constant Pdp11.ISA.Instruction_Type :=
                      Get_Instruction (Operator);

      R0  : constant Command_Operand :=
              (Operand => (Register => 0, others => <>),
               others  => <>);

      AC0 : constant Command_Operand :=
              (Operand => (Register => 0, others => <>),
               others  => <>);
      AC1 : constant Command_Operand :=
              (Operand => (Register => 1, others => <>),
               others  => <>);
      L1  : constant String := Next_Temporary_Label;
      L2  : constant String := Next_Temporary_Label;

   begin
--      Ada.Text_IO.Put_Line
--        (Operands.Image (Destination)
--         & " <- "
--         & Operands.Image (Source_1)
--         & " "
--         & Operator'Image
--         & " "
--         & Operands.Image (Source_2));
      This.Last_Op := Operator;
      if Operator in Zero_Argument_Operator then
         Dispatch (This).Move (Source_1, Destination);
      elsif Operator in One_Argument_Operator then
         This.Append (Instruction,
                      Source_Operand (Source_1),
                      Destination_Operand (Destination),
                      Byte);
      else
         if Operator in Condition_Operator then
            if Destination = Tagatha.Operands.No_Operand then
               if Data = Floating_Point_Data then
                  This.Append (I_LDF, AC0, Source_Operand (Source_1));
                  This.Append (I_LDF, AC1, Source_Operand (Source_2));
                  This.Append (I_CMPF, AC0, AC1);
               else
                  This.Append (Instruction,
                               Source_Operand (Source_1),
                               Source_Operand (Source_2),
                               Byte);
               end if;
            else
               This.Append (Pdp11.ISA.I_CLR);

               declare
                  Br : constant Pdp11.ISA.Branch_Instruction :=
                         Get_Branch (To_Condition (Operator), True);
                  procedure Compare (Src_1, Src_2 : Command_Operand);

                  -------------
                  -- Compare --
                  -------------

                  procedure Compare (Src_1, Src_2 : Command_Operand) is
                  begin
                     This.Append (Pdp11.ISA.I_CMP, Src_1, Src_2);
                     This.Branch_To_Label (Br, L1);
                     if Br /= I_BNE then
                        This.Branch_To_Label (I_BNE, L2);
                     end if;
                  end Compare;

               begin
                  if Data = Floating_Point_Data then
                     This.Append
                       (I_LDF, AC0, Source_Operand (Source_1));
                     This.Append
                       (I_LDF, AC1, Source_Operand (Source_2));
                     This.Append
                       (I_CMPF, AC0, AC1);
                     This.Branch_To_Label (Br, L1);
                  else
                     Long_Operation (Source_1, Source_2, Instr_Bits, True,
                                     Compare'Access);
                     This.Label (L2);
                  end if;
               end;

               This.Append (I_INC, R0);
               This.Label (L1);
               This.Append
                 (Instruction => I_MOV,
                  Src         => R0,
                  Dst         => Destination_Operand (Destination),
                  Byte        => Dst_Bits <= 8);
            end if;
         else
            if not Operands.Is_Address (Destination)
              or else Source_1 /= Operands.Dereference (Destination)
            then
               Dispatch (This).Move (Source_1, Destination);
            end if;

            This.Append (Instruction, Source_Operand (Source_2),
                         Destination_Operand (Destination));
         end if;
      end if;
   end Operate;

   --------------
   -- Peephole --
   --------------

   procedure Peephole
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean)
   is
      Previous : Command := (others => <>);
      Position : Command_Lists.Cursor := Commands.First;
      State    : Inspect.Machine_State;
   begin
      Changed := False;
      while Command_Lists.Has_Element (Position) loop
         declare
            use Pdp11.ISA;
            Cmd : constant Command := Command_Lists.Element (Position);
            New_Command : Command;
            Can_Update  : constant Boolean :=
                            Cmd.Label_List.Is_Empty
                                and then Previous.Instruction not in
                                  Branch_Instruction | I_JSR | I_JMP | I_RTS;
            Updated     : Boolean := False;
         begin
            if Can_Update then
               Inspect.Check_Sequence (Previous, Cmd, New_Command, Updated);
            end if;

            --  if not Updated then
            --     Inspect.Save (State, Commands, Position, Updated);
            --  end if;

            if Updated then
               Changed := True;
               declare
                  Deleted : Command_Lists.Cursor := Position;
               begin
                  Command_Lists.Previous (Position);
                  Commands.Delete (Deleted);
                  Commands (Position) := New_Command;
               end;
               Previous := New_Command;
            else
               Previous := Cmd;
            end if;
         end;
         Command_Lists.Next (Position);
      end loop;

      if Changed then
         return;
      end if;

      Position := Commands.First;

      while Command_Lists.Has_Element (Position) loop
         declare
            Updated     : Boolean := False;
         begin
            Inspect.Save (State, Commands, Position, Updated);
            Changed := Changed or else Updated;
         end;
         Command_Lists.Next (Position);
      end loop;

   end Peephole;

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

   ---------
   -- Put --
   ---------

   procedure Put
     (This : in out Instance'Class;
      Cmd  : Command)
   is
   begin
      for Label of Cmd.Label_List loop
         This.Put_Label (Label_Vector.Element (Label));
      end loop;
      if Cmd.Has_Instruction then
         declare
            S : constant String := To_Assembly (Cmd);
         begin
            This.Put (S);
         end;
      end if;
   end Put;

   ----------------------
   -- Register_Operand --
   ----------------------

   overriding procedure Register_Operand
     (Process    : in out Command_Operand_Process;
      Context    : Tagatha.Operands.Operand_Context;
      Group      : Tagatha.Registers.Register_Group'Class;
      Register   : Tagatha.Registers.Register)
   is
      use Pdp11.ISA;
      use type Tagatha.Registers.Register;
      G : Register_Group'Class renames Register_Group'Class (Group);
      R : constant Register_Index := Register_Index (G.First + Register);
      Operand : Operand_Type;
   begin
      if not Context.Is_Initialized
        or else not Context.Is_Address
      then
         Operand := Operand_Type'
           (Register => R, others => <>);
      elsif Process.Multiword then
         Operand := Operand_Type'
           (Register => R, Mode => Autoincrement_Mode, Deferred => False);
      else
         Operand := Operand_Type'
           (Register => R, Mode => Register_Mode, Deferred => True);
      end if;
      Process.Result := Command_Operand'
        (Operand => Operand, others => <>);

   end Register_Operand;

   --------------------
   -- Register_State --
   --------------------

   procedure Register_State
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean)
   is
      use Pdp11.ISA;
      type Current_State is
         record
            Known : Boolean := False;
            Src   : Register_Index := 0;
         end record;

      State : array (Register_Index) of Current_State;
      New_List : Command_Lists.List;
   begin
      Changed := False;
      for Cmd of Commands loop
         if not Cmd.Label_List.Is_Empty then
            State := (others => <>);
         end if;

         declare
            Keep : Boolean := True;
            Src  : constant Operand_Type := Cmd.Src.Operand;
            Dst  : constant Operand_Type := Cmd.Dst.Operand;
         begin
            if Cmd.Has_Instruction
              and then Dst.Mode = Register_Mode
              and then not Dst.Deferred
            then
               if Cmd.Instruction = I_MOV
                 and then Src.Mode = Register_Mode
                 and then not Src.Deferred
               then
                  if State (Dst.Register).Known
                    and then State (Dst.Register).Src
                    = Src.Register
                  then
                     Changed := True;
                     Keep := False;
                  else
                     State (Dst.Register) := (True, Src.Register);
                  end if;
               else
                  State (Dst.Register).Known := False;
               end if;
            end if;

            if Keep then
               New_List.Append (Cmd);
            end if;
         end;
      end loop;

      if Changed then
         Commands := New_List;
      end if;

   end Register_State;

   --------------------
   -- Source_Operand --
   --------------------

   function Source_Operand
     (Operand : Tagatha.Operands.Operand_Type;
      Offset  : Natural := 0)
      return Command_Operand
   is
      To_Operand : Command_Operand_Process :=
                     (Source => True, Word_Index => Offset, others => <>);
   begin
      To_Operand.Process (Operand);
      return To_Operand.Result;
   end Source_Operand;

   -------------------
   -- Stack_Operand --
   -------------------

   overriding procedure Stack_Operand
     (Process : in out Command_Operand_Process;
      Context : Operands.Operand_Context)
   is
   begin
      Process.Result :=
        (if Process.Source then SP_Plus else Minus_SP);
   end Stack_Operand;

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
      This.Append (Pdp11.ISA.I_MOV, FP, Minus_SP);
      This.Append (Pdp11.ISA.I_MOV, SP, FP);
   end Start_Routine;

   -----------------
   -- To_Assembly --
   -----------------

   function To_Assembly (This : Command) return String is
      use Pdp11.ISA;

      Is_Floating_Point : constant Boolean :=
                            This.Instruction in Floating_Point_Instruction;

      function Mnemonic return String;

      function Operand_String
        (Op    : Command_Operand)
         return String;

      --------------
      -- Mnemonic --
      --------------

      function Mnemonic return String is
         Img : constant String :=
                 Ada.Characters.Handling.To_Lower (This.Instruction'Image);
      begin
         return Img (Img'First + 2 .. Img'Last);
      end Mnemonic;

      --------------------
      -- Operand_String --
      --------------------

      function Operand_String
        (Op    : Command_Operand)
         return String
      is
         Operand  : constant Operand_Type := Op.Operand;
         Register : constant Register_Index := Operand.Register;
         Mode     : constant Mode_Type := Operand.Mode;
         Deferred : constant Boolean := Operand.Deferred;
         Reg_Index : constant Character :=
                       Character'Val (Natural (Register) + 48);

         function Op_Img (R : String) return String;

         ------------
         -- Op_Img --
         ------------

         function Op_Img (R : String) return String is

         begin
            case Operand.Mode is
               when Register_Mode =>
                  return (if Deferred then "(" & R & ")" else R);
               when Autoincrement_Mode =>
                  return (if Deferred then "@" else "")
                    & "(" & R & ")+";
               when Autodecrement_Mode =>
                  return "-" & (if Deferred then "@" else "")
                    & "(" & R & ")";
               when Index_Mode =>
                  if Op.Label /= 0 then
                     return (if Deferred then "@" else "")
                       & Label_Vector (Op.Label)
                       & "(" & R & ")";
                  elsif Op.Offset /= 0 then
                     return (if Deferred then "@" else "")
                       & Op.Offset'Image
                       & "(" & R & ")";
                  else
                     return (if Deferred then "@" else "")
                       & "(" & R & ")";
                  end if;
            end case;
         end Op_Img;

      begin
         case Operand.Register is
            when 0 .. 5 =>
               if Is_Floating_Point
                 and then Mode = Register_Mode
                 and then not Deferred
               then
                  return "ac" & Reg_Index;
               else
                  return Op_Img ("r" & Reg_Index);
               end if;
            when 6 =>
               return Op_Img ("sp");
            when 7 =>
               case Mode is
                  when Autoincrement_Mode =>
                     return (if Deferred
                             then "@#" else "#")
                       & (if Op.Label = 0
                          then Ada.Strings.Fixed.Trim
                            (Op.Value'Image, Ada.Strings.Left)
                          else Label_Vector (Op.Label));
                  when Index_Mode =>
                     return (if Deferred
                             then "@" else "")
                       & Label_Vector (Op.Label);

                  when others =>
                     return Op_Img ("pc");
               end case;
         end case;
      end Operand_String;

   begin
      case This.Instruction is
         when Double_Operand_Instruction =>
            return Mnemonic & " "
              & Operand_String (This.Src)
              & ", "
              & Operand_String (This.Dst);
         when Single_Operand_Instruction =>
            return Mnemonic & " "
              & Operand_String (This.Src);
         when Branch_Instruction =>
            declare
               Index : constant Positive := This.Branch_Label;
               Label : constant String := Label_Vector.Element (Index);
            begin
               return Mnemonic & " " & Label;
            end;
         when Floating_Point_F1 =>
            return Mnemonic & " "
              & "ac" & Character'Val (Natural (This.Src.Operand.Register) + 48)
              & ", " & Operand_String (This.Dst);
         when I_JSR =>
            return "jsr pc, " & Operand_String (This.Dst);
         when I_RTS =>
            return "rts pc";
         when others =>
            return This.Instruction'Image;
      end case;
   end To_Assembly;

end Tagatha.Arch.Pdp11_Generator;
