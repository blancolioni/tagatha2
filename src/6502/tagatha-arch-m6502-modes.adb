package body Tagatha.Arch.M6502.Modes is

   subtype Instruction_Name is String (1 .. 3);

   type Mode_Operation is (Start, Command, Increment);

   type Mode_Process is new Operand_Process with
      record
         Operation   : Mode_Operation;
         Instruction : Instruction_Name := "   ";
         Argument    : Argument_Type;
         Byte        : Natural := 0;
         Have_FP     : Boolean := False;
         Inc_FP      : Boolean := False;
      end record;

   overriding procedure Frame_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Offset  : Frame_Offset);

   overriding procedure Integer_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Integer);

   type Jsr_Process is new Operand_Process with null record;

   overriding procedure External_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Name    : String);

   -------------
   -- Command --
   -------------

   procedure Command
     (This        : in out Any_Instance;
      Instruction : String;
      Argument    : Argument_Type)
   is
      Process : Mode_Process := Mode_Process'
        (Operand_Process with
         Operation => Command,
         Instruction => Instruction,
         Argument    => Argument,
         Have_FP     => This.Have_FP,
         Byte        => This.Arguments (Argument).Byte,
         Inc_FP      => False);
   begin
      This.Put (Process, This.Arguments (Argument).Operand.Element);
      This.Have_FP := Process.Have_FP;
   end Command;

   ----------------------
   -- External_Operand --
   ----------------------

   overriding procedure External_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Name    : String)
   is
   begin
      Process.Put ("jsr", Name);
   end External_Operand;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Offset  : Frame_Offset)
   is
   begin
      case Process.Operation is
         when Start =>
            if not Process.Have_FP then
               Process.Put ("ldy", "fp");
               Process.Have_FP := True;
            end if;
         when Command =>
            declare
               Adjusted_Offset : constant Frame_Offset :=
                                   (if Offset < 0
                                    then Offset
                                    else Offset + 3)
                                   + Frame_Offset (Process.Byte);
               Base_Address    : constant String :=
                                   Hex (Stack_Base + Max_Frame_Size
                                        + Integer (Adjusted_Offset));
            begin
               Process.Put
                 (Instruction => Process.Instruction,
                  Arg_1       => Base_Address & ",y");
            end;

         when Increment =>
            null;
      end case;
   end Frame_Operand;

   ---------------
   -- Increment --
   ---------------

   procedure Increment
     (This     : in out Any_Instance;
      Argument : Argument_Type)
   is
      Process : Mode_Process := Mode_Process'
        (Operand_Process with
         Operation   => Increment, others => <>);
   begin
      This.Put (Process, This.Arguments (Argument).Operand.Element);
      This.Arguments (Argument).Byte := @ + 1;
   end Increment;

   ---------------------
   -- Integer_Operand --
   ---------------------

   overriding procedure Integer_Operand
     (Process : in out Mode_Process;
      Context : Operands.Operand_Context;
      Value   : Tagatha_Integer)
   is
   begin
      case Process.Operation is
         when Start =>
            null;
         when Command =>
            Process.Put (Process.Instruction,
                         Imm
                           (Natural
                              (Value / 2 ** (Process.Byte * 8)) mod 256));
         when Increment =>
            null;
      end case;
   end Integer_Operand;

   --------------
   -- Jsr_Mode --
   --------------

   function Jsr_Mode return Operand_Process'Class is
   begin
      return Result : Jsr_Process;
   end Jsr_Mode;

   -----------
   -- Start --
   -----------

   procedure Start
     (This     : in out Any_Instance;
      Argument : Argument_Type;
      Operand  : Operands.Operand_Type)
   is
      Process : Mode_Process := Mode_Process'
        (Operand_Process with
         Operation => Start,
         Have_FP   => This.Have_FP,
         others    => <>);
   begin
      This.Put (Process, Operand);
      This.Arguments (Argument) := Argument_Record'
        (Active  => True,
         Operand => Operand_Holders.To_Holder (Operand),
         Byte    => 0);
      This.Have_FP := Process.Have_FP;
   end Start;

end Tagatha.Arch.M6502.Modes;
