private package Tagatha.Arch.M6502.Modes is

   procedure Start
     (This     : in out Any_Instance;
      Argument    : Argument_Type;
      Operand     : Operands.Operand_Type);

   procedure Command
     (This        : in out Any_Instance;
      Instruction : String;
      Argument    : Argument_Type);

   procedure Increment
     (This     : in out Any_Instance;
      Argument : Argument_Type);

   function Jsr_Mode
     return Operand_Process'Class;

end Tagatha.Arch.M6502.Modes;
