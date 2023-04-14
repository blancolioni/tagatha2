private package Tagatha.Arch.Aqua_Generator.Operands is

   procedure Set_Jump_Destination
     (Cmd         : in out Command;
      Context     : in out Operation_Context;
      Destination : out Natural;
      Op          : Tagatha.Operands.Operand_Type);

   procedure Prepare_X
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      X       : out Command_Operand);

   procedure Store_X
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      X       : Command_Operand);

   procedure Load_Y
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Y       : in out Command_Operand);

   procedure Load_Z
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Z       : out Command_Operand);

   procedure Load_YZ
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Y, Z    : in out Command_Operand);

end Tagatha.Arch.Aqua_Generator.Operands;
