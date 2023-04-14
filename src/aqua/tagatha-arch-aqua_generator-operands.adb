package body Tagatha.Arch.Aqua_Generator.Operands is

   type Processor_Target is
     (Jump_Destination, Prepare_X, Store_X,
      Load_Y, Load_Z);

   type Operand_Processor is
     new Tagatha.Operands.Operand_Process_Interface with
      record
         Target      : Processor_Target;
         Commands    : Command_Lists.List;
         Context     : Operation_Context;
         Op          : Command_Operand;
         Op_2        : Command_Operand;
         Destination : Natural := 0;
      end record;

   overriding procedure Integer_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context;
      Value   : Tagatha_Integer);

   overriding procedure External_Operand
     (Process    : in out Operand_Processor;
      Context    : Tagatha.Operands.Operand_Context;
      Name       : String;
      Absolute   : Boolean);

   overriding procedure Frame_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset);

   overriding procedure Stack_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context);

   ----------------------
   -- External_Operand --
   ----------------------

   overriding procedure External_Operand
     (Process    : in out Operand_Processor;
      Context    : Tagatha.Operands.Operand_Context;
      Name       : String;
      Absolute   : Boolean)
   is
   begin
      case Process.Target is
         when Jump_Destination =>
            Process.Destination := Process.Context.To_Integer (Name);
         when Store_X =>
            null;
         when Prepare_X | Load_Y | Load_Z =>
            null;
      end case;
   end External_Operand;

   -------------------
   -- Frame_Operand --
   -------------------

   overriding procedure Frame_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context;
      Offset  : Frame_Offset)
   is
      R : constant Aqua.Word_8 :=
            (if Offset < 0
             then Aqua.Word_8
               (Process.Context.First_Stack - 1 - Integer (Offset))
             else Aqua.Word_8
               (Process.Context.First_Argument
                + Natural (Offset) - 1));
   begin
      case Process.Target is
         when Jump_Destination =>
            null;
         when Store_X =>
            null;
         when Prepare_X | Load_Y | Load_Z =>
            Process.Op := (R, False);
      end case;
   end Frame_Operand;

   ---------------------
   -- Integer_Operand --
   ---------------------

   overriding procedure Integer_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context;
      Value   : Tagatha_Integer)
   is
   begin
      case Process.Target is
         when Jump_Destination =>
            null;
         when Prepare_X =>
            null;
         when Store_X =>
            null;
         when Load_Y =>
            if Value <= 65535 then
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => True,
                           Immediate => Natural (Value),
                           X         => Process.Op,
                           others    => <>));
            else
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => True,
                           X       => Process.Op,
                           Immediate => Natural (Value) mod 65536,
                           others    => <>));
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => False,
                           X         => Process.Op,
                           Immediate => Natural (Value) / 65536 mod 65536,
                           others    => <>));
            end if;
         when Load_Z =>
            if Value <= 255 then
               Process.Op := (Aqua.Word_8 (Value), True);
            elsif Value <= 65535 then
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => True,
                           Immediate => Natural (Value), others => <>));
            else
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => True,
                           Immediate => Natural (Value) mod 65536,
                           others    => <>));
               Process.Commands.Append
                 (Command'(General => G_Set, Clear_Other => False,
                           Immediate => Natural (Value) / 65536 mod 65536,
                           others    => <>));
            end if;
      end case;
   end Integer_Operand;

   ------------
   -- Load_Y --
   ------------

   procedure Load_Y
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Y       : in out Command_Operand)
   is
      This : Operand_Processor :=
               (Target => Load_Y, Op => Y, Context => Context, others => <>);
   begin
      This.Process (Op);
      Context := This.Context;
      Cmds := This.Commands;
      Y    := This.Op;
   end Load_Y;

   -------------
   -- Load_YZ --
   -------------

   procedure Load_YZ
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Y, Z    : in out Command_Operand)
   is
      This : Operand_Processor :=
               (Target => Load_Y, Op => Y, Op_2 => Z, Context => Context,
                others => <>);
   begin
      This.Process (Op);
      Context := This.Context;
      Cmds := This.Commands;
      Y    := This.Op;
      Z    := This.Op_2;
   end Load_YZ;

   ------------
   -- Load_Z --
   ------------

   procedure Load_Z
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      Z       : out Command_Operand)
   is
      This : Operand_Processor :=
               (Target => Load_Z, Context => Context, others => <>);
   begin
      This.Process (Op);
      Cmds := This.Commands;
      Z    := This.Op;
   end Load_Z;

   ---------------
   -- Prepare_X --
   ---------------

   procedure Prepare_X
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      X       : out Command_Operand)
   is
      This : Operand_Processor :=
               (Target => Prepare_X, Context => Context, others => <>);
   begin
      This.Process (Op);
      Context := This.Context;
      Cmds := This.Commands;
      X    := This.Op;
   end Prepare_X;

   --------------------------
   -- Set_Jump_Destination --
   --------------------------

   procedure Set_Jump_Destination
     (Cmd         : in out Command;
      Context     : in out Operation_Context;
      Destination : out Natural;
      Op          : Tagatha.Operands.Operand_Type)
   is
      pragma Unreferenced (Cmd);
      This : Operand_Processor :=
               (Target => Jump_Destination, Context => Context, others => <>);
   begin
      This.Process (Op);
      Destination := This.Destination;
   end Set_Jump_Destination;

   -------------------
   -- Stack_Operand --
   -------------------

   overriding procedure Stack_Operand
     (Process : in out Operand_Processor;
      Context : Tagatha.Operands.Operand_Context)
   is
   begin
      case Process.Target is
         when Jump_Destination =>
            null;
         when Prepare_X =>
            Process.Op :=
              (Aqua.Word_8 (Process.Context.Stack), False);
            Process.Context.Stack := @ + 1;

         when Store_X =>
            null;

         when Load_Y | Load_Z =>
            Process.Context.Stack := @ - 1;
            Process.Op := (Aqua.Word_8 (Process.Context.Stack), False);

      end case;
   end Stack_Operand;

   -------------
   -- Store_X --
   -------------

   procedure Store_X
     (Op      : Tagatha.Operands.Operand_Type;
      Context : in out Operation_Context;
      Cmds    : out Command_Lists.List;
      X       : Command_Operand)
   is
      This : Operand_Processor :=
               (Target => Store_X, Op => X, Context => Context,
                others => <>);
   begin
      This.Process (Op);
      Cmds := This.Commands;
   end Store_X;

end Tagatha.Arch.Aqua_Generator.Operands;
