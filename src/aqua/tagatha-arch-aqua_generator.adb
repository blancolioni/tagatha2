with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Tagatha.Arch.Aqua_Generator.Inspect;
with Tagatha.Arch.Aqua_Generator.Operands;

package body Tagatha.Arch.Aqua_Generator is

   package Label_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Label_Maps is
     new WL.String_Maps (Positive);

   Label_Vector : Label_Vectors.Vector;
   Label_Map    : Label_Maps.Map;

   --  Next_Temporary_Index : Natural := 0;

   --  function Next_Temporary_Label return String;

   function Get_Label_Image
     (Label : Tagatha.Labels.Label)
      return String
   is ("_" & Tagatha.Labels.Image (Label));

   subtype Parent is Tagatha.Arch.Instance;

   type Instance is new Parent with
      record
         Last_Op    : Tagatha_Operator := Op_Nop;
         Commands   : Command_Lists.List;
         T1, T2, T3 : Command_Operand;
         Context    : Operation_Context;
      end record;

   overriding function Name
     (This : Instance)
      return String
   is ("pdp-11");

   overriding procedure Start_Routine
     (This   : in out Instance;
      Name   : String;
      Args   : Natural;
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

   overriding function Get_Group
     (This : Instance;
      Data : Tagatha_Data_Type;
      Size : Tagatha_Size)
      return Tagatha.Registers.Register_Group'Class;

   procedure Append
     (This : in out Instance'Class;
      Item : Command);

   procedure Append
     (This : in out Instance'Class;
      List : Command_Lists.List);

   procedure Put
     (This : in out Instance'Class;
      Cmd  : Command);

   procedure Assemble
     (This : in out Instance'Class;
      Cmd  : Command);

   procedure Branch_To_Label
     (This        : in out Instance'Class;
      Condition   : Tagatha_Condition;
      Negated     : Boolean;
      Destination : String);

   function Label_Reference
     (Name : String)
      return Positive;

   procedure Peephole
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean);

   procedure Move_Registers
     (Commands : in out Command_Lists.List);

   procedure Register_State
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean);

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Instance'Class;
      Item : Command)
   is
   begin
      This.Commands.Append (Item);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Instance'Class;
      List : Command_Lists.List)
   is
   begin
      for Item of List loop
         This.Commands.Append (Item);
      end loop;
   end Append;

   --------------
   -- Assemble --
   --------------

   procedure Assemble
     (This : in out Instance'Class;
      Cmd  : Command)
   is

      function R (X : Natural) return String
      is ("%" & Ada.Strings.Fixed.Trim (X'Image, Ada.Strings.Both));

      function R (X : Command_Operand) return String
      is ((if X.Imm then "" else "%")
          & Ada.Strings.Fixed.Trim (X.R'Image, Ada.Strings.Both));

      function Rs return String
      is (R (Cmd.X)
          & ", " & R (Cmd.Y)
          & ", " & R (Cmd.Z));

   begin
      case Cmd.General is
         when G_Call =>
            This.Put
              ("PUSHJ " & R (Cmd.X)
               & ", " & Label_Vector (Cmd.Branch_Label));

         when G_Return =>
            This.Put
              ("PUT rJ, "
               & R (This.Context.Return_Address));

            This.Put
              ("POP" & Cmd.X.R'Image & ", 0");

         when G_Begin =>
            This.Put
              ("GET "
               & R (This.Context.Return_Address)
               & ", rJ");

         when G_Operate =>
            case Cmd.Operator is
               when Op_Nop =>
                  This.Put
                    ("SET "
                     & R (Cmd.X)
                     & ", "
                     & R (Cmd.Y));
               when Condition_Operator =>
                  This.Put ("CMP " & R (Cmd.X) & ", " & R (Cmd.Y)
                            & ", " & R (Cmd.Z));

                  declare
                     Op : constant String :=
                            (case Condition_Operator (Cmd.Operator) is
                                when Op_Equal         => "ZSZ",
                                when Op_Not_Equal     => "ZSNZ",
                                when Op_Greater       => "ZSP",
                                when Op_Less          => "ZSN",
                                when Op_Greater_Equal => "ZSNN",
                                when Op_Less_Equal    => "ZSNP");
                  begin
                     This.Put (Op & " "
                               & R (Cmd.X)
                               & ", "
                               & R (Cmd.X)
                               & ", 1");
                  end;
               when Op_Test =>
                  null;

               when others =>
                  if Cmd.X.R < 256 and then Cmd.Y.R < 256 then
                     declare
                        Op_Img : constant String :=
                                   Cmd.Operator'Image;
                     begin
                        This.Put
                          (Op_Img (4 .. Op_Img'Last)
                           & " " & Rs);
                     end;
                  else
                     Ada.Text_IO.Put_Line ("unhandled: " & Cmd'Image);
                  end if;

            end case;

         when G_Branch =>
            declare
               S : constant String :=
                     (case Cmd.Condition is
                         when C_Always    => "JMP",
                         when C_Equal     => "Z",
                         when C_Not_Equal => "NZ",
                         when C_Greater   => "P",
                         when C_Less      => "N",
                         when C_At_Least  => "NN",
                         when C_At_Most   => "NP");
            begin
               This.Put
                 ((if Cmd.Condition = C_Always then "JMP "
                  else "B" & S & " " & R (Cmd.X) & ", ")
                  & Label_Vector.Element (Cmd.Branch_Label));
            end;
         when G_Set =>
            This.Put
              ((if Cmd.Clear_Other then "SET" else "INC")
               & (if Cmd.Set_High then "H" else "L")
               & " "
               & R (Cmd.X)
               & ","
               & Natural'Image (Cmd.Immediate));

         when G_None =>
            null;
      end case;

   end Assemble;

   ------------
   -- Branch --
   ------------

   overriding procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label)
   is
   begin
      if This.Last_Op = Op_Nop then
         This.Branch_To_Label
           (Condition, False, Get_Label_Image (Destination));
      else
         This.Branch_To_Label
           (To_Condition (This.Last_Op),
            Condition = C_Equal,
            Get_Label_Image (Destination));
         This.Last_Op := Op_Nop;
      end if;
   end Branch;

   ---------------------
   -- Branch_To_Label --
   ---------------------

   procedure Branch_To_Label
     (This        : in out Instance'Class;
      Condition   : Tagatha_Condition;
      Negated     : Boolean;
      Destination : String)
   is
   begin
      This.Append
        (Command'
           (General => G_Branch,
            Branch_Label    => Label_Reference (Destination),
            Condition       => Condition,
            Negated         => Negated,
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
      Cmd : Command := Command'
        (General => G_Call,
         X       => (This.Context.Stack - Argument_Count - 1,
                     False, False),
         Save_J  => This.Context.Return_Address,
         others  => <>);
   begin
      Operands.Set_Jump_Destination (Cmd, This.Context, Cmd.Branch_Label,
                                     Destination);
      This.Append (Cmd);
   end Call;

   ------------------
   -- Change_Stack --
   ------------------

   overriding procedure Change_Stack
     (This   : in out Instance;
      Change : Integer)
   is
   begin
      This.Context.Stack := This.Context.Stack + Change;
   end Change_Stack;

   --------------------
   -- End_Generation --
   --------------------

   overriding procedure End_Generation
     (This   : in out Instance)
   is
      Changed : Boolean := True;
   begin
      while Changed loop
         Peephole (This.Commands, Changed);
         if not Changed then
            Register_State (This.Commands, Changed);
         end if;
      end loop;

      if True then
         Move_Registers (This.Commands);
      end if;

      for Command of This.Commands loop
         This.Put (Command);
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
      This.Append
        (Command'
           (General         => G_Return,
            X               => (0, False, False),
            Save_J          => This.Context.Return_Address,
            others          => <>));
   end End_Routine;

   ---------
   -- Get --
   ---------

   function Get return Any_Instance is
   begin
      return Arch : Instance (32);
   end Get;

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
                    Floating_Point_Register, 1, 30);
      else
         Result := (Tagatha.Registers.Register_Group with
                    General_Register, 1, 80);
      end if;
      return Result;
   end Get_Group;

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

   ----------
   -- Move --
   ----------

   overriding procedure Move
     (This        : in out Instance;
      Source      : Tagatha.Operands.Operand_Type;
      Destination : Tagatha.Operands.Operand_Type)
   is
      Cmd             : Command :=
                          (General => G_Operate, others => <>);
      Load_X, Load_YZ : Command_Lists.List;
      Store_X         : Command_Lists.List;
   begin
      Cmd.X := This.T1;
      Cmd.Y := This.T2;
      Cmd.Z := This.T3;
      Operands.Load_YZ (Source, This.Context, Load_YZ, Cmd.Y, Cmd.Z);
      Operands.Prepare_X (Destination, This.Context, Load_X, Cmd.X);

      This.Append (Load_YZ);
      This.Append (Load_X);
      This.Append (Cmd);
      Operands.Store_X (Destination, This.Context, Store_X, Cmd.X);
      This.Append (Store_X);
   end Move;

   --------------------
   -- Move_Registers --
   --------------------

   procedure Move_Registers
     (Commands : in out Command_Lists.List)
   is

      type Reg_Record is
         record
            Map_To : Natural;
            Used   : Boolean;
         end record;

      Rs : array (0 .. 255) of Reg_Record;

      procedure Check
        (Op  : Command_Operand);

      procedure Update
        (Op  : in out Command_Operand);

      -----------
      -- Check --
      -----------

      procedure Check
        (Op  : Command_Operand)
      is
      begin
         if not Op.Imm then
            Rs (Op.R).Used := True;
         end if;
      end Check;

      ------------
      -- Update --
      ------------

      procedure Update
        (Op  : in out Command_Operand)
      is
      begin
         if not Op.Imm then
            Op.R := Rs (Op.R).Map_To;
         end if;
      end Update;

   begin
      for R in Rs'Range loop
         Rs (R) := (R, False);
      end loop;

      for Cmd of Commands loop
         Check (Cmd.X);
         Check (Cmd.Y);
         Check (Cmd.Z);
      end loop;

      declare
         Next : Natural := 0;
      begin
         for Rec of Rs loop
            if Rec.Used then
               if False then
                  Ada.Text_IO.Put_Line
                    ("map:" & Rec.Map_To'Image & " ->" & Next'Image);
               end if;
               Rec.Map_To := Next;
               Next := Next + 1;
            end if;
         end loop;
      end;

      for Cmd of Commands loop
         Update (Cmd.X);
         Update (Cmd.Y);
         Update (Cmd.Z);
      end loop;

   end Move_Registers;

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
      Cmd                    : Command;
      Load_X, Load_Y, Load_Z : Command_Lists.List;
      Store_X                : Command_Lists.List;

      procedure Report (List : Command_Lists.List);

      ------------
      -- Report --
      ------------

      procedure Report (List : Command_Lists.List) is
      begin
         for Cmd of List loop
            Ada.Text_IO.Put_Line (Cmd'Image);
         end loop;
      end Report;

   begin
      Cmd.X := This.T1;
      Cmd.Y := This.T2;
      Cmd.Z := This.T3;

      Operands.Load_Y (Source_1, This.Context, Load_Y, Cmd.Y);
      Operands.Load_Z (Source_2, This.Context, Load_Y, Cmd.Z);
      Operands.Prepare_X (Destination, This.Context, Load_X, Cmd.X);

      if False then
         Report (Load_Y);
         Report (Load_Z);
         Report (Load_X);
      end if;

      This.Append (Load_Y);
      This.Append (Load_Z);
      This.Append (Load_X);

      Cmd.General := G_Operate;
      Cmd.Operator := Operator;
      This.Append (Cmd);

      --  Ada.Text_IO.Put_Line (Cmd'Image);

      Operands.Store_X (Destination, This.Context, Store_X, Cmd.X);
      This.Append (Store_X);

      Report (Store_X);

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
            Cmd         : constant Command := Command_Lists.Element (Position);
            New_Command : Command;
            Can_Update  : constant Boolean :=
                            Cmd.Label_List.Is_Empty
                                and then Previous.General not in
                                  G_Branch | G_Call | G_Begin | G_Return;
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

   procedure Put
     (This : in out Instance'Class;
      Cmd  : Command)
   is
   begin
      for Label of Cmd.Label_List loop
         This.Put_Label (Label_Vector.Element (Label));
      end loop;

      This.Assemble (Cmd);
   end Put;

   --------------------
   -- Register_State --
   --------------------

   procedure Register_State
     (Commands : in out Command_Lists.List;
      Changed  : out Boolean)
   is
      pragma Unreferenced (Commands);
   begin
      Changed := False;
   end Register_State;

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
      This.Context :=
        Operation_Context'
          (First_Argument  => 0, Argument_Count => Args,
           Return_Address  => Args,
           First_Temporary => Args + 1, Temporary_Count => 3,
           First_Stack     => Args + 4, Stack => Args + 4,
           To_Integer      => Label_Reference'Access);
      This.T1 := (This.Context.First_Temporary,
                  False, False);
      This.T2 := (This.Context.First_Temporary + 1,
                  False, False);
      This.T3 := (This.Context.First_Temporary + 2,
                  False, False);
      This.Append (Command'(General => G_Begin, others => <>));
   end Start_Routine;

end Tagatha.Arch.Aqua_Generator;
