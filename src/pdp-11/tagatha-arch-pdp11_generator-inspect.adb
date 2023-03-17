package body Tagatha.Arch.Pdp11_Generator.Inspect is

   type Checker is access
     function (Previous, Current : Command) return Boolean;

   type Joiner is access
     function (Previous, Current : Command) return Command;

   type Check_Record is
      record
         Check : Checker;
         Join  : Joiner;
      end record;

   package Check_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Check_Record);

   Check_List : Check_Lists.List;

   function Check_Add_Zero
     (Previous, Current : Command)
      return Boolean;

   function Join_Add_Zero
     (Previous, Current : Command)
      return Command;

   function Check_Pop_Push
     (Previous, Current : Command)
      return Boolean;

   function Join_Pop_Push
     (Previous, Current : Command)
      return Command;

   function Check_Subtract_Add
     (Previous, Current : Command)
      return Boolean;

   function Join_Subtract_Add
     (Previous, Current : Command)
      return Command;

   --------------------
   -- Check_Add_Zero --
   --------------------

   function Check_Add_Zero
     (Previous, Current : Command)
      return Boolean
   is
      pragma Unreferenced (Previous);
      use Pdp11.ISA;
   begin
      return Current.Has_Instruction
        and then Current.Instruction in I_ADD | I_SUB
        and then Current.Src.Operand = Immediate_Operand
        and then Current.Src.Value = 0;
   end Check_Add_Zero;

   --------------------
   -- Check_Pop_Push --
   --------------------

   function Check_Pop_Push
     (Previous, Current : Command)
      return Boolean
   is
      use Pdp11.ISA;
   begin
      return Previous.Has_Instruction and then Current.Has_Instruction
        and then Previous.Instruction = I_TST
        and then Previous.Src.Operand = (Autoincrement_Mode, False, 6)
        and then Current.Instruction in Double_Operand_Instruction
        and then Current.Dst.Operand = (Autodecrement_Mode, False, 6);
   end Check_Pop_Push;

   --------------------
   -- Check_Sequence --
   --------------------

   procedure Check_Sequence
     (Previous    : Command;
      Current     : Command;
      Replacement : out Command;
      Changed     : out Boolean)
   is
   begin
      for Item of Check_List loop
         if Item.Check (Previous, Current) then
            Replacement := Item.Join (Previous, Current);
            Changed := True;
            return;
         end if;
      end loop;
      Changed := False;
   end Check_Sequence;

   ------------------------
   -- Check_Subtract_Add --
   ------------------------

   function Check_Subtract_Add
     (Previous, Current : Command)
      return Boolean
   is
      use Pdp11.ISA;
   begin
      return Current.Has_Instruction
        and then Current.Instruction in I_ADD | I_SUB
        and then Current.Src.Operand = Immediate_Operand
        and then Previous.Has_Instruction
        and then Previous.Instruction in I_ADD | I_SUB
        and then Previous.Src.Operand = Immediate_Operand
        and then Current.Dst = Previous.Dst;
   end Check_Subtract_Add;

   -------------------
   -- Join_Add_Zero --
   -------------------

   function Join_Add_Zero
     (Previous, Current : Command)
      return Command
   is
      pragma Unreferenced (Current);
   begin
      return Previous;
   end Join_Add_Zero;

   -------------------
   -- Join_Pop_Push --
   -------------------

   function Join_Pop_Push
     (Previous, Current : Command)
      return Command
   is
      pragma Unreferenced (Previous);
      use Pdp11.ISA;
      Replace_Stack_Top : constant Operand_Type :=
                            (Register_Mode, True, 6);
   begin
      return Cmd : Command := Current do
         Cmd.Dst.Operand := Replace_Stack_Top;
      end return;
   end Join_Pop_Push;

   -----------------------
   -- Join_Subtract_Add --
   -----------------------

   function Join_Subtract_Add
     (Previous, Current : Command)
      return Command
   is
      use all type Pdp11.ISA.Instruction_Type;
   begin
      return Join : Command := Previous do
         if Previous.Instruction = Current.Instruction then
            Join.Src.Value := Join.Src.Value + Current.Src.Value;
         else
            if Current.Src.Value > Join.Src.Value then
               Join.Instruction := Current.Instruction;
               Join.Src.Value := Current.Src.Value - Join.Src.Value;
            else
               Join.Src.Value := Join.Src.Value - Current.Src.Value;
            end if;
         end if;
      end return;
   end Join_Subtract_Add;

   ----------
   -- Save --
   ----------

   procedure Save
     (State   : in out Machine_State;
      List    : in out Command_Lists.List;
      Current : Command_Lists.Cursor;
      Changed : out Boolean)
   is
      use Pdp11.ISA;
      Cmd : constant Command := Command_Lists.Element (Current);
      Rs   : Register_State_Array renames State.Registers;
      Src  : constant Operand_Type := Cmd.Src.Operand;
      Dst  : constant Operand_Type := Cmd.Dst.Operand;

      procedure Log (Register : Register_Index;
                     State    : Register_State);

      ---------
      -- Log --
      ---------

      procedure Log (Register : Register_Index;
                     State    : Register_State)
      is null;
      --  begin
      --     Ada.Text_IO.Put_Line
      --       ((if Register = 7 then "pc"
      --        elsif Register = 6 then "sp"
      --        elsif Register = 5 then "fp"
      --        else ('r', Character'Val (Register + 48)))
      --        & " <- "
      --        & (if State.Known
      --          then ('r', Character'Val (State.Src + 48))
      --            & (if State.Offset > 0
      --              then " +" & State.Offset'Image
      --              elsif State.Offset < 0
      --              then " -" & Integer'Image (-State.Offset)
      --            else "")
      --          else "unknown"));
      --  end Log;

   begin
      Changed := False;
      if not Cmd.Label_List.Is_Empty then
         State := (others => <>);
      end if;

      if Cmd.Has_Instruction
        and then Cmd.Dst.Operand.Mode = Register_Mode
        and then Cmd.Dst.Operand.Deferred = False
      then

         for St of State.Registers loop
            if St.Src = Cmd.Dst.Operand.Register then
               St := (others => <>);
            end if;
         end loop;

         if Cmd.Instruction = I_MOV
           and then Cmd.Src.Operand.Mode = Register_Mode
           and then Cmd.Src.Operand.Deferred = False
         then
            declare
               Src_State : constant Register_State :=
                             Rs (Src.Register)
                             with Unreferenced;
               Dst_State : Register_State renames
                             Rs (Dst.Register);
            begin
               Dst_State := Register_State'
                 (Known    => True,
                  Src      => Src.Register,
                  Offset   => 0,
                  Loaded   => Current,
                  Adjusted => Command_Lists.No_Element);
               Log (Dst.Register, Dst_State);
            end;
         elsif (Cmd.Instruction = I_ADD
                or else Cmd.Instruction = I_SUB)
           and then Cmd.Src.Operand = Immediate_Operand
           and then Rs (Dst.Register).Known
         then
            if not Command_Lists.Has_Element
              (Rs (Dst.Register).Adjusted)
            then
               declare
                  Offset : Integer renames Rs (Dst.Register).Offset;
               begin
                  if Cmd.Instruction = I_ADD then
                     Offset := Offset + Natural (Cmd.Src.Value);
                  else
                     Offset := Offset - Natural (Cmd.Src.Value);
                  end if;
               end;
               Rs (Dst.Register).Adjusted := Current;
               Log (Dst.Register, Rs (Dst.Register));
            else
               Rs (Dst.Register) := (others => <>);
               Log (Dst.Register, Rs (Dst.Register));
            end if;
         end if;
      elsif Dst.Mode = Register_Mode
        and then Dst.Deferred
        and then Rs (Dst.Register).Known
      then
         --  Ada.Text_IO.Put_Line
         --    ("optimizing: " & Rs (Dst.Register)'Image);
         declare
            St : Register_State renames Rs (Dst.Register);
         begin
            if Command_Lists.Has_Element (St.Loaded) then
               List.Delete (St.Loaded);
               St.Loaded := Command_Lists.No_Element;
            end if;
            if Command_Lists.Has_Element (St.Adjusted) then
               List.Delete (St.Adjusted);
               St.Adjusted := Command_Lists.No_Element;
            end if;
            List.Replace_Element
              (Current,
               Command'
                 (Has_Instruction => True,
                  Label_List      => <>,
                  Src             => Cmd.Src,
                  Dst             =>
                    Command_Operand'
                      (Operand =>
                           Operand_Type'
                         (Mode     => Index_Mode,
                          Deferred => False,
                          Register => St.Src),
                       Offset  => Frame_Offset (St.Offset),
                       others  => <>),
                  Byte            => Cmd.Byte,
                  Branch_Label    => 0,
                  Instruction     => Cmd.Instruction));
            Changed := True;
         end;
      elsif Src.Mode = Register_Mode
        and then Src.Deferred
        and then Rs (Src.Register).Known
      then
         --  Ada.Text_IO.Put_Line
         --    ("optimizing: " & Rs (Src.Register)'Image);
         declare
            St : Register_State renames Rs (Src.Register);
         begin
            if Command_Lists.Has_Element (St.Loaded) then
               List.Delete (St.Loaded);
               St.Loaded := Command_Lists.No_Element;
            end if;
            if Command_Lists.Has_Element (St.Adjusted) then
               List.Delete (St.Adjusted);
               St.Adjusted := Command_Lists.No_Element;
            end if;
            List.Replace_Element
              (Current,
               Command'
                 (Has_Instruction => True,
                  Label_List      => <>,
                  Src             =>
                    Command_Operand'
                      (Operand =>
                           Operand_Type'
                         (Mode     => Index_Mode,
                          Deferred => False,
                          Register => St.Src),
                       Offset  => Frame_Offset (St.Offset),
                       others  => <>),
                  Dst             => Cmd.Dst,
                  Byte            => Cmd.Byte,
                  Branch_Label    => 0,
                  Instruction     => Cmd.Instruction));
            Changed := True;
         end;
      end if;
   end Save;

begin
   Check_List.Append
     (Check_Record'(Check_Add_Zero'Access, Join_Add_Zero'Access));
   Check_List.Append
     (Check_Record'(Check_Pop_Push'Access, Join_Pop_Push'Access));
   Check_List.Append
     (Check_Record'(Check_Subtract_Add'Access, Join_Subtract_Add'Access));
end Tagatha.Arch.Pdp11_Generator.Inspect;
