package body Tagatha.Arch.Aqua_Generator.Inspect is

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

   --------------------
   -- Check_Add_Zero --
   --------------------

   function Check_Add_Zero
     (Previous, Current : Command)
      return Boolean
   is
      pragma Unreferenced (Previous);
   begin
      return Current.General = G_Operate
        and then Current.Operator in Op_Add | Op_Sub
        and then Current.Z.Imm
        and then Current.Z.R = 0
        and then Current.X = Current.Y;
   end Check_Add_Zero;

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

   ----------
   -- Save --
   ----------

   procedure Save
     (State   : in out Machine_State;
      List    : in out Command_Lists.List;
      Current : Command_Lists.Cursor;
      Changed : out Boolean)
   is
      Cmd  : constant Command := Command_Lists.Element (Current);
      Rs   : Register_State_Array renames State.Registers;
   begin
      Changed := False;
      if not Cmd.Label_List.Is_Empty then
         State := (others => <>);
      end if;

      if Cmd.General = G_Set
        or else (Cmd.General = G_Operate and then Cmd.Operator = Op_Nop)
      then
         for St of State.Registers loop
            if St.Src = Cmd.X.R then
               St := (others => <>);
            end if;
         end loop;

         if not Cmd.Y.Def then
            declare
               Src_State : Register_State renames
                             Rs (Cmd.Y.R);
               Dst_State : Register_State renames
                             Rs (Cmd.X.R);
            begin
               if not Cmd.Y.Imm
                 and then Command_Lists.Has_Element (Src_State.Loaded)
                 and then not Command_Lists.Has_Element (Src_State.Adjusted)
               then
                  declare
                     Src : constant Command :=
                             Command_Lists.Element (Src_State.Loaded);
                  begin
                     List.Replace_Element
                       (Current,
                        (Cmd with delta
                             General => Src.General,
                         Y       => Src.Y,
                         Immediate => Src.Immediate,
                         Clear_Other => Src.Clear_Other));
                     List.Delete (Src_State.Loaded);
                     Changed := True;
                  end;
               else
                  Dst_State := Register_State'
                    (Known     => True,
                     Immediate => Cmd.Y.Imm,
                     Src       => (if Cmd.Y.Imm
                                   then Cmd.Immediate
                                   else Cmd.Y.R),
                     Offset    => 0,
                     Loaded    => Current,
                     Adjusted  => Command_Lists.No_Element);
               end if;
            end;
         end if;
      end if;
   end Save;

begin
   Check_List.Append
     (Check_Record'(Check_Add_Zero'Access, Join_Add_Zero'Access));
end Tagatha.Arch.Aqua_Generator.Inspect;
