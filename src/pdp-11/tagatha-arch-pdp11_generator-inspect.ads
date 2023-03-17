private package Tagatha.Arch.Pdp11_Generator.Inspect is

   procedure Check_Sequence
     (Previous    : Command;
      Current     : Command;
      Replacement : out Command;
      Changed     : out Boolean);

   type Machine_State is private;

   procedure Save
     (State   : in out Machine_State;
      List    : in out Command_Lists.List;
      Current : Command_Lists.Cursor;
      Changed : out Boolean);

private

   type Register_State is
      record
         Known    : Boolean := False;
         Src      : Pdp11.ISA.Register_Index := 0;
         Offset   : Integer := 0;
         Loaded   : Command_Lists.Cursor := Command_Lists.No_Element;
         Adjusted : Command_Lists.Cursor := Command_Lists.No_Element;
      end record;

   type Register_State_Array is
     array (Pdp11.ISA.Register_Index) of Register_State;

   type Machine_State is
      record
         Registers : Register_State_Array;
      end record;

end Tagatha.Arch.Pdp11_Generator.Inspect;
