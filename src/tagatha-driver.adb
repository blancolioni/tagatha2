with Tagatha.Arch;
with Tagatha.Code;
with Tagatha.Labels;
with Tagatha.Operands;

procedure Tagatha.Driver is
   Code : Tagatha.Code.Instance;
   Label : constant Tagatha.Labels.Label :=
             Code.Next_Label;
begin
   Code.Begin_Routine ("test_gen", 0, 0, True);
   Code.Reserve (3);
   Code.Push
     (Tagatha.Operands.Take_Address (Tagatha.Operands.Local_Operand (1)));
   Code.Push (Tagatha.Operands.Constant_Operand (65));
   Code.Store;
   Code.Push
     (Tagatha.Operands.Take_Address (Tagatha.Operands.Local_Operand (1)));
   Code.Push
     (Tagatha.Operands.Local_Operand (1));
   Code.Push (Tagatha.Operands.Constant_Operand (3));
   Code.Operate (Tagatha.Op_Add);
   Code.Store;
   Code.Push (Tagatha.Operands.Local_Operand (1));
   Code.Push (Tagatha.Operands.Constant_Operand (22));
   Code.Operate (Tagatha.Op_Greater);
   Code.Branch (Tagatha.C_Equal, Label);
   Code.Push
     (Tagatha.Operands.Take_Address
        (Tagatha.Operands.Set_Size (Tagatha.Default_Floating_Point_Size,
         Tagatha.Operands.Set_Data_Type (Tagatha.Floating_Point_Data,
           Tagatha.Operands.Local_Operand (3)))));
   Code.Push (Tagatha.Operands.Constant_Operand (1.0));
   Code.Store;
   Code.Label (Label);
   Code.Push (Tagatha.Operands.Local_Operand (1));
   Code.Push (Tagatha.Operands.External_Operand ("putchar"));
   Code.Call (1);
   Code.Drop (3);
   Code.End_Routine;

   declare
      Arch : Tagatha.Arch.Any_Instance :=
               Tagatha.Arch.Get ("pdp-11");
   begin
      Code.Generate (Arch);
      Arch.Save ("test-gen.s");
   end;

end Tagatha.Driver;
