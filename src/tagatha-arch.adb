with Tagatha.Arch.M6502;
with Tagatha.Arch.Pdp11;

package body Tagatha.Arch is

   type Arch_Builder is access
     function return Any_Instance;

   package Arch_Maps is
     new WL.String_Maps (Arch_Builder);

   Arch_Map : Arch_Maps.Map;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This  : in out Instance;
      Group : Tagatha.Registers.Register_Group'Class)
      return Tagatha.Registers.Register
   is
      Key : constant String := Group.Name;
   begin
      if not This.Registers.Contains (Key) then
         This.Registers.Insert (Key, Active_Register_Vectors.Empty_Vector);
      end if;

      declare
         Vector : Active_Register_Vectors.Vector renames
                    This.Registers (Key);
      begin
         for I in 0 .. Vector.Last_Index loop
            if not Vector.Element (I) then
               Vector (I) := True;
               return I;
            end if;
         end loop;
         Vector.Append (True);
         return Vector.Last_Index;
      end;
   end Allocate;

   ----------------------
   -- Begin_Generation --
   ----------------------

   procedure Begin_Generation
     (This             : in out Instance;
      Source_File_Name : String)
   is
   begin
      This.Source_File_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Source_File_Name);
   end Begin_Generation;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (This     : in out Instance;
      Group    : Tagatha.Registers.Register_Group'Class;
      Register : Tagatha.Registers.Register)
   is
      Key : constant String := Group.Name;
      Vector : Active_Register_Vectors.Vector renames
                 This.Registers (Key);
   begin
      pragma Assert (Vector (Register), "register not allocated");
      Vector (Register) := False;
   end Deallocate;

   --------------------
   -- End_Generation --
   --------------------

   procedure End_Generation
     (This             : in out Instance)
   is
   begin
      null;
   end End_Generation;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Any_Instance is
   begin
      if Arch_Map.Contains (Name) then
         return Arch_Map (Name).all;
      else
         raise Constraint_Error with
           "unknown architecture: " & Name;
      end if;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (This    : in out Any_Instance;
      Process : in out Operand_Process'Class;
      Operand : Operands.Operand_Type)
   is
   begin
      Operands.Process (Process, Operand);
      This.Put (Process.Lines);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This        : in out Operand_Process;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
   is
   begin
      Tagatha.Assembler.Put
        (This.Lines, Instruction, Arg_1, Arg_2, Arg_3);
   end Put;

begin
   Arch_Map.Insert ("pdp-11", Pdp11.Get'Access);
   Arch_Map.Insert ("pdp11", Pdp11.Get'Access);
   Arch_Map.Insert ("6502", M6502.Get'Access);
end Tagatha.Arch;
