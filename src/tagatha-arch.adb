with Tagatha.Arch.Pdp11;

package body Tagatha.Arch is

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
      if Name = "pdp-11" then
         return Tagatha.Arch.Pdp11.Get;
      else
         raise Constraint_Error with
           "unknown architecture: " & Name;
      end if;
   end Get;

end Tagatha.Arch;
