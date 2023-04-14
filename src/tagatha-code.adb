with Ada.Strings.Fixed;
with Ada.Text_IO;

with Tagatha.Registers;
with Tagatha.Stacks;

package body Tagatha.Code is

   type Transfer_Record is
      record
         Src_1, Src_2 : Operand_Holders.Holder;
         Operator     : Tagatha_Operator;
         Annotation   : Operand_Holders.Holder;
      end record;

   type Stacked_Element_Type is
     (No_Element, Operand_Element, Transfer_Element);

   type Stacked_Element
     (Element_Type : Stacked_Element_Type := No_Element)
   is
      record
         case Element_Type is
            when No_Element =>
               null;
            when Operand_Element =>
               Operand : Operand_Holders.Holder;
            when Transfer_Element =>
               Transfer : Transfer_Record;
         end case;
      end record;

   function No_Element return Stacked_Element is
     ((Element_Type => No_Element));

   package Operand_Stacks is new Tagatha.Stacks (Stacked_Element);

   ------------
   -- Append --
   ------------

   procedure Append
     (This : in out Instance;
      Item : Instruction_Record)
   is
      Copy : constant Instruction_Record :=
               (Item with delta Labels => This.Pending_Labels);
   begin
      This.Active_Routine.Vector.Append (Copy);
      This.Pending_Labels.Clear;
   end Append;

   -------------------
   -- Begin_Routine --
   -------------------

   procedure Begin_Routine
     (This           : in out Instance;
      Name           : in     String;
      Argument_Words : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean)
   is
   begin
      This.Active_Routine :=
        Routine_Type'
          (Name           => Ada.Strings.Unbounded.To_Unbounded_String (Name),
           Argument_Words => Argument_Words,
           Result_Words   => Result_Words,
           Global         => Global,
           Vector         => Instruction_Vectors.Empty_Vector);
   end Begin_Routine;

   ------------
   -- Branch --
   ------------

   procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label)
   is
   begin
      This.Append (Branch (Condition, Destination));
   end Branch;

   ----------
   -- Call --
   ----------

   procedure Call
     (This           : in out Instance;
      Result         : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural)
   is
   begin
      This.Append (Call (Result, Argument_Count));
   end Call;

   ----------
   -- Drop --
   ----------

   procedure Drop
     (This       : in out Instance;
      Word_Count : Natural)
   is
   begin
      if Word_Count > 0 then
         This.Append (Reserve (-Word_Count));
      end if;
   end Drop;

   -----------------
   -- End_Routine --
   -----------------

   procedure End_Routine
     (This           : in out Instance)
   is
   begin
      if not This.Pending_Labels.Is_Empty then
         This.Append (Reserve (0));
      end if;
      This.Routines.Append (This.Active_Routine);
   end End_Routine;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (This : in out Instance;
      Arch : in out Tagatha.Arch.Any_Instance)
   is
      Operand_Stack  : Operand_Stacks.Stack;

      function Get_Operand
        (From : Stacked_Element)
         return Operands.Operand_Type;

      procedure Push_Operand
        (Element : Stacked_Element);

      procedure Evaluate
        (Element : Stacked_Element);

      procedure Deallocate_Source
        (Element : Stacked_Element);

      procedure Deallocate
        (Operand : Operands.Operand_Type);

      ----------------
      -- Deallocate --
      ----------------

      procedure Deallocate
        (Operand : Operands.Operand_Type)
      is
         procedure Deallocate
           (Group    : Tagatha.Registers.Register_Group'Class;
            Register : Tagatha.Registers.Register);

         ----------------
         -- Deallocate --
         ----------------

         procedure Deallocate
           (Group    : Tagatha.Registers.Register_Group'Class;
            Register : Tagatha.Registers.Register)
         is
         begin
            Arch.Deallocate (Group, Register);
         end Deallocate;

      begin
         Operands.Update_Registers (Operand, Deallocate'Access);
      end Deallocate;

      -----------------------
      -- Deallocate_Source --
      -----------------------

      procedure Deallocate_Source
        (Element : Stacked_Element)
      is
      begin
         case Element.Element_Type is
            when No_Element =>
               null;
            when Operand_Element =>
               Deallocate (Element.Operand.Element);
            when Transfer_Element =>
               Deallocate (Element.Transfer.Src_1.Element);
               Deallocate (Element.Transfer.Src_2.Element);
         end case;
      end Deallocate_Source;

      --------------
      -- Evaluate --
      --------------

      procedure Evaluate
        (Element : Stacked_Element)
      is
      begin
         case Element.Element_Type is
            when No_Element =>
               null;
            when Operand_Element =>
               Arch.Operate
                 (Operator    => Op_Test,
                  Source_1    => Element.Operand.Element,
                  Source_2    => Operands.No_Operand,
                  Destination => Operands.No_Operand);
            when Transfer_Element =>
               Arch.Operate
                 (Operator    => Element.Transfer.Operator,
                  Source_1    => Element.Transfer.Src_1.Element,
                  Source_2    => Element.Transfer.Src_2.Element,
                  Destination => Operands.No_Operand);
         end case;
         Deallocate_Source (Element);
      end Evaluate;

      -----------------
      -- Get_Operand --
      -----------------

      function Get_Operand
        (From : Stacked_Element)
         return Operands.Operand_Type
      is
      begin
         case From.Element_Type is
            when No_Element =>
               return Operands.No_Operand;
            when Operand_Element =>
               return From.Operand.Element;
            when Transfer_Element =>
               declare
                  use type Tagatha.Operands.Operand_Type;
                  Src : constant Operands.Operand_Type :=
                          From.Transfer.Src_1.Element;
                  Anno : constant Operands.Operand_Type :=
                           From.Transfer.Annotation.Element;
                  Group : constant Tagatha.Registers.Register_Group'Class :=
                            Arch.Get_Group
                              (Data => Operands.Data_Type (Src),
                               Size => Operands.Size (Src));
                  R     : constant Tagatha.Registers.Register :=
                            Arch.Allocate (Group);
                  Dst : constant Operands.Operand_Type :=
                            Operands.Register_Assignment_Operand
                              (Operand  => (if Anno = Operands.No_Operand
                                            then Src else Anno),
                               Group    => Group,
                               Register => R);
               begin
                  Arch.Operate
                    (Operator    => From.Transfer.Operator,
                     Source_1    => From.Transfer.Src_1.Element,
                     Source_2    => From.Transfer.Src_2.Element,
                     Destination => Dst);

                  return Operands.Register_Operand
                    (Operand  => (if Anno = Operands.No_Operand
                                  then Src else Anno),
                     Group    => Group,
                     Register => R);
               end;
         end case;
      end Get_Operand;

      ------------------
      -- Push_Operand --
      ------------------

      procedure Push_Operand
        (Element : Stacked_Element)
      is
      begin
         case Element.Element_Type is
            when No_Element =>
               null;
            when Operand_Element =>
               Arch.Move
                 (Element.Operand.Element,
                  Tagatha.Operands.To_Stack_Operand (Element.Operand.Element));
            when Transfer_Element =>
               Arch.Operate
                 (Operator    => Element.Transfer.Operator,
                  Source_1    => Element.Transfer.Src_1.Element,
                  Source_2    => Element.Transfer.Src_2.Element,
                  Destination => Tagatha.Operands.Stack_Operand);
         end case;
      end Push_Operand;

   begin

      Arch.Begin_Generation;

      for Routine of This.Routines loop
         Arch.Start_Routine
           (Name   => Ada.Strings.Unbounded.To_String (Routine.Name),
            Args   => Routine.Argument_Words,
            Global => Routine.Global);

         for Item of Routine.Vector loop
            for Label of Item.Labels loop
               Arch.Local_Label (Label);
            end loop;
            case Item.T is
               when Push =>
                  Operand_Stack.Push ((Operand_Element, Item.Operand));
               when Store =>
                  declare
                     Src : constant Stacked_Element :=
                             Operand_Stack.Pop;
                     Dst : constant Stacked_Element :=
                             Operand_Stack.Pop;
                     Dst_Op : constant Operands.Operand_Type :=
                                Get_Operand (Dst);
                  begin
                     case Src.Element_Type is
                        when No_Element =>
                           null;
                        when Operand_Element =>
                           Arch.Move
                             (Src.Operand.Element, Dst_Op);
                        when Transfer_Element =>
                           Arch.Operate
                             (Operator    => Src.Transfer.Operator,
                              Source_1    => Src.Transfer.Src_1.Element,
                              Source_2    => Src.Transfer.Src_2.Element,
                              Destination => Dst_Op);
                     end case;
                     Deallocate_Source (Src);
                     Deallocate (Dst_Op);
                  end;

               when Operator =>
                  declare
                     Right : constant Stacked_Element :=
                               (if Item.Op in Two_Argument_Operator
                                then Operand_Stack.Pop
                                else No_Element);
                     Left  : constant Stacked_Element :=
                               (if Item.Op not in Zero_Argument_Operator
                                then Operand_Stack.Pop
                                else No_Element);
                  begin
                     Operand_Stack.Push
                       (Element => Stacked_Element'
                          (Element_Type => Transfer_Element,
                           Transfer     => Transfer_Record'
                             (Src_1    =>
                                  Operand_Holders.To_Holder
                                (Get_Operand (Left)),
                              Src_2    =>
                                Operand_Holders.To_Holder
                                  (Get_Operand (Right)),
                              Operator => Item.Op,
                              Annotation => Item.Op_Result)));
                  end;

               when Branch =>

                  if Item.Condition /= C_Always then
                     Evaluate (Operand_Stack.Pop);
                  end if;
                  Arch.Branch
                    (Condition   => Item.Condition,
                     Destination => Item.Destination);

               when Call =>
                  declare
                     Arg_Stack   : Operand_Stacks.Stack;
                     Destination : constant Tagatha.Operands.Operand_Type :=
                                     Get_Operand (Operand_Stack.Pop);
                  begin
                     for I in 1 .. Item.Argument_Count loop
                        Arg_Stack.Push (Operand_Stack.Pop);
                     end loop;
                     for I in 1 .. Item.Argument_Count loop
                        Push_Operand (Arg_Stack.Pop);
                     end loop;
                     Arch.Call (Destination, Item.Argument_Count);
                     Arch.Change_Stack (-Item.Argument_Count);
                     Deallocate (Destination);
                     Operand_Stack.Push ((Operand_Element, Item.Result));
                  end;
               when Reserve =>
                  Arch.Change_Stack (Item.Word_Count);
               when Transfer =>
                  Arch.Operate
                    (Operator    => Item.Transfer_Op,
                     Source_1    => Item.Src_1.Element,
                     Source_2    => Item.Src_2.Element,
                     Destination => Item.Dst.Element);
                  Deallocate (Item.Src_1.Element);
                  Deallocate (Item.Src_2.Element);
                  Deallocate (Item.Dst.Element);
            end case;
         end loop;

         Arch.End_Routine
           (Ada.Strings.Unbounded.To_String (Routine.Name));
      end loop;

      Arch.End_Generation;

   end Generate;

   -----------
   -- Image --
   -----------

   function Image (Instruction : Instruction_Record) return String is
      use type Tagatha.Operands.Operand_Type;
      Img : constant String :=
              (case Instruction.T is
                  when Push =>
                    "push "
               & Tagatha.Operands.Image (Instruction.Operand.Element),
                  when Store =>
                    "store",
                  when Operator   =>
                    Instruction.Op'Image
               & (if Instruction.Op_Result.Element = Operands.No_Operand
                 then ""
                 else Tagatha.Operands.Image
                   (Instruction.Op_Result.Element)),
                  when Branch     =>
                    "br " & Instruction.Condition'Image & " "
               & Tagatha.Labels.Image (Instruction.Destination),
                  when Call       =>
                    "call/" & Ada.Strings.Fixed.Trim
                 (Instruction.Argument_Count'Image,
                  Ada.Strings.Left),
                  when Reserve    =>
                    "reserve/" & Ada.Strings.Fixed.Trim
                 (Instruction.Word_Count'Image,
                  Ada.Strings.Left),
                  when Transfer   =>
                    Tagatha.Operands.Image (Instruction.Dst.Element)
               & " <- "
               & Tagatha.Operands.Image (Instruction.Src_1.Element)
               & " "
               & Instruction.Transfer_Op'Image
               & " "
               & Tagatha.Operands.Image (Instruction.Src_2.Element));
   begin
      return Img;
   end Image;

   -----------
   -- Label --
   -----------

   procedure Label
     (This  : in out Instance;
      Label : Tagatha.Labels.Label)
   is
   begin
      This.Pending_Labels.Append (Label);
   end Label;

   ----------------
   -- Next_Label --
   ----------------

   function Next_Label
     (This : in out Instance)
      return Tagatha.Labels.Label
   is
   begin
      return This.Label_Source.Next;
   end Next_Label;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (This     : in out Instance;
      Operator : Tagatha_Operator;
      Result   : Operands.Operand_Type := Operands.No_Operand)
   is
   begin
      This.Append (Operate (Operator, Result));
   end Operate;

   ----------
   -- Push --
   ----------

   procedure Push
     (This    : in out Instance;
      Operand : Tagatha.Operands.Operand_Type)
   is
   begin
      This.Append (Push (Operand));
   end Push;

   -------------
   -- Reserve --
   -------------

   procedure Reserve
     (This       : in out Instance;
      Word_Count : Natural)
   is
   begin
      if Word_Count > 0 then
         This.Append (Reserve (Word_Count));
      end if;
   end Reserve;

   ---------------------
   -- Source_Location --
   ---------------------

   procedure Source_Location
     (This   : in out Instance;
      Line   : Positive;
      Column : Positive)
   is
   begin
      This.Current_Line := Line;
      This.Current_Column := Column;
   end Source_Location;

   -----------
   -- Store --
   -----------

   procedure Store (This : in out Instance) is
   begin
      This.Append (Store);
   end Store;

   -------------------
   -- Write_Listing --
   -------------------

   procedure Write_Listing
     (This : Instance;
      Path : String)
   is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Routine of This.Routines loop
         Put_Line (File, Ada.Strings.Unbounded.To_String (Routine.Name) & ":");
         for Item of Routine.Vector loop
            for Label of Item.Labels loop
               Put_Line (File, Tagatha.Labels.Image (Label) & ":");
            end loop;
            Put_Line (File, "        " & Image (Item));
         end loop;
      end loop;
      Close (File);
   end Write_Listing;

end Tagatha.Code;
