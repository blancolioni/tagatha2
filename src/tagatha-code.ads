private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

with Tagatha.Arch;
with Tagatha.Labels;
with Tagatha.Operands;

package Tagatha.Code is

   type Instance is tagged private;

   procedure Begin_Routine
     (This           : in out Instance;
      Name           : in     String;
      Argument_Words : in     Natural;
      Result_Words   : in     Natural;
      Global         : in     Boolean);

   procedure End_Routine
     (This           : in out Instance);

   procedure Source_Location
     (This   : in out Instance;
      Line   : Positive;
      Column : Positive);

   procedure Push
     (This    : in out Instance;
      Operand : Tagatha.Operands.Operand_Type);

   procedure Operate
     (This     : in out Instance;
      Operator : Tagatha_Operator;
      Result   : Operands.Operand_Type := Operands.No_Operand);

   procedure Index
     (This        : in out Instance;
      Word_Offset : Natural);

   procedure Store
     (This    : in out Instance);

   procedure Call
     (This           : in out Instance;
      Result         : Tagatha.Operands.Operand_Type;
      Argument_Count : Natural);

   procedure Branch
     (This        : in out Instance;
      Condition   : Tagatha_Condition;
      Destination : Tagatha.Labels.Label);

   procedure Reserve
     (This       : in out Instance;
      Word_Count : Natural);

   procedure Drop
     (This       : in out Instance;
      Word_Count : Natural);

   function Next_Label
     (This : in out Instance)
      return Tagatha.Labels.Label;

   procedure Label
     (This  : in out Instance;
      Label : Tagatha.Labels.Label);

   procedure Generate
     (This : in out Instance;
      Arch : in out Tagatha.Arch.Any_Instance);

   procedure Write_Listing
     (This : Instance;
      Path : String);

private

   package Local_Label_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Tagatha.Labels.Label, Tagatha.Labels."=");

   package Operand_Holders is
     new Ada.Containers.Indefinite_Holders
       (Tagatha.Operands.Operand_Type, Tagatha.Operands."=");

   type Instruction_Type is
     (Push, Store, Operator, Branch, Call, Reserve, Transfer);

   type Instruction_Record (T : Instruction_Type) is
      record
         Labels      : Local_Label_Lists.List;
         case T is
            when Push =>
               Operand           : Operand_Holders.Holder;
            when Store =>
               null;
            when Operator =>
               Op                : Tagatha_Operator;
               Op_Result         : Operand_Holders.Holder;
            when Branch =>
               Condition         : Tagatha_Condition;
               Destination       : Tagatha.Labels.Label;
            when Call =>
               Result            : Operand_Holders.Holder;
               Argument_Count    : Natural;
            when Reserve =>
               Word_Count        : Integer;
            when Transfer =>
               Src_1, Src_2, Dst : Operand_Holders.Holder;
               Transfer_Op       : Tagatha_Operator;
         end case;
      end record;

   function Image (Instruction : Instruction_Record) return String;

   function Push
     (Operand : Tagatha.Operands.Operand_Type)
      return Instruction_Record
   is (Push, Local_Label_Lists.Empty_List,
       Operand_Holders.To_Holder (Operand));

   function Store return Instruction_Record
   is (Store, Local_Label_Lists.Empty_List);

   function Operate
     (Op : Tagatha_Operator;
      Result : Tagatha.Operands.Operand_Type)
      return Instruction_Record
   is (Operator, Local_Label_Lists.Empty_List, Op,
       Operand_Holders.To_Holder (Result));

   function Branch (Condition : Tagatha_Condition;
                    Destination : Tagatha.Labels.Label)
                    return Instruction_Record
   is (Branch, Local_Label_Lists.Empty_List, Condition, Destination);

   function Call (Result : Operands.Operand_Type;
                  Argument_Count : Natural)
                  return Instruction_Record
   is (Call, Local_Label_Lists.Empty_List,
       Operand_Holders.To_Holder (Result), Argument_Count);

   function Reserve (Word_Count : Integer) return Instruction_Record
   is (Reserve, Local_Label_Lists.Empty_List, Word_Count);

   function Transfer
     (Dst : Operands.Operand_Type;
      Src_1 : Operands.Operand_Type;
      Src_2 : Operands.Operand_Type;
      Op    : Tagatha_Operator)
      return Instruction_Record
   is (Instruction_Record'
         (T              => Transfer,
          Labels         => <>,
          Src_1          => Operand_Holders.To_Holder (Src_1),
          Src_2          => Operand_Holders.To_Holder (Src_2),
          Dst            => Operand_Holders.To_Holder (Dst),
          Transfer_Op    => Op));

   package Instruction_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Instruction_Record);

   type Routine_Type is
      record
         Name           : Ada.Strings.Unbounded.Unbounded_String;
         Argument_Words : Natural;
         Result_Words   : Natural;
         Global         : Boolean;
         Vector         : Instruction_Vectors.Vector;
      end record;

   package Routine_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Routine_Type);

   type Instance is tagged
      record
         Label_Source   : Tagatha.Labels.Label_Source;
         Pending_Labels : Local_Label_Lists.List;
         Routines       : Routine_Lists.List;
         Active_Routine : Routine_Type;
         Current_Line   : Positive := 1;
         Current_Column : Positive := 1;
      end record;

   procedure Append
     (This : in out Instance;
      Item : Instruction_Record);

end Tagatha.Code;
