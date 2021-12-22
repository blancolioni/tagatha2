private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Tagatha.Assembler is

   type Instance is tagged private;
   subtype Any_Instance is Instance'Class;

   procedure Put_Line
     (This : in out Any_Instance;
      Text : String);

   procedure Put
     (This : in out Instance;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "");

   procedure Put_Label
     (This  : in out Any_Instance;
      Label : String);

   procedure Save
     (This : Any_Instance;
      Path : String);

   type Assembler_Line is private;

   function Is_Empty (Line : Assembler_Line) return Boolean;

   function Create
     (Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
      return Assembler_Line;

   procedure Put
     (This        : in out Assembler_Line;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "");

   procedure Put
     (This : in out Instance;
      Line : Assembler_Line);

private

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Instance is tagged
      record
         Lines : String_Lists.List;
      end record;

   type Assembler_Line is
      record
         Lines : String_Lists.List;
      end record;

   function Is_Empty (Line : Assembler_Line) return Boolean
   is (Line.Lines.Is_Empty);

end Tagatha.Assembler;
