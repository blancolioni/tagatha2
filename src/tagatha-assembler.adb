with Ada.Text_IO;

package body Tagatha.Assembler is

   ------------
   -- Create --
   ------------

   function Create
     (Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
      return Assembler_Line
   is
   begin
      return Result : Assembler_Line do
         Put (Result, Instruction, Arg_1, Arg_2, Arg_3);
      end return;
   end Create;

   ---------
   -- Put --
   ---------

   procedure Put
     (This        : in out Assembler_Line;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
   is
      function Field
        (Text    : String;
         Comma   : Boolean)
         return String;

      -----------
      -- Field --
      -----------

      function Field
        (Text  : String;
         Comma : Boolean)
         return String
      is
      begin
         if Text = "" then
            return "";
         else
            return (if Comma then "," else "") & " " & Text;
         end if;
      end Field;

   begin
      This.Lines.Append
        (String'
           ("       "
            & Field (Instruction, False)
            & Field (Arg_1, False)
            & Field (Arg_2, True)
            & Field (Arg_3, True)));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This        : in out Instance;
      Instruction : String;
      Arg_1       : String := "";
      Arg_2       : String := "";
      Arg_3       : String := "")
   is
      function Field
        (Text    : String;
         Comma   : Boolean)
         return String;

      -----------
      -- Field --
      -----------

      function Field
        (Text  : String;
         Comma : Boolean)
         return String
      is
      begin
         if Text = "" then
            return "";
         else
            return (if Comma then "," else "") & " " & Text;
         end if;
      end Field;

   begin
      This.Put_Line
        ("       "
         & Field (Instruction, False)
         & Field (Arg_1, False)
         & Field (Arg_2, True)
         & Field (Arg_3, True));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This : in out Instance;
      Line : Assembler_Line)
   is
   begin
      for Item of Line.Lines loop
         This.Put_Line (Item);
      end loop;
   end Put;

   ---------------
   -- Put_Label --
   ---------------

   procedure Put_Label
     (This  : in out Any_Instance;
      Label : String)
   is
   begin
      This.Put_Line (Label & ":");
   end Put_Label;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : in out Any_Instance; Text : String) is
   begin
      This.Lines.Append (Text);
   end Put_Line;

   ----------
   -- Save --
   ----------

   procedure Save (This : Any_Instance; Path : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      for Line of This.Lines loop
         Put_Line (File, Line);
      end loop;
      Close (File);
   end Save;

end Tagatha.Assembler;
