private with Ada.Strings.Fixed;

package Tagatha.Labels is

   type Label is private;

   function Image (T : Label) return String;

   type Label_Source is tagged private;

   function Next (Source : in out Label_Source) return Label;

private

   type Label is
      record
         Index : Natural := 0;
      end record;

   type Label_Source is tagged
      record
         Next : Natural := 0;
      end record;

   function Image (T : Label) return String
   is (Ada.Strings.Fixed.Trim (T.Index'Image, Ada.Strings.Left));

end Tagatha.Labels;
