package body Tagatha.Stacks is

   ----------
   -- Drop --
   ----------

   procedure Drop (Container : in out Stack; Count : Natural := 1) is
   begin
      for I in 1 .. Count loop
         Container.Delete_Last;
      end loop;
   end Drop;

   ---------
   -- Pop --
   ---------

   function Pop (Container : in out Stack) return Element_Type is
   begin
      return Element : constant Element_Type := Container.Last_Element do
         Container.Delete_Last;
      end return;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Container : in out Stack; Element : Element_Type) is
   begin
      Container.Append (Element);
   end Push;

   ---------
   -- Top --
   ---------

   function Top (Container : Stack) return Element_Type is
   begin
      return Container.Last_Element;
   end Top;

end Tagatha.Stacks;
