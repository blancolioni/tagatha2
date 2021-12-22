private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Tagatha.Stacks is

   type Stack is tagged private;

   function Is_Empty (Container : Stack) return Boolean;

   function Top
     (Container : Stack)
      return Element_Type
     with Pre => not Container.Is_Empty;

   function Pop
     (Container : in out Stack)
      return Element_Type
     with Pre => not Container.Is_Empty;

   procedure Push
     (Container : in out Stack;
      Element   : Element_Type)
     with Post => not Container.Is_Empty;

   procedure Drop
     (Container : in out Stack;
      Count     : Natural := 1)
     with Pre => not Container.Is_Empty;

private

   package Stack_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type, "=");

   type Stack is new Stack_Lists.List with null record;

   overriding function Is_Empty (Container : Stack) return Boolean
   is (Stack_Lists.List (Container).Is_Empty);

end Tagatha.Stacks;
