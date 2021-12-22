package body Tagatha.Labels is

   ----------
   -- Next --
   ----------

   function Next (Source : in out Label_Source) return Label is
   begin
      Source.Next := Source.Next + 1;
      return Label'(Index => Source.Next);
   end Next;

end Tagatha.Labels;
