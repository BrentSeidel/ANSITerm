package body BBS.ANSI is

   --
   --  Position cursor
   --
   function PosCursor(Line, Column : Natural) return String is
      r : constant String := Natural'Image(Line);
      c : constant String := Natural'Image(Column);
   begin
      return csi & r(r'First + 1 .. r'Last) & ';' &
        c(c'First + 1 .. c'Last) & 'H';
   end;

end BBS.ANSI;
