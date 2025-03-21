with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
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
   --
   --  Create a string that will draw a box on the screen
   --
   function drawBox(row, col, height, width : Natural) return String is
      s : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      s := s & posCursor(row, col + 1);
      for i in 1 .. width - 1 loop
         s := s & '-';
      end loop;
      s := s & posCursor(row + height, col + 1);
      for i in 1 .. width - 1 loop
         s := s & '-';
      end loop;
      for i in row + 1 .. row + height - 1 loop
         s := s & posCursor(i, col) & '|' & posCursor(i, col + width) & '|';
      end loop;
      s := s & posCursor(row, col) & '+' & posCursor(row, col + width) & '+';
      s := s & posCursor(row + height, col) & '+' & posCursor(row + height, col + width) & '+';
      return Ada.Strings.Unbounded.To_String(s);
   end;
   --
   --  Get character or escape sequence
   --
   --  This would be much more efficient if Ada.Text_IO.Get would return immediately
   --  if a character is present rather than waiting for a complete line to be
   --  entered.
   --
   function getCharOrEscape return String is
      c : Character;
      r : Boolean := False;
      s : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      while not r loop
         Ada.Text_IO.Get_Immediate(c, r);
      end loop;
      if Character'Pos(c) /= 27 then  --  Check if an escape character
         return c & "";               --  If not, just return it
      end if;
      loop
         r := False;
         while not r loop
            Ada.Text_IO.Get_Immediate(c, r);
         end loop;
         s := s & c;
         --
         --  Expand this list as more escape sequence terminators are recognized
         --
         exit when c = 'S';  --  Graphics attributes
         exit when c = 'c';  --  Device attributes
         exit when c = 'R';  --  Cursor position report
         exit when c = '~';  --  Function key
         exit when c = 'A';  --  Cursor up
         exit when c = 'B';  --  Cursor down
         exit when c = 'C';  --  Cursor right
         exit when c = 'D';  --  Cursor left
      end loop;
      return '>' &  Ada.Strings.Unbounded.To_String(s);
   end;
   --
   --  Get the size of the terminal window
   --
   procedure getSize(rows : out Natural; cols : out Natural) is
      s : Ada.Strings.Unbounded.Unbounded_String;
      r : Ada.Strings.Unbounded.Unbounded_String;
      c : Ada.Strings.Unbounded.Unbounded_String;
      i : Natural;
      l : Natural;
   begin
      Ada.Text_IO.Put(BBS.ANSI.posCursor(9999,9999));
      Ada.Text_IO.Put(BBS.ANSI.reqPos);
      declare
         t : constant String := BBS.ANSI.getCharOrEscape;
      begin
         s := Ada.Strings.Unbounded.To_Unbounded_String(t);
      end;
      if (Ada.Strings.Unbounded.Slice(s, 1, 2) = ">[") and
        (Ada.Strings.Unbounded.Slice(s, Ada.Strings.Unbounded.Length(s), Ada.Strings.Unbounded.Length(s)) = "R") then
         s := Ada.Strings.Unbounded.Unbounded_Slice(s, 3, Ada.Strings.Unbounded.Length(s) - 1);
         l := Ada.Strings.Unbounded.Length(s);
         i := Ada.Strings.Unbounded.Index(s, ";");
         declare
            r : constant String := Ada.Strings.Unbounded.Slice(s, 1, i - 1);
            c : constant String := Ada.Strings.Unbounded.Slice(s, i + 1, l);
         begin
            rows := Natural'Value(r);
            cols := Natural'Value(c);
         end;
      else
         rows := 0;
         cols := 0;
      end if;
   end;

end BBS.ANSI;
