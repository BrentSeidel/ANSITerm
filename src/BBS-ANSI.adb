with Ada.Calendar;
use type Ada.Calendar.Time;
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
   function drawBox(row, col, height, width : Natural; line : Boolean) return String is
      s : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      s := s & posCursor(row, col + 1);
      if line then
         s := s & so;
         for i in 1 .. width - 1 loop
            s := s & symHoriz;
         end loop;
         s := s & posCursor(row + height, col + 1);
         for i in 1 .. width - 1 loop
            s := s & symHoriz;
         end loop;
         for i in row + 1 .. row + height - 1 loop
            s := s & posCursor(i, col) & symVert & posCursor(i, col + width) & symVert;
         end loop;
         s := s & posCursor(row, col) & symCornerUL & posCursor(row, col + width) & symCornerUR;
         s := s & posCursor(row + height, col) & symCornerLL & posCursor(row + height, col + width) & symCornerLR;
         s := s & si;
      else
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
      end if;
      return Ada.Strings.Unbounded.To_String(s);
   end;
   --
   --  Get character or escape sequence
   --
   --  This would be much more efficient if Ada.Text_IO.Get would return immediately
   --  if a character is present rather than waiting for a complete line to be
   --  entered.
   --
   function getCharOrEscape(d : Duration) return String is
      c : Character;
      r : Boolean := False;
      s : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      limit : constant Ada.Calendar.Time := Ada.Calendar.Clock + d;
   begin

      while not r loop
         Ada.Text_IO.Get_Immediate(c, r);
         if Ada.Calendar.Clock > limit then
            return "";
         end if;
      end loop;
      if Character'Pos(c) /= 27 then  --  Check if an escape character
         return c & "";               --  If not, just return it
      end if;
      loop
         r := False;
         while not r loop
            Ada.Text_IO.Get_Immediate(c, r);
         if Ada.Calendar.Clock > limit then
            return "";
         end if;
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
      Ada.Text_IO.Put(posCursor(9999,9999));
      Ada.Text_IO.Put(reqPos);
      declare
         t : constant String := getCharOrEscape(1.0);
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
   --
   --  Try and identify the terminal
   --
   --  PDA is primary device attributes.  Return is
   --  CSI ? n1 ; n2 c
   --    n1 - Is device type
   --    n2 - Is (possible multiple) device attributes.
   --
   --  CSI ? 1 ; 0 c  ("VT101 with No Options")
   --  CSI ? 1 ; 2 c  ("VT100 with Advanced Video Option")
   --  CSI ? 4 ; 6 c  ("VT132 with Advanced Video and Graphics")
   --  CSI ? 6 c  ("VT102")
   --  CSI ? 7 c  ("VT131")
   --  CSI ? 12 ; Ps c  ("VT125")
   --  CSI ? 62 ; Ps c  ("VT220")
   --  CSI ? 63 ; Ps c  ("VT320")
   --  CSI ? 64 ; Ps c  ("VT420")
   --  CSI ? 65 ; Ps c  ("VT510" to ("VT525")
   --
   --  SDA is secondary device attributes.  Return is
   --  CSI > n1 ; n2 ; n3 c
   --    n1 - Is device type
   --    n2 - Firmware version
   --    n3 - ROM cartridge registration number (always zero)
   --
   --   n1 = 0  => "VT100".
   --   n1 = 1  => "VT220".
   --   n1 = 2  => "VT240" or "VT241".
   --   n1 = 18 => "VT330".
   --   n1 = 19 => "VT340".
   --   n1 = 24 => "VT320".
   --   n1 = 32 => "VT382".
   --   n1 = 41 => "VT420".
   --   n1 = 61 => "VT510".
   --   n1 = 64 => "VT520".
   --   n1 = 65 => "VT525".
   --
   function identify return term_type is
      pda : constant String := CSI & "0c";
      sda : constant String := CSI & ">0c";
      s : Ada.Strings.Unbounded.Unbounded_String;
      r : Ada.Strings.Unbounded.Unbounded_String;
      i : Natural;
      l : Natural;
   begin
      Ada.Text_IO.Put(pda);
      s := Ada.Strings.Unbounded.To_Unbounded_String(getCharOrEscape(1.0));
      if (Ada.Strings.Unbounded.Slice(s, 1, 3) = ">[?") and
        (Ada.Strings.Unbounded.Slice(s, Ada.Strings.Unbounded.Length(s), Ada.Strings.Unbounded.Length(s)) = "c") then
         s := Ada.Strings.Unbounded.Unbounded_Slice(s, 4, Ada.Strings.Unbounded.Length(s) - 1);
         l := Ada.Strings.Unbounded.Length(s);
         i := Ada.Strings.Unbounded.Index(s, ";");
         if i = 0 then  --  Some codes do not have an additional parameter
            i := Ada.Strings.Unbounded.length(s);
         end if;
      else
         return unknown;
      end if;
      declare
         r : constant String := Ada.Strings.Unbounded.Slice(s, 1, i - 1);
         c : constant String := Ada.Strings.Unbounded.Slice(s, i + 1, l);
      begin
         if r = "1" and c = "0" then
            return VT101;
         elsif r = "1" and c = "2" then
            return VT100;
         elsif r = "4" then
            return VT132;
         elsif r = "6" then
            return VT102;
         elsif r = "7" then
            return VT131;
         elsif r = "12" then
            return VT125;
         elsif r = "62" then
            return VT220;
         elsif r = "63" then
            return VT320;
         elsif r = "64" then
            return VT420;
         end if;  --  r = "65" is VT510 to VT525, so decode using secondary attributes
      end;
      --
      --  Query secondary attributes if still unknown.  Note that some of the
      --  terminal types detected below should be detected above.
      --
      Ada.Text_IO.Put(sda);
      s := Ada.Strings.Unbounded.To_Unbounded_String(getCharOrEscape(1.0));
      if (Ada.Strings.Unbounded.Slice(s, 1, 3) = ">[>") and
        (Ada.Strings.Unbounded.Slice(s, Ada.Strings.Unbounded.Length(s), Ada.Strings.Unbounded.Length(s)) = "c") then
         s := Ada.Strings.Unbounded.Unbounded_Slice(s, 4, Ada.Strings.Unbounded.Length(s) - 1);
         l := Ada.Strings.Unbounded.Length(s);
         i := Ada.Strings.Unbounded.Index(s, ";");
      else
         return unknown;
      end if;
      declare
         r : constant String := Ada.Strings.Unbounded.Slice(s, 1, i - 1);
         c : constant String := Ada.Strings.Unbounded.Slice(s, i + 1, l);
      begin
--         Ada.Text_IO.Put_Line("Secondary terminal type code <" & r & ">");
         if r = "0" then
            return VT100;  --  Probably can't happen
         elsif r = "1" then
            return VT220;  --  Probably can't happen
         elsif r = "2" then
            return VT240;
         elsif r = "18" then
            return VT330;
         elsif r = "19" then
            return VT340;
         elsif r = "24" then
            return VT320;  --  Probably can't happen
         elsif r = "32" then
            return VT382;
         elsif r = "41" then
            return VT420;  --  Probably can't happen
         elsif r = "61" then
            return VT510;
         elsif r = "64" then
            return VT520;
         elsif r = "65" then
            return VT525;
         end if;
      end;
      return unknown;
   end;

end BBS.ANSI;
