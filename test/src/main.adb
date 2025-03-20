--
--  Test various ANSI escape sequences
--
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with BBS.ANSI;

procedure Main is
   start : Natural := 2;
   s : Ada.Strings.Unbounded.Unbounded_String;
   rows : Natural;
   cols : Natural;
begin
   BBS.ANSI.getSize(rows, cols);
   Ada.Text_IO.Put(BBS.ANSI.cls & BBS.ANSI.red & "Hello world!" & BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(2, 2));
   for i in 1 .. cols - 2 loop
      Ada.Text_IO.Put("-");
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(rows + 25, 2));
   for i in 1 .. cols - 2 loop
      Ada.Text_IO.Put("-");
   end loop;
   for i in start .. rows loop
      Ada.Text_IO.Put(BBS.ANSI.PosCursor(i, 1) & '|' & BBS.ANSI.PosCursor(i, cols) & '|');
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(start, 1) & '+' & BBS.ANSI.PosCursor(start, cols) & '+');
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(rows, 1) & '+' & BBS.ANSI.PosCursor(rows, cols) & '+');
   Ada.Text_IO.Put_Line(BBS.ANSI.PosCursor(30,1) & '#' & BBS.ANSI.rst);
   Ada.Text_IO.Put_Line("Screen size is " & Natural'Image(cols) & " X" & Natural'Image(rows));
   Ada.Text_IO.Put(BBS.ANSI.reqAttr);
   declare
      s : constant String := BBS.ANSI.getCharOrEscape;
   begin
      Ada.Text_IO.Put_Line("Attribute sequence is: " & s);
   end;
end Main;
