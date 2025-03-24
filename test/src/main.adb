--
--  Test various ANSI escape sequences
--
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with BBS.ANSI;

procedure Main is
   start : Natural := 2;
   s : Ada.Strings.Unbounded.Unbounded_String;
   rows : Natural;
   cols : Natural;
   x, y1, y2 : Float;
begin
   BBS.ANSI.getSize(rows, cols);
   Ada.Text_IO.Put(BBS.ANSI.cls & BBS.ANSI.csi & BBS.ANSI.chBold & ';' & BBS.ANSI.chBlink &
                   ';' & BBS.ANSI.fgRed & ';' & BBS.ANSI.bgYellow & BBS.ANSI.chMode & "Hello world!" & BBS.ANSI.rst & BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.drawBox(2, 1, rows - 3, cols - 1));
   for i in 2 .. cols - 2 loop
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23, i) & BBS.ANSI.white & '-');
      x := Float(i)*0.1;
      y1 := Ada.Numerics.Elementary_Functions.Sin(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y1), i) & BBS.ANSI.blue & '*');
      y2 := Ada.Numerics.Elementary_Functions.Cos(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y2), i) & BBS.ANSI.yellow & '+');
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y1), 66 + Integer(y2)) & BBS.ANSI.cyan & "o");
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.drawBox(5, 30, 3, 10));
   Ada.Text_IO.Put(BBS.ANSI.posCursor(6, 31) & BBS.ANSI.blue & '*' & BBS.ANSI.white &
                     " Sin(x)");
   Ada.Text_IO.Put(BBS.ANSI.posCursor(7, 31) & BBS.ANSI.yellow & '*' & BBS.ANSI.white &
                     " Cos(x)");
--   Ada.Text_IO.Put_Line("Screen size is " & Natural'Image(cols) & " X" & Natural'Image(rows));
--   Ada.Text_IO.Put(BBS.ANSI.reqAttr);
--   declare
--      s : constant String := BBS.ANSI.getCharOrEscape;
--   begin
--      Ada.Text_IO.Put_Line("Attribute sequence is: " & s);
--   end;
   Ada.Text_IO.Put(BBS.ANSI.posCursor(rows - 1, 1) & BBS.ANSI.rst);
end Main;
