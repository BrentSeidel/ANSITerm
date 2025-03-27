--
--  Test various ANSI escape sequences
--
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with BBS.ANSI;
use type BBS.ANSI.term_type;

procedure Main is
   start : Natural := 2;
   s : Ada.Strings.Unbounded.Unbounded_String;
   rows : Natural;
   cols : Natural;
   x, y1, y2 : Float;
   kind : BBS.ANSI.term_type;
begin
   BBS.ANSI.getSize(rows, cols);
   Ada.Text_IO.Put_Line("Screen size is " & Natural'Image(cols) & " X" & Natural'Image(rows));
   kind := BBS.ANSI.identify;
   Ada.Text_IO.Put_Line("Found terminal type " & BBS.ANSI.term_type'Image(kind));
   Ada.Text_IO.Put_Line(BBS.ANSI.esc & ")0");
   Ada.Text_IO.Put_Line(BBS.ANSI.so & "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
   Ada.Text_IO.Put_Line(BBS.ANSI.si & "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
   Ada.Text_IO.Put("Press return to continue: ");
   declare
      dummy : String := Ada.Text_IO.Get_Line;
   begin
      null;
   end;
   Ada.Text_IO.Put(BBS.ANSI.cls & BBS.ANSI.csi & BBS.ANSI.chBold & ';' & BBS.ANSI.chBlink &
                   ';' & BBS.ANSI.fgRed & ';' & BBS.ANSI.bgYellow & BBS.ANSI.chMode & "Hello world!" & BBS.ANSI.rst & BBS.ANSI.white);
   if kind = BBS.ANSI.VT420 then
      Ada.Text_IO.Put(BBS.ANSI.csi & BBS.ANSI.bgBlack & BBS.ANSI.chMode);
   end if;
   Ada.Text_IO.Put(BBS.ANSI.drawBox(2, 1, rows - 3, cols - 1, True));
   Ada.Text_IO.Put(BBS.ANSI.so & BBS.ANSI.posCursor(23, 1) & BBS.ANSI.symLeftT & BBS.ANSI.posCursor(23, cols) &
                     BBS.ANSI.symRightT & BBS.ANSI.si);
   for i in 2 .. cols - 1 loop
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23, i) & BBS.ANSI.so & BBS.ANSI.white & BBS.ANSI.symHoriz & BBS.ANSI.si);
      x := Float(i)*0.1;
      y1 := Ada.Numerics.Elementary_Functions.Sin(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y1), i) & BBS.ANSI.blue & '*');
      y2 := Ada.Numerics.Elementary_Functions.Cos(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y2), i) & BBS.ANSI.yellow & '+');
      Ada.Text_IO.Put(BBS.ANSI.posCursor(23 - Integer(y1), 66 + Integer(y2)) & BBS.ANSI.cyan & "o");
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.drawBox(5, 30, 3, 10, True));
   Ada.Text_IO.Put(BBS.ANSI.posCursor(6, 31) & BBS.ANSI.blue & '*' & BBS.ANSI.white &
                     " Sin(x)");
   Ada.Text_IO.Put(BBS.ANSI.posCursor(7, 31) & BBS.ANSI.yellow & '*' & BBS.ANSI.white &
                     " Cos(x)");
   Ada.Text_IO.Put(BBS.ANSI.posCursor(rows - 1, 1) & BBS.ANSI.rst);
end Main;
