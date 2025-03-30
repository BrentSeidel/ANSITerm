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
   rMid : Natural;
   cMid : Natural;
   x, y1, y2 : Float;
   kind : BBS.ANSI.term_type;
begin
   BBS.ANSI.getSize(rows, cols);
   rMid := rows/2;
   cMid := cols/2;
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
   Ada.Text_IO.Put(BBS.ANSI.so & BBS.ANSI.posCursor(rMid, 1) & BBS.ANSI.symLeftT & BBS.ANSI.posCursor(rMid, cols) &
                     BBS.ANSI.symRightT);
   for i in 3 .. rows - 2 loop
      Ada.Text_IO.Put(BBS.ANSI.posCursor(i, cMid) & BBS.ANSI.white & BBS.ANSI.symVert);
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.posCursor(2, cMid) & BBS.ANSI.symUpperT & BBS.ANSI.posCursor(rows - 1, cMid) & BBS.ANSI.symLowerT);
   for i in 2 .. cols - 1 loop
      Ada.Text_IO.Put(BBS.ANSI.posCursor(rMid, i) & BBS.ANSI.so & BBS.ANSI.white & BBS.ANSI.symHoriz & BBS.ANSI.si);
      if i = cMid then
         Ada.Text_IO.Put(BBS.ANSI.posCursor(rMid, cMid) & BBS.ANSI.so & BBS.ANSI.symCross & BBS.ANSI.si);
      end if;
      x := Float(i - cMid)*0.1;
      y1 := Ada.Numerics.Elementary_Functions.Sin(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(rMid - Integer(y1), i) & BBS.ANSI.blue & '*');
      y2 := Ada.Numerics.Elementary_Functions.Cos(x)*20.0;
      Ada.Text_IO.Put(BBS.ANSI.posCursor(rMid - Integer(y2), i) & BBS.ANSI.yellow & '+');
      Ada.Text_IO.Put(BBS.ANSI.posCursor(rMid - Integer(y1), cMid + Integer(y2)) & BBS.ANSI.cyan & "o");
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.drawBox(5, 30, 3, 10, True));
   Ada.Text_IO.Put(BBS.ANSI.posCursor(6, 31) & BBS.ANSI.blue & '*' & BBS.ANSI.white &
                     " Sin(x)");
   Ada.Text_IO.Put(BBS.ANSI.posCursor(7, 31) & BBS.ANSI.yellow & '*' & BBS.ANSI.white &
                     " Cos(x)");
   delay 1.0;
   Ada.Text_IO.Put(BBS.ANSI.drawBox(10, 20, 24, 80, True));
   Ada.Text_IO.Put(BBS.ANSI.fillBox(10, 20, 24, 80, ' '));
   Ada.Text_IO.Put(BBS.ANSI.posCursor(rows - 1, 1) & BBS.ANSI.rst);
end Main;
