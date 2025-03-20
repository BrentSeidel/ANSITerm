--
--  Test various ANSI escape sequences
--
with Ada.Text_IO;
with BBS.ANSI;

procedure Main is
   start : Natural := 2;
begin
   Ada.Text_IO.Put(BBS.ANSI.cls & BBS.ANSI.red & "Hello world!" & BBS.ANSI.white);
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(start, 2));
   for i in 1 .. 78 loop
      Ada.Text_IO.Put("-");
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(start + 25, 2));
   for i in 1 .. 78 loop
      Ada.Text_IO.Put("-");
   end loop;
   for i in 1 .. 24 loop
      Ada.Text_IO.Put(BBS.ANSI.PosCursor(start + i, 1) & '|' & BBS.ANSI.PosCursor(start + i, 80) & '|');
   end loop;
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(start, 1) & '+' & BBS.ANSI.PosCursor(start, 80) & '+');
   Ada.Text_IO.Put(BBS.ANSI.PosCursor(start + 25, 1) & '+' & BBS.ANSI.PosCursor(start + 25, 80) & '+');
   Ada.Text_IO.Put_Line(BBS.ANSI.PosCursor(30,1) & '#' & BBS.ANSI.rst);
end Main;
