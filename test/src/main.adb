--
--  Test various ANSI escape sequences
--
with Ada.Text_IO;
with BBS.ANSI;

procedure Main is
begin
   Ada.Text_IO.Put_Line(BBS.ANSI.red & "Hello world!" & BBS.ANSI.rst);
end Main;
