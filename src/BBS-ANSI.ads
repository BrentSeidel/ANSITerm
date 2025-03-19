package BBS.ANSI is
   --
   --  Define constant escape sequences
   --
   esc : constant Character := Character'Val(27);
   csi : constant String := esc & '[';
   --
   --  Colors
   --
   red   : constant String := csi & "1;31m";
   blue  : constant String := csi & "1;34m";
   green : constant String := csi & "1;32m";
   white : constant String := csi & "1;37m";
   --
   --  Reset formatting
   --
   rst : constant String := csi & "0m";
   --
   --  Home and clear screen
   --
   cls : constant String := csi & "H" & csi & "J";
   --
   --  Position cursor
   --
   function PosCursor(Line, Column : Natural) return String;

end BBS.ANSI;
