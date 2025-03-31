package BBS.ANSI is
   --
   --  Recognized terminal types.  This may be extended as more types are recognized.
   --
   type term_type is (unknown, VT100, VT101, VT102, VT125, VT131, VT132, VT220,
                      VT240, VT320, VT330, VT340, VT382, VT420, VT510, VT520,
                      VT525);
   --
   --  Define single character control characters
   --
   bell : constant Character := Character'Val(7);   --  Ctrl-G ring bell
   bs   : constant Character := Character'Val(8);   --  Ctrl-H backspace
   tab  : constant Character := Character'Val(9);   --  Ctrl-I backspace
   lf   : constant Character := Character'Val(10);  --  Ctrl-J line feed
   cr   : constant Character := Character'Val(13);  --  Ctrl-M carriage return
   so   : constant Character := Character'Val(14);  --  Ctrl-N shift out
   si   : constant Character := Character'Val(15);  --  Ctrl-O shift in
   --
   --  Define constant escape sequences
   --
   esc : constant Character := Character'Val(27);
   csi : constant String := esc & '[';  --  Control Sequence Introducer
   dcs : constant String := esc & 'P';  --  Device Control String
   osc : constant String := esc & ']';  --  Operating System Command
   --
   --  Select character sets.  There are several more that are not supported by
   --  the VT100.  Some later terminals also support G2 and G3 character sets.
   --
   g0_uk  : constant String := esc & "(A";
   g0_us  : constant String := esc & "(B";
   g0_sym : constant String := esc & "(0";
   g1_uk  : constant String := esc & ")A";
   g1_us  : constant String := esc & ")B";
   g1_sym : constant String := esc & ")0";
   --
   --  Line drawing characters and symbols
   --
   symHoriz    : constant Character := 'q';
   symVert     : constant Character := 'x';
   symLeftT    : constant Character := 't';
   symRightT   : constant Character := 'u';
   symUpperT   : constant Character := 'w';
   symLowerT   : constant Character := 'v';
   symCornerUL : constant Character := 'l';
   symCornerUR : constant Character := 'k';
   symCornerLL : constant Character := 'm';
   symCornerLR : constant Character := 'j';
   symCross    : constant Character := 'n';
   symHT       : constant Character := 'b';
   symFF       : constant Character := 'c';
   symCR       : constant Character := 'd';
   symLF       : constant Character := 'e';
   symDegree   : constant Character := 'f';
   symPlusMin  : constant Character := 'g';
   symNL       : constant Character := 'h';
   symVT       : constant Character := 'i';
   symLE       : constant Character := 'y';
   symGE       : constant Character := 'z';
   symPi       : constant Character := '{';
   symNE       : constant Character := '|';
   --
   --  Character formatting codes
   --
   --  Mode character
   chMode : constant String := "m";
   --  Reset all formatting
   chReset   : constant String := "0";
   --  Set formatting
   chBold    : constant String := "1";
   chDim     : constant String := "2";
   chItalic  : constant String := "3";
   chUnderline : constant String := "4";
   chBlink   : constant String := "5";
   chInverse : constant String := "7";
   chHidden  : constant String := "8";
   chStrike  : constant String := "9";
   --  Clear formatting
   chNoBold    : constant String := "22";  --  "21" will probably also work
   chNoDim     : constant String := "22";
   chNoItalic  : constant String := "23";
   chNoUnderline : constant String := "24";
   chNoBlink   : constant String := "25";
   chNoInverse : constant String := "27";
   chNoHidden  : constant String := "28";
   chNoStrike  : constant String := "29";
   --  Foreground Color codes
   fgBlack   : constant String := "30";
   fgRed     : constant String := "31";
   fgGreen   : constant String := "32";
   fgYellow  : constant String := "33";
   fgBlue    : constant String := "34";
   fgMagenta : constant String := "35";
   fgCyan    : constant String := "36";
   fgWhite   : constant String := "37";
   fgDefault : constant String := "39";
   --  Background Color codes
   bgBlack   : constant String := "40";
   bgRed     : constant String := "41";
   bgGreen   : constant String := "42";
   bgYellow  : constant String := "43";
   bgBlue    : constant String := "44";
   bgMagenta : constant String := "45";
   bgCyan    : constant String := "46";
   bgWhite   : constant String := "47";
   bgDefault : constant String := "49";
   --
   --  Bright, 256 color, and RGB color sequences may be added here, though
   --  functions may be simpler.
   --
   --  Useful constant sequences
   --
   --
   --  Colors.  chBold is included to make the colors brighter.
   --
   red     : constant String := csi & chBold & ";" & fgRed & chMode;
   blue    : constant String := csi & chBold & ";" & fgBlue & chMode;
   green   : constant String := csi & chBold & ";" & fgGreen & chMode;
   yellow  : constant String := csi & chBold & ";" & fgYellow & chMode;
   cyan    : constant String := csi & chBold & ";" & fgCyan & chMode;
   magenta : constant String := csi & chBold & ";" & fgMagenta & chMode;
   white   : constant String := csi & chBold & ";" & fgWhite & chMode;
   --
   --  Reset formatting
   --
   rst : constant String := csi & "0m";
   --
   --  Home and clear screen
   --
   cls : constant String := csi & "H" & csi & "J";
   --
   --  Request cursor position
   --
   reqPos : constant String := csi & "6n";
   --
   --  Request device attributes
   --
   reqAttr : constant String := csi & "0c";
   --
   --  Functions and procedures.
   --  The functions generally use strings that can be put to the terminal or saved
   --  for later use.
   --
   --  Position cursor
   --
   function posCursor(Line, Column : Natural) return String;
   --
   --  Create a string that will draw a box on the screen.  If line is true, it
   --  will use the DEC line drawing characters.
   --
   function drawBox(row, col, height, width : Natural; line : Boolean) return String;
   --
   --  Fill a box with a specific character
   --
   function fillBox(row, col, height, width : Natural; c : Character) return String;
   --
   --  Get character or escape sequence
   --
   function getCharOrEscape(d : Duration) return String;
   --
   --  Get the size of the terminal window.  Sets rows and cols to 0 if not
   --  successful.
   --
   procedure getSize(rows : out Natural; cols : out Natural);
   --
   --  Try and identify the terminal
   --
   function identify return term_type;

end BBS.ANSI;
