package BBS.ANSI is
   --
   --  Recognized terminal types.  This may be extended as more types are recognized.
   --
   type term_type is (unknown, VT100, VT101, VT102, VT125, VT131, VT132, VT220,
                      VT240, VT320, VT330, VT340, VT382, VT420, VT510, VT520,
                      VT525);
   --
   --  Define constant escape sequences
   --
   esc : constant Character := Character'Val(27);
   csi : constant String := esc & '[';  --  Control Sequence Introducer
   dcs : constant String := esc & 'P';  --  Device Control String
   osc : constant String := esc & ']';  --  Operating System Command
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
   --  Colors
   --
   red     : constant String := csi & chBold & ";" & fgRed & chMode;
   blue    : constant String := csi & chBold & ";" & fgBlue & chMode;
   green   : constant String := csi & chBold & ";" & fgGreen & chMode;
   yellow  : constant String := csi & chBold & ";" & fgYellow & chMode;
   cyan    : constant String := csi & chBold & ";" & fgCyan & chMode;
   Magenta : constant String := csi & chBold & ";" & fgMagenta & chMode;
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
   --  Position cursor
   --
   function posCursor(Line, Column : Natural) return String;
   --
   --  Create a string that will draw a box on the screen
   --
   function drawBox(row, col, height, width : Natural) return String;
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
