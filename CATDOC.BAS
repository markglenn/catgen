''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
DEFINT A-Z

'Mouse Stuff
DECLARE FUNCTION ThereIsAMouse% ()
DECLARE FUNCTION GetMouseStatus% (MPosX%, MPosY%)
DECLARE FUNCTION MouseReset% ()
DECLARE SUB MouseHandler (ax%, bx%, cx%, dx%)
DECLARE SUB MouseOn ()
DECLARE SUB MouseOff ()

'CatGen Stuff
DECLARE SUB DB ()
DECLARE SUB DrawBottom ()
DECLARE SUB DrawLine (Kind)
DECLARE SUB InitMouse ()
DECLARE SUB InitHelp ()
DECLARE SUB OnlineHelp ()  '<--- The whole program
DECLARE SUB P (TheColor, st$)
DECLARE SUB PrintDocument ()  ' <-- Not implemented yet
DECLARE SUB Search ()
DECLARE SUB ScrollBar (Position%, Total%)
DECLARE SUB DrawEnding ()

DECLARE FUNCTION Erase$ (TheStr$)
DECLARE FUNCTION GetKey$ ()

DIM SHARED MouseVisible, MHMax, MVMax, Mouseinit
DIM SHARED MouseIntr, MHCell, MVCell, DoneIt
DIM SHARED Text$(272)
DIM SHARED ColorVal(272)
DIM SHARED num, wid, LastPos, UseMouse, Start

TYPE RegType       'Typedef for DOS registers
	ax  AS INTEGER
	bx  AS INTEGER
	cx  AS INTEGER
	dx  AS INTEGER
	bp  AS INTEGER
	si  AS INTEGER
	di  AS INTEGER
	flg AS INTEGER
END TYPE

COMMON SHARED /QBMouse/ True, False
CLS
True = -1: False = 0: SWidth = 80

'Mouse button press definitions

	PrL = 1: PrR = 2: PrLr = 3: PrM = 4
	PrLM = 5: PrMR = 6: PrAll = 7: PrNone = 0

'Button definitions

	ButtonLeft = 0
	ButtonRight = 1
	ButtonMiddle = 2

' Full use of the program with 4 lines of code

' >>>>>>>>  START HERE  <<<<<<<<<

DIM SHARED Inregs AS RegType, Outregs AS RegType
DIM SHARED dos%(45)                    'get some memory space
DEF SEG = VARSEG(dos%(0))
RESTORE ASM
FOR i% = 0 TO 88
	READ d%
	POKE VARPTR(dos%(0)) + i%, d%'copy code into memory
NEXT i%

UseMouse = True ' Change to False to not use a mouse (Scans for a mouse too)
					 ' and don't touch anything else.

InitMouse       ' Initialize the mouse
OnlineHelp      ' Command for the viewer

MouseOff        ' Turn mouse off

' >>>>>>>>  END HERE  <<<<<<<<<
DrawEnding
SYSTEM
Help:
DATA 272
DATA "08                        ___ ___ ___________ ___ ___"
DATA "08                       Y   Y   Y   _   _   Y   Y   |"
DATA "08                       |   l   l___|   |___|   l   |"
DATA "08                       l____   |   |   |   |   _   |"
DATA "07                           |   |   |   |   |   |   |"
DATA "07                           l___|   |   l   l___|   |"
DATA "07                                   `---'       `---'"
DATA "07      ______   ___ ___ ___ _______ ______  _______ ___ _______ ______"
DATA "07     Y   _  \ Y   Y   Y   Y   _   Y   _  \Y   _   Y   Y   _   Y   _  \"
DATA "07     |   |   \|   |       |   l___|   |   |   l___l   |   |   |   |   |"
DATA "0F     |   |    \   |  \_/  |   __)_    |   l____   |   |   |   |   |   |"
DATA "0F     |   l    /   |   |   |   l   |   |   |   l   |   |   l   |   |   |"
DATA "0F     l_______/|   l___|   l_______l___|   l_______|   l_______l___|   l"
DATA "0F              `---'   `---'           `---'       `---'           `---'"
DATA "0F     _______ _______ _______ ___________ ___ ___ _______ _______ _______"
DATA "07    Y   _   Y   _   Y   _   Y   _   _   Y   Y   Y   _   Y   _   \   _   Y"
DATA "07    |   l___l   |   |   l___l___|   |___l   |   |   l   |   l   /   l___|"
DATA "07    l____   |   |   |   __)     |   |   |  / \  |   _   |   _   l   __)_"
DATA "08    |   l   |   l   |   |       |   |   |       |   |   |   |   |   l   |"
DATA "08    |_______|_______l   |       |   l   l___l___l___|   l___|   l_______|"
DATA "08                    `---'       `---'               `---'   `---'"
DATA "0F           CatGen ~04v~093~04.~090 ~04(~0FC~04) ~0F1997 ~0E4th Dimension Software, ~0AMark Glenn"
DATA "03þLINE1"
DATA "03                             þBUTTON0250"
DATA "04    INTRODUCTION"
DATA "01    ------------"
DATA "03"
DATA "0F    CatGen ~04v~093~04.~090 ~03is a program to allow programmers of QB or QBASIC to have a"
DATA "03    useful file to BAS converter."
DATA "03"
DATA "03    Only compiling is required to create these catalogs."
DATA "03"
DATA "04    REQUIREMENTS"
DATA "01    ------------"
DATA "02      1~09) ~0FColor Monitor"
DATA "02      2~09) ~0FQB or QBasic"
DATA "02      3~09) ~0FHard drive"
DATA "02      4~09) ~0FQB for compilation"
DATA "02      5~09) ~0FKeyboard"
DATA "03"
DATA "04    FEATURES included"
DATA "01    -----------------"
DATA "02      1~09) ~0FColor capability"
DATA "02      2~09) ~0FSpeed (YEAH SPEEEEEED)"
DATA "02      3~09) ~0F5000 line maximum"
DATA "02      4~09) ~0FScrolling feature"
DATA "02      5~09) ~0FConvert TEXT to BAS to be compiled"
DATA "02      6~09) ~0FSearch Feature"
DATA "02      7~09) ~0FMulti colored lines"
DATA "02      8~09) ~0FBackground colors"
DATA "02      9~09) ~0FNo flickering"
DATA "02     10~09) ~0FOne file only (Doc inside BAS file)"
DATA "02     11~09) ~0FMouse (even in QBasic)"
DATA "02     12~09) ~0FScroll Bar"
DATA "02     13~09) ~0FFast Code"
DATA "02     14~09) ~0FButtons"
DATA "02     15~09) ~0FPreprogrammed lines"
DATA "03"
DATA "04    TABLE OF CONTENTS"
DATA "01    -----------------"
DATA "02      1~09) ~0FWhat is CatGen v3.0"
DATA "02      2~09) ~0FWhat is included"
DATA "02      3~09) ~0FCommand line perimeters"
DATA "02      4~09) ~0FWhat is it doing while running."
DATA "02      5~09) ~0FHow does it do all this"
DATA "02      6~09) ~0FI've run it, now what?"
DATA "02      7~09) ~0FColor"
DATA "02      8~09) ~0FModifying"
DATA "02      9~09) ~0FHow do I use the actual compiled work."
DATA "02     10~09) ~0FFinal touches"
DATA "03     "
DATA "04    APPENDIXES"
DATA "01    ----------"
DATA "02      A~09) ~0FTrouble Shooting"
DATA "02      B~09) ~0FUpcoming Features"
DATA "02      C~09) ~0FFeatures of each version."
DATA "02      D~09) ~0FInformation about our company"
DATA "02      E~09) ~0FOther programs"
DATA "03"
DATA "04  1~02) ~0FWhat is CatGen v3.0?"
DATA "01     --------------------"
DATA "0F     CatGen ~04v~093~04.~090 ~03is a program that takes your TEXT files and converts them"
DATA "03  into self running file viewers.  Not only that, but it is in color. "
DATA "03  Believe it or not, this file was created with ~0FCatGen ~04v~093~04.~090~03.  It's"
DATA "03  capabilities are outrages.  It has absolutely no flicker and has incredible"
DATA "03  speed.  It is based on 25x80 screen size and will look quite weird in 50x80."
DATA "03  It has scrolling features that will be used by using the up, down, PGUP, PGDN"
DATA "03  keys.  The best feature though is that you can distribute your file."
DATA "03"
DATA "04  2~02) ~0FWhat is included?"
DATA "01     -----------------"
DATA "03     CatGen comes with 9 files"
DATA "02      1~04: ~0FCATGEN.EXE   ~03- The catalog generator"
DATA "02      2~04: ~0FCATDOC.BAS   ~03- This file source code"
DATA "02      3~04: ~0FCATDOC.EXE   ~03- This file"
DATA "02      4~04: ~0FCATGEN.DOC   ~03- This file in TEXT format"
DATA "02      5~04: ~0FORDER.FRM    ~03- Order and Registration file for other programs"
DATA "02      6~04: ~0FFILE_ID.DIZ  ~03- File Info"
DATA "02      7~04: ~0FCATALOG.EXE  ~03- Catalog for Inventive Software"
DATA "02      8~04: ~0FANSI2CAT.EXE ~03- Ansi converter"
DATA "02      9~04: ~0FFONTSET.TXT  ~03- ASCII Fonts"
DATA "03"
DATA "04  3~02) ~0FCommand line perimeters"
DATA "01     -----------------------"
DATA "03     There really is only one command line perimeter."
DATA "0F       : CATGEN Infile.ext (Outfile.ext)"
DATA "03        The Infile.exe must be include.  Outfile.ext is something that is"
DATA "03        optional.  If you decide not to use this, it will output to CAT1.OUT."
DATA "03"
DATA "04  4~02) ~0FWhat is it doing while running?"
DATA "01     -------------------------------"
DATA "03     It does multiple things.  It first creates a temp file called TEMP1.TMP"
DATA "03  that holds your DOC with the DATA statements in front of it.  Then it goes"
DATA "03  in and loads all the BASIC statements to run it.  A file is created called"
DATA "03  (filename) and you have a finished product ready to compile."
DATA "03"
DATA "04  5~02) ~0FHow does it do all this?"
DATA "01     ------------------------"
DATA "03     It takes your file and process it and creates BASIC text.  Take a look at"
DATA "03  the file sometime and see how it is really done."
DATA "03"
DATA "04  6~02) ~0FI've run it, now what?"
DATA "01     ----------------------"
DATA "03     Okay, you have created your file and now it is time to modify.  Look at"
DATA "03  Chapter 8 for that.  Chapter 7 shows how to change the color of each line."
DATA "03  After that, you want to compile it.  Load the program into QB by typing this:"
DATA "0F    QB filename /L QB.QLB   or"
DATA "0F    QBASIC filename"
DATA "03  Now go to run and compile.  Make it Stand Alone EXE and click on Make EXE"
DATA "03  and Exit.  If you only have qbasic, do the following.  Create a file called"
DATA "03  filename.BAT and edit it."
DATA "0F  @ECHO OFF"
DATA "0F  QBASIC filename /RUN"
DATA "03  "
DATA "04  7~02) ~0FColor"
DATA "01     -----"
DATA "03     Wow, I can change color?  Yep, you can.  If you look at the end, you will"
DATA "03  see that is says the DATA statement and then the numbers '03'.  This is the"
DATA "03  color that it changes it all to.  You can change this number to anything."
DATA "03  It must be two digits long like:  03 or C5.  In 2.0, the numbers were"
DATA "03  changed to hex so you can now do backgrounds.  The colors are as following:"
DATA "00  00) BLACK       ~0808) GREY             ~2010) BLACK ON BLUE~03"
DATA "01  01) BLUE        ~0909) LIGHT BLUE       ~EFEF) BRIGHT WHITE ON WHITE~03"
DATA "02  02) GREEN       ~0A0A) LIGHT GREEN"
DATA "03  03) CYAN        ~0B0B) LIGHT CYAN"
DATA "04  04) RED         ~0C0C) LIGHT RED"
DATA "05  05) MAGENTA     ~0D0D) LIGHT MAGENTA"
DATA "06  06) BROWN       ~0E0E) YELLOW"
DATA "07  07) WHITE       ~0F0F) BRIGHT WHITE"
DATA "03"
DATA "04  8~02) ~0FModifying"
DATA "01     ---------"
DATA "03     For people who want to make a big logo on the doc, I included a ANSI to"
DATA "03  CATGEN converter.  All you do is add it to your doc and it will look exactly"
DATA "03  like the ansi file.  (Sorry, no animations and only 25 lines long)."
DATA "03    Also, you can add buttons and lines:"
DATA "03                            þBUTTON0001"
DATA "03    CHR$(254) + BUTTON0001  (4 digit line number)"
DATA "03    þLINE1"
DATA "03    CHR$(254) + LINE1 (1 digit line type)"
DATA "03"
DATA "03"
DATA "04  9~02) ~0FHow do I use the actual compiled work?"
DATA "01     --------------------------------------"
DATA "03     Well, you first run the program.  You see the file and information at the"
DATA "03  bottom.  You use the arrow keys or the PGUP or PGDN key.  These move the DOC"
DATA "03  up or down.  The new feature is the ÿSÿ feature.  It searches the file for"
DATA "03  whatever you want."
DATA "03"
DATA "04 10~02) ~0FFinal Touches"
DATA "01     -------------"
DATA "03     Since I don't really know any final touches, but all great DOCs have this"
DATA "03   area.  You can modify the generated file all you want.  The generated viewer"
DATA "03   is freeware and maybe modified and distributed without permission.  Think of"
DATA "03   it as your program you and only you made."
DATA "03"
DATA "04  A~02) ~0FTrouble Shooting"
DATA "01     ----------------"
DATA "04     1) While compiling, it says an out of memory error"
DATA "02       A:Try freeing more memory.  If you don't want to do this, if you look"
DATA "02          at the begining of the file, it shows DIM statements.  Change this"
DATA "02          to the length of your file.  If you don't know the length, look at"
DATA "02          the first DATA statement.  It will say something like this:"
DATA "02           DATA 253"
DATA "02          Change the DIM statements to:"
DATA "02           DIM text$(255) 'add 2 just in case"
DATA "02           DIM ColorVal(255) 'add 2 just in case"
DATA "02           You shouln't have to do this because it is automatic now."
DATA "04     2) While compiling, it says an error"
DATA "02       A:Try trouble shooting it.  If that doesn't work, re-generate the file."
DATA "02          if that still doesn't work, contact me about it."
DATA "04     3) Doesn't generate"
DATA "02       A:You have a corrupted CatGen v3.0.  Please reinstall.  You may also"
DATA "02          have a bad output file.  DOS only allows certain characters in"
DATA "02          filenames.  Try a filename with only letters:"
DATA "02           CATGEN MY.DOC MYDOC.BAS"
DATA "04     4) Bombs"
DATA "02       A:You have a corrupted CatGen v3.0.  Please reinstall."
DATA "04     5) Subscript out of range"
DATA "02       A:Your file is more than your memory can handle.  Cut it down."
DATA "04     6) Unexpected error, please recompile"
DATA "02       A:For some odd reason, your DOC got edited so it can not be used.  You"
DATA "02         might have edited it and messed it up.  If you know what you are"
DATA "02         doing, take out the ON ERROR statement and try to debug it.  If not,"
DATA "02         re compile it.  If the same error occurs, contact me."
DATA "04     7) I lost my cursor."
DATA "02       A:When the Help is invoked, it turns off the cursor.  It doesn't just"
DATA "02         disappear.  Just turn it back on:"
DATA "02         LOCATE , , 1, 7, 8"
DATA "04     8) I don't have a mouse and start-up is sooo slow."
DATA "02        You can easily turn off the mouse.  Look for the lines that say"
DATA "02        >>>>>> START HERE <<<<<<"
DATA "04     9) I edit the basic file and it doesn't have any ÿ's in the DOC.  How"
DATA "04        do I add them?"
DATA "02       A:Well, since DATA statements are surrounded by ÿ's, all ÿ's in your"
DATA "02         DOC were converted to character 255's.  To add ÿ's, go to the "
DATA "02         position where you want them and hold down ALT.  Then press 255 on the"
DATA "02         key pad and let go of ALT.  It is an invisible character, but it is "
DATA "02         there."
DATA "03"
DATA "04  B~02) ~0FUpcoming Features"
DATA "01     -----------------"
DATA "03        Editor"
DATA "03        Multicolored search"
DATA "03"
DATA "04  C~02) ~0FFeatures of Each Version"
DATA "01     ------------------------"
DATA "03      v1.0b - The first official version.  Never distributed."
DATA "03"
DATA "03      v1.0 - Added color feature.  PGUP and PGDN keys added.  Fixed"
DATA "03             Arrow key bug.  Finally distributed"
DATA "03"
DATA "03      v1.1 - Search feature added.  Also added the ON ERROR function so"
DATA "03             (Just in case) if it has an error it won't show that ugly error"
DATA "03             screen.  Added the registration file(oops, sorry to everyone"
DATA "03             with version 1.0)."
DATA "03"
DATA "03      v1.2 - Added 'S' at the bottom.  Fixed the scrolling up one line on"
DATA "03             search bug.  Fixed bottom text on no search bug.  Man did I have"
DATA "03             more trouble with that search feature than anything else.  Mainly"
DATA "03             a bug fix version."
DATA "03"
DATA "03      v1.3 - This would have been the one with EMS support but found it would"
DATA "03             only work in QuickBasic without rewriting the whole code.  Maybe"
DATA "03             later."
DATA "03"
DATA "03      v1.4 - Added online help support.  With this new option, you won't see"
DATA "03             all the ugly code because it has been put in SUBS.  Also I added"
DATA "03             a change in the DIM statements.  No need to change them this time"
DATA "03             because it automaticly changes them.  Also added limited edition"
DATA "03             source code."
DATA "03"
DATA "03      v2.0 - Limited edition source code taken away because of people without"
DATA "03             respect.  Scrolls sideways.  Multiple colored lines.  Background"
DATA "03             colors.  Improved search.  Now only supports HELP programs "
DATA "03             because the two options were identical except HELP was cleaner."
DATA "03"
DATA "0F      v3.0~03 - New and improved version.  CatGen is now 15x faster, cleaner code"
DATA "03             and mouse/scroll bar/buttons.  Also includes preprogrammed lines"
DATA "03             and buttons to press.  Better search, ANSI converter, and ASCII"
DATA "03             fonts document."
DATA "03  "
DATA "04  D~02) ~0FInformation about our company"
DATA "01     -----------------------------"
DATA "03    As of the summer of 1996, our company changed its name to:"
DATA "08            4~07t~0Fh Dimension Softwa~07r~08e"
DATA "03    If you see any programs with the company Inventive software or 4th Dim."
DATA "03    then you can feel safe that they are from our company."
DATA "03"
DATA "12    Please visit us at out web-site."
DATA "0D    http://4ds.simplenet.com/"
DATA "03    and click on the big 4th Dimension image."

ASM:
'       DOS call code
DATA &H55, &H06, &H1E, &H8B, &HEC, &H9C, &H8B, &H7E, &H0E, &H8A
DATA &H05, &H8B, &H7E, &H0C, &HB4, &H35, &HCD, &H21, &H8B, &H46
DATA &HF8, &H05, &H20, &H00, &H0E, &H50, &H06, &H53, &H8B, &H05
DATA &H8B, &H5D, &H02, &H8B, &H4D, &H04, &H8B, &H55, &H06, &H8B
DATA &H6D, &H08, &H8B, &H75, &H0A, &H8B, &H7D, &H0C, &HFA, &HCB
DATA &H1F, &H07, &H57, &H9C, &H8B, &HFC, &H36, &H8B, &H7D, &H0A
DATA &H89, &H05, &H89, &H5D, &H02, &H89, &H4D, &H04, &H89, &H55
DATA &H06, &H89, &H6D, &H08, &H89, &H75, &H0A, &H58, &H89, &H45
DATA &H0E, &H58, &H89, &H45, &H0C, &H5D, &HCA, &H06, &H00

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                          Draws a button quickly                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'15 long (&BTTNXXXX)
SUB DB
P 8, ">": P 7, ">": P 15, "> ": P 15, "Click Here": P 15, " <": P 7, "<": P 8, "<"
END SUB

DEFSNG A-Z
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                 Draws the lines on the bottom of the screen              ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB DrawBottom
LOCATE 24, 1
P 8, "ÄÄ": P 7, "ÄÄ": P 15, STRING$(72, 196): P 7, "ÄÄ": P 8, "ÄÄ"
LOCATE 25, 1
PRINT SPACE$(80); : LOCATE 25, 1
P 10, "CatGen v3.0"
LOCATE 25, 29
P 8, "4": P 7, "t"
P 15, "h Dimension Softwa": P 7, "r": P 8, "e"
PRINT "         ";
P 4, "(": P 10, CHR$(24) + CHR$(25): P 4, ") ("
P 10, "S": P 4, ")": P 9, "earch ": P 4, "(": P 10, "P": P 4, ")"
P 9, "rint"
COLOR 0, 7
LOCATE 1, 80: PRINT CHR$(24);
FOR a = 2 TO 22
LOCATE a, 80: PRINT "°";
NEXT
LOCATE 23, 80: PRINT CHR$(25);
COLOR 7, 0

END SUB

DEFINT A-Z
SUB DrawEnding
CLS
COLOR 8: PRINT "                        ___ ___ ___________ ___ ___"
COLOR 8: PRINT "                       Y   Y   Y   _   _   Y   Y   |"
COLOR 8: PRINT "                       |   l   l___|   |___|   l   |"
COLOR 8: PRINT "                       l____   |   |   |   |   _   |"
COLOR 7: PRINT "                           |   |   |   |   |   |   |"
COLOR 7: PRINT "                           l___|   |   l   l___|   |"
COLOR 7: PRINT "                                   `---'       `---'"
COLOR 7: PRINT "      ______   ___ ___ ___ _______ ______  _______ ___ _______ ______"
COLOR 7: PRINT "     Y   _  \ Y   Y   Y   Y   _   Y   _  \Y   _   Y   Y   _   Y   _  \"
COLOR 7: PRINT "     |   |   \|   |       |   l___|   |   |   l___l   |   |   |   |   |"
COLOR 15: PRINT "     |   |    \   |  \_/  |   __)_    |   l____   |   |   |   |   |   |"
COLOR 15: PRINT "     |   l    /   |   |   |   l   |   |   |   l   |   |   l   |   |   |"
COLOR 15: PRINT "     l_______/|   l___|   l_______l___|   l_______|   l_______l___|   l"
COLOR 15: PRINT "              `---'   `---'           `---'       `---'           `---'"
COLOR 15: PRINT "     _______ _______ _______ ___________ ___ ___ _______ _______ _______"
COLOR 7: PRINT "    Y   _   Y   _   Y   _   Y   _   _   Y   Y   Y   _   Y   _   \   _   Y"
COLOR 7: PRINT "    |   l___l   |   |   l___l___|   |___l   |   |   l   |   l   /   l___|"
COLOR 7: PRINT "    l____   |   |   |   __)     |   |   |  / \  |   _   |   _   l   __)_"
COLOR 8: PRINT "    |   l   |   l   |   |       |   |   |       |   |   |   |   |   l   |"
COLOR 8: PRINT "    |_______|_______l   |       |   l   l___l___l___|   l___|   l_______|"
COLOR 8: PRINT "                    `---'       `---'               `---'   `---'"
COLOR 15: PRINT "                         http://4ds.simplenet.com/"
PRINT
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                    Draws a kind of line (1 at the moment)                ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB DrawLine (Kind)
	SELECT CASE Kind
		CASE 1
			P 8, " ÄÄ": P 7, "ÄÄ": P 15, STRING$(69, 196): P 7, "ÄÄ": P 8, "ÄÄ"
	END SELECT
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                       Erases all color codes in a line                   ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION Erase$ (TheStr$)
		ddd% = INSTR(TheStr$, "~")
		WHILE ddd% > 0
			aff% = INSTR(Temp$, "~")
			TheTemp$ = LEFT$(Temp$, aff% - 1)
			Temp$ = RIGHT$(Temp$, LEN(Temp$) - 3)
			ddd% = INSTR(Temp$, "~")
		WEND

END FUNCTION

FUNCTION GetKey$
	a$ = "": WHILE a$ = "": a$ = INKEY$: WEND
	GetKey$ = UCASE$(a$)
END FUNCTION

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                       Gets X/Y coordinates for Mouse                     ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION GetMouseStatus (MPosX, MPosY)
		ax = 3
		Outregs.bx = 0: Outregs.cx = 0: Outregs.dx = 0
		MouseHandler ax, 0, 0, 0
		GetMouseStatus = Outregs.bx
		MPosX = Outregs.cx \ MHCell + 1
		MPosY = Outregs.dx \ MVCell + 1
END FUNCTION

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                        Initializes the Help viewer                       ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB InitHelp
	 LastPos = 2
	 PCOPY 0, 7
	 DoneIt = 1                 ' Used to check if InitHelp was called before
	 wid = 1
	 RESTORE Help
	 READ num
	 FOR a = 1 TO num                                               'Get Colors
		  READ Text$(a - 1)
		  ColorVal(a - 1) = VAL("&H" + LEFT$(Text$(a - 1), 2))
		  Text$(a - 1) = RIGHT$(Text$(a - 1), LEN(Text$(a - 1)) - 2)
		  TheLen = 79
		  FOR BB = 1 TO LEN(Text$(a - 1))
				IF INSTR(BB, Text$(a - 1), "~") THEN TheLen = TheLen + 3
		  NEXT
		  Text$(a - 1) = LEFT$(Text$(a - 1), TheLen)    'Convert CHR$(255)
		  WHILE INSTR(Text$(a - 1), CHR$(255))          ' to quotes ("")
				quotes = INSTR(Text$(a - 1), CHR$(255))
				IF quotes THEN MID$(Text$(a - 1), quotes, 1) = CHR$(34)
		  WEND
	 NEXT
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           Initializes the Mouse                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB InitMouse
IF UseMouse THEN
	IF Mouseinit = 0 THEN
		IF ThereIsAMouse THEN
			a% = MouseReset
			UseMouse = a%
			Mouseinit = 1
		END IF
	END IF
END IF
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           Handles all Mouse code                         ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB MouseHandler (ax, bx, cx, dx)

'DIM Inregs AS REGS
		Inregs.ax = ax
		Inregs.bx = bx
		Inregs.cx = cx
		Inregs.dx = dx
		'INTERRUPT MouseIntr, Inregs, Outregs
		DEF SEG = VARSEG(dos%(0))
		CALL ABSOLUTE(MouseIntr%, Inregs, Outregs, VARPTR(dos%(0)))

END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                        Turns off Mouse temporarily                       ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB MouseOff
		IF MouseVisible THEN
			 MouseHandler 2, 0, 0, 0
			 MouseVisible = False
		END IF
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                         Turns on Mouse temporarily                       ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB MouseOn
		IF NOT MouseVisible THEN
			 MouseHandler 1, 0, 0, 0
			 MouseVisible = True
		END IF
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                        Resets Mouse to basic usage                       ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION MouseReset
		MHMax = 639    'Max virtual horizontal mouse position
		MVMax = 199    'Max virtual vertical mouse position
		MHCell = 8     'Mouse horizontal cell width
		MVCell = 8     'Mouse vertical cell height
		MouseIntr = &H33
		MouseHandler 0, 0, 0, 0
		MouseReset = Outregs.ax
		MouseVisible = False
END FUNCTION

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                              Actual program                              ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB OnlineHelp
 InitHelp
 DrawBottom
 Start = 1
WHILE KeyIn$ <> CHR$(27)
TheStart:
 IF UseMouse% THEN MouseOff
 ScrollBar Start, num - 21
 FOR a = Start TO Start + 22

	LOCATE a - Start + 1, 1
	IF INSTR(Text$(a - 1), "þ") THEN
		IF INSTR(Text$(a - 1), "LINE") THEN
			TypeLine = VAL(MID$(Text$(a - 1), INSTR(Text$(a - 1), "LINE") + 4, 1))
			DrawLine TypeLine
			GOTO Outta
		END IF
		IF INSTR(Text$(a - 1), "BUTTON") THEN
		PRINT SPACE$(79);
		LOCATE a - Start + 1, 1
			PRINT SPACE$(INSTR(Text$(a - 1), "BUTTON"));
			DB
			GOTO Outta
		END IF
	END IF
	IF INSTR(Text$(a - 1), "~") THEN
		PRINT SPACE$(79);
		LOCATE a - Start + 1, 1
		NextColor = ColorVal(a - 1)
		Temp$ = Text$(a - 1)
		ddd% = INSTR(Text$(a - 1), "~")
		WHILE ddd% > 0
			aff% = INSTR(Temp$, "~")
			TheTemp$ = LEFT$(Temp$, aff% - 1)
			Temp$ = RIGHT$(Temp$, LEN(Temp$) - aff% + 1)
			P NextColor, TheTemp$
			NextColor$ = "&H" + RIGHT$(LEFT$(Temp$, 3), 2)
			NextColor = VAL(NextColor$)
			Temp$ = RIGHT$(Temp$, LEN(Temp$) - 3)
			ddd% = INSTR(Temp$, "~")
		WEND
			P NextColor, Temp$
		  
	ELSE
		P ColorVal(a - 1), Text$(a - 1) + SPACE$(79 - LEN(Text$(a - 1)))
	END IF
Outta:
 NEXT
 IF UseMouse% THEN MouseOn
GetKey:
	KeyIn$ = ""
	WHILE KeyIn$ = "": KeyIn$ = INKEY$
IF UseMouse THEN
	buttons = GetMouseStatus(MPosX, MPosY)
	IF buttons AND Count > 0 THEN
		Count = Count + 1
		IF Count = 1500 THEN Count = 0
	ELSE
		Count = 0
	END IF
	IF buttons = 1 THEN
		IF MPosX = 80 THEN
			IF MPosY = 1 THEN
				KeyIn$ = CHR$(0) + CHR$(&H48)
			END IF
			IF MPosY = 23 THEN
				KeyIn$ = CHR$(0) + CHR$(&H50)
			END IF
			IF MPosY <> 1 AND MPosY < 23 THEN
				IF Count = 0 THEN
					IF MPosY < LastPos THEN KeyIn$ = CHR$(0) + CHR$(&H49): Count = 1
					IF MPosY > LastPos THEN KeyIn$ = CHR$(0) + CHR$(&H51): Count = 1
				END IF
			END IF
		ELSE
			IF MPosY < 24 THEN
			IF INSTR(Text$(Start + MPosY - 2), "þBUTTON") THEN
				DDDD = Start + MPosY - 2
				TypeLine = VAL(MID$(Text$(DDDD), INSTR(Text$(DDDD), "BUTTON") + 6, 4))
				Start = TypeLine: GOTO TheStart
			END IF
			END IF
		END IF
	END IF
END IF
	WEND
SELECT CASE KeyIn$
	CASE CHR$(0) + CHR$(&H50)
		Start = Start + 1
		IF Start = num - 21 THEN Start = Start - 1: GOTO GetKey
	CASE CHR$(0) + CHR$(&H48)
		Start = Start - 1
		IF Start = 0 THEN Start = 1: GOTO GetKey
	CASE CHR$(27)
		EXIT SUB
	CASE CHR$(0) + CHR$(&H51)
		IF Start = num - 22 THEN GOTO GetKey
		Start = Start + 22
		IF Start > num - 22 THEN Start = num - 22
	CASE CHR$(0) + CHR$(&H49)
		IF Start = 1 THEN GOTO GetKey
		Start = Start - 22
		IF Start < 1 THEN Start = 1
	CASE CHR$(0) + CHR$(&H47)'Home
		IF Start = 1 THEN GOTO GetKey
		Start = 1
	CASE CHR$(0) + CHR$(&H4F)'END
		IF Start = num - 22 THEN GOTO GetKey
		Start = num - 22
	CASE "S", "s"
		Search
	CASE "P", "p"
		PrintDocument
	CASE ELSE
		GOTO GetKey
END SELECT
WEND
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                     Used to replace 2 long lines of code.                ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB P (TheColor, st$)
COLOR TheColor MOD 32, INT(TheColor / 32)
PRINT st$;
END SUB

SUB PrintDocument
	LOCATE 25, 1: PRINT SPACE$(80);

	LOCATE 25, 1: PRINT "Print Complete Document? (Y/N)";
	WHILE a$ <> "N" AND a$ <> "Y": a$ = GetKey$: WEND
	IF a$ = "N" THEN
		DrawBottom
		EXIT SUB
	ELSE
		FOR TheLoop = 1 TO num
			LPRINT Erase$(Text$(TheLoop - 1))
		NEXT
		LPRINT CHR$(12); 'np
		LOCATE 25, 1: PRINT SPACE$(80);
		LOCATE 25, 1: PRINT "Printing Completed, press any key to continue";
		a$ = GetKey$
		DrawBottom
	END IF
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                       Displays scrollbar on the side                     ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB ScrollBar (Position, Total)
Percentage = INT(Position / Total * 21) + 2
LOCATE LastPos, 80: COLOR 0, 15: PRINT "°";
LOCATE Percentage, 80
P 0, " "
LastPos = Percentage
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                                Does search                               ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
SUB Search
	COLOR 7, 0
	LOCATE 25, 1
	PCOPY 0, 7
	PRINT SPACE$(80);
	LOCATE 25, 1
	INPUT "Search for What:", TheSearch$
	PCOPY 7, 0
	TheSearch$ = UCASE$(TheSearch$)
	FOR a = 1 TO num
		IF INSTR(UCASE$(Text$(a - 1)), TheSearch$) THEN
	IF a + 22 > num THEN DD = a - 22: bottom = 1:  ELSE DD = a: bottom = 0
	FOR BB = DD TO DD + 22
		LOCATE BB - DD + 1, 1
			IF INSTR(Text$(BB - 1), "þ") THEN
				IF INSTR(Text$(BB - 1), "LINE") THEN
					TypeLine = VAL(MID$(Text$(BB - 1), INSTR(Text$(BB - 1), "LINE") + 4, 1))
					DrawLine TypeLine
					GOTO Outta1
				END IF
				IF INSTR(Text$(BB - 1), "BUTTON") THEN
				PRINT SPACE$(79);
				LOCATE BB - DD + 1, 1
					PRINT SPACE$(INSTR(Text$(BB - 1), "BUTTON"));
					DB
					GOTO Outta1
				END IF
			END IF
			IF INSTR(Text$(BB - 1), "~") THEN
				PRINT SPACE$(79);
				LOCATE BB - DD + 1, 1
				NextColor = ColorVal(BB - 1)
				Temp$ = Text$(BB - 1)
				ddd% = INSTR(Text$(BB - 1), "~")
				WHILE ddd% > 0
					aff% = INSTR(Temp$, "~")
					TheTemp$ = LEFT$(Temp$, aff% - 1)
					Temp$ = RIGHT$(Temp$, LEN(Temp$) - aff% + 1)
					P NextColor, TheTemp$
					NextColor$ = "&H" + RIGHT$(LEFT$(Temp$, 3), 2)
					NextColor = VAL(NextColor$)
					Temp$ = RIGHT$(Temp$, LEN(Temp$) - 3)
					ddd% = INSTR(Temp$, "~")
				WEND
					P NextColor, Temp$
		 
			ELSE
				P ColorVal(BB - 1), Text$(BB - 1) + SPACE$(79 - LEN(Text$(BB - 1)))
			END IF
Outta1:

 NEXT
 IF bottom = 0 THEN
	COLOR 15, 0: LOCATE 1, 1: PRINT "->": LOCATE 1, 78: PRINT "<-"
 ELSE
	COLOR 15, 0: LOCATE 23, 1: PRINT "->": LOCATE 23, 78: PRINT "<-"
 END IF
 LOCATE 25, 1: PRINT SPACE$(80); : LOCATE 25, 1
 PRINT "Continue? (Y/N)";
 Start = a
IF Start > num - 22 THEN Start = num - 22
 
HEHE: F$ = "": WHILE F$ = "": F$ = INKEY$: WEND
	IF F$ = "Y" OR F$ = "y" THEN GOTO kewll
	IF F$ = "N" OR F$ = "n" THEN GOTO endingsearch
	IF UCASE$(F$) <> "Y" AND UCASE$(F$) <> "N" THEN GOTO HEHE
kewll:
	END IF
 NEXT
 SOUND 750, 1.5
 COLOR 18, 0
 LOCATE 25, 1: PRINT SPACE$(80); : LOCATE 25, 1
 PRINT "No search is found...  ";
 a$ = ""
 WHILE a$ = ""
	a$ = INKEY$
 WEND
 LOCATE 25, 1: PRINT SPACE$(80);

DrawBottom
EXIT SUB
endingsearch:
Start = a
DrawBottom
IF Start > num - 22 THEN Start = num - 22
ScrollBar Start, num - 22
END SUB

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                           CatGen v3.0 Shareware                          ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''                 Checks to see if there is a mouse installed              ''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION ThereIsAMouse
		IRET = 207
		DEF SEG = 0    'Set to base system address
		DIM MouseSegment AS LONG
		MouseSegment = PEEK(207) * 256 + PEEK(206)
		MouseOffset = PEEK(205) * 256 + PEEK(204)
		IF MouseSegment = 0 AND MouseOffset = 0 THEN
			 ThereIsAMouse = False
		ELSE
			 DEF SEG = MouseSegment
			 MouseInstruction = PEEK(MouseOffset)
			 IF MouseInstruction = IRET THEN
					ThereIsAMouse = False
			 ELSE
					ThereIsAMouse = True
			 END IF
		END IF
		DEF SEG
END FUNCTION

