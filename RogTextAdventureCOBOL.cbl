      ******************************************************************
      * Author: Roger Williams
      * Date:   01/08/2024
      * Purpose:Text Adventure!
      *
      *Converted from Visual Basic
      *
      *written in OpenCobolIDE
      *
      *
      *Needed a LOT of fettling as the two languages are HUGELY
      *different, yet old COBOL has in some ways more power than ANY
      *modern language and its much faster and unhackable!
      *
      *One major downside is cannot declare variables in paragraphs!
      *hence one big mass of variables at the top of the code, also can
      *make debugging hard, plus it doesnt have a debugger!
      *
      *intro screen and level text files needed MAJOR editing as
      *COBOL only supports 80 columns and 30 rows AND COBOL expects files
      *to have FIXED length field data
      *
      *NOTE: where possible Visual Basic comments are included, being
      *      radically different to VB in many ways has actually
      *      sparked the development of a more advanced parser
      *      due to COBOLs simpler string manipulation functions!
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROGsTextAdventureCOBOL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

      *define data files and access
       FILE-CONTROL.
           SELECT FILE-INTRO1 ASSIGN TO
             STR-INTRO1NAME
             ORGANISATION IS LINE SEQUENTIAL.

           SELECT FILE-INTRO2 ASSIGN TO
             STR-INTRO2NAME
             ORGANISATION IS LINE SEQUENTIAL.

           SELECT FILE-LEVEL1 ASSIGN TO
             STR-LEVEL1NAME
             ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *define tables for reading intro screens into
       FD FILE-INTRO1.
           01 REC-INTRO1.
              03 INTRO1-LINE  PIC X(80).

       FD FILE-INTRO2.
           01 REC-INTRO2.
              03 INTRO2-LINE  PIC X(80).
      *define tbale for reading room data into
       FD FILE-LEVEL1.
           01 REC-ROOM-READ.
              05 INT-ROOMID-READ PIC 99 VALUE ZEROS.
      *    'next 4 properties determine which room this one leads to 0 = no room!
              05 INT-NEXTROOMNORTH-READ PIC 99 VALUE ZEROS.
              05 INT-NEXTROOMSOUTH-READ PIC 99 VALUE ZEROS.
              05 INT-NEXTROOMEAST-READ PIC 99 VALUE ZEROS.
              05 INT-NEXTROOMWEST-READ PIC 99 VALUE ZEROS.
      *'used for text to describe room to player
              05 STR-DESC1-READ PIC X(80) VALUE SPACES.
              05 STR-DESC2-READ PIC X(80) VALUE SPACES.
              05 STR-DESC3-READ PIC X(80) VALUE SPACES.
              05 STR-DESC4-READ PIC X(80) VALUE SPACES.
              05 STR-DESC5-READ PIC X(80) VALUE SPACES.
              05 STR-DESC6-READ PIC X(80) VALUE SPACES.
              05 STR-DESC7-READ PIC X(80) VALUE SPACES.
              05 STR-DESC8-READ PIC X(80) VALUE SPACES.
              05 STR-DESC9-READ PIC X(80) VALUE SPACES.
              05 STR-DESC10-READ PIC X(80) VALUE SPACES.
              05 STR-DESC11-READ PIC X(80) VALUE SPACES.
              05 STR-DESC12-READ PIC X(80) VALUE SPACES.
              05 STR-DESC13-READ PIC X(80) VALUE SPACES.
              05 STR-DESC14-READ PIC X(80) VALUE SPACES.
              05 STR-DESC15-READ PIC X(80) VALUE SPACES.
              05 STR-DESC16-READ PIC X(80) VALUE SPACES.
              05 STR-DESC17-READ PIC X(80) VALUE SPACES.
              05 STR-DESC18-READ PIC X(80) VALUE SPACES.
              05 STR-DESC19-READ PIC X(80) VALUE SPACES.
              05 STR-DESC20-READ PIC X(80) VALUE SPACES.
              05 STR-DESC21-READ PIC X(80) VALUE SPACES.
              05 STR-DESC22-READ PIC X(80) VALUE SPACES.
              05 STR-DESC23-READ PIC X(80) VALUE SPACES.
              05 STR-DESC24-READ PIC X(80) VALUE SPACES.
              05 STR-DESC25-READ PIC X(80) VALUE SPACES.


       WORKING-STORAGE SECTION.
      *    define file location vars
      *    NOTE: using level 77 on purpose as these vars have no children
           01 STR-PROJECTPATH PIC X(200)
             VALUE "C:\projects\COBOL\Projects\RogTextAdventureCOBOL\".
           77 STR-INTRO1NAME PIC X(60) VALUE "INTROSCR1_COBOL.TXT".
           77 STR-INTRO2NAME PIC X(60) VALUE "INTROSCR2_COBOL.TXT".
           77 STR-LEVEL1NAME PIC X(60) VALUE "LEVEL1_COBOL.TXT".

      *    room class vars allow maximum of 40 rooms per level
      *
           01 REC-ROOM-INTERNAL.
            03 REC-ROOM OCCURS 40 TIMES.
              05 INT-ROOMID PIC 99 VALUE ZEROES.
      *    'next 4 propoerties determine which room this one leads to 0=no room!
              05 INT-NEXTROOMNORTH PIC 99 VALUE ZEROES.
              05 INT-NEXTROOMSOUTH PIC 99 VALUE ZEROES.
              05 INT-NEXTROOMEAST PIC 99 VALUE ZEROES.
              05 INT-NEXTROOMWEST PIC 99 VALUE ZEROES.
      *'used for text to describe room to player
              05 STR-DESC-INT1 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT2 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT3 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT4 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT5 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT6 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT7 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT8 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT9 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT10 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT11 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT12 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT13 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT14 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT15 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT16 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT17 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT18 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT19 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT20 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT21 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT22 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT23 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT24 PIC X(80) VALUE SPACES.
              05 STR-DESC-INT25 PIC X(80) VALUE SPACES.

      *    stores maximum number of rooms in current level
           77 INT-NUMROOMS PIC 99 VALUE ZEROS.
      *    stores current room ID
           77 INT-CURROOM PIC 99 VALUE 1.

      *********************************************************
      *    parser vars
      *
      *internal lists below converted into COBOL tables
      *
      *    Private ReadOnly lstVerbs As New List(Of String)({"be", "have", "do", "go", "get", "make", "know", "take", "see", "look", "give", "need", "put", "get", "let", "begin", "create", "start", "run", "move", "creep",
      *                                                    "hold", "use", "include", "set", "stop", "allow", "appear", "destroy", "kill", "disable", "enable", "open", "close", "run", "talk", "listen", "walk"})
      *    Private ReadOnly lstNouns As New List(Of String)({"exit", "my", "you", "them", "they", "him", "she", "me", "their", "knIFe", "apple", "bread", "sword", "dragon", "knight", "key", "plate", "cnadle", "matches", "door", "exit"})
      *    Private ReadOnly lstAdjectives As New List(Of String)({"new", "old", "box", "first", "last", "current", "low", "high", "partial", "full", "common", "late", "early", "on", "used", "alert", "away", "forward", "backward",
      *                                                  "left", "right"})
      *    Private ReadOnly lstPrepositions As New List(Of String)({"in", "of", "with", "to", "behind", "when", "why", "while", "kind", "by", "under", "before", "up", "down", "between"})
      *    Private ReadOnly lstDirections As New List(Of String)({"north", "south", "east", "west"})
      *

       01 LST-VERBS-INTERNAL.
         05 FILLER PIC X(20) VALUE "be".
         05 FILLER PIC X(20) VALUE "have".
         05 FILLER PIC X(20) VALUE "do".
         05 FILLER PIC X(20) VALUE "go".
         05 FILLER PIC X(20) VALUE "get".
         05 FILLER PIC X(20) VALUE "make".
         05 FILLER PIC X(20) VALUE "know".
         05 FILLER PIC X(20) VALUE "take".
         05 FILLER PIC X(20) VALUE "see".
         05 FILLER PIC X(20) VALUE "look".
         05 FILLER PIC X(20) VALUE "give".
         05 FILLER PIC X(20) VALUE "need".
         05 FILLER PIC X(20) VALUE "put".
         05 FILLER PIC X(20) VALUE "get".
         05 FILLER PIC X(20) VALUE "let".
         05 FILLER PIC X(20) VALUE "begin".
         05 FILLER PIC X(20) VALUE "create".
         05 FILLER PIC X(20) VALUE "start".
         05 FILLER PIC X(20) VALUE "run".
         05 FILLER PIC X(20) VALUE "move".
         05 FILLER PIC X(20) VALUE "creep".
         05 FILLER PIC X(20) VALUE "hold".
         05 FILLER PIC X(20) VALUE "use".
         05 FILLER PIC X(20) VALUE "include".
         05 FILLER PIC X(20) VALUE "set".
         05 FILLER PIC X(20) VALUE "stop".
         05 FILLER PIC X(20) VALUE "allow".
         05 FILLER PIC X(20) VALUE "appear".
         05 FILLER PIC X(20) VALUE "destroy".
         05 FILLER PIC X(20) VALUE "kill".
         05 FILLER PIC X(20) VALUE "disable".
         05 FILLER PIC X(20) VALUE "enable".
         05 FILLER PIC X(20) VALUE "open".
         05 FILLER PIC X(20) VALUE "close".
         05 FILLER PIC X(20) VALUE "run".
         05 FILLER PIC X(20) VALUE "talk".
         05 FILLER PIC X(20) VALUE "listen".
         05 FILLER PIC X(20) VALUE "walk".
      *    this bit allows programmatic acces to the table values
       01 LST-VERBS-REDEF REDEFINES LST-VERBS-INTERNAL.
         05 LST-VERBS OCCURS 38 TIMES PIC X(20).

       01 LST-NOUNS-INTERNAL.
         05 FILLER PIC X(20) VALUE "exit".
         05 FILLER PIC X(20) VALUE "my".
         05 FILLER PIC X(20) VALUE "you".
         05 FILLER PIC X(20) VALUE "them".
         05 FILLER PIC X(20) VALUE "they".
         05 FILLER PIC X(20) VALUE "him".
         05 FILLER PIC X(20) VALUE "she".
         05 FILLER PIC X(20) VALUE "me".
         05 FILLER PIC X(20) VALUE "their".
         05 FILLER PIC X(20) VALUE "knIFe".
         05 FILLER PIC X(20) VALUE "apple".
         05 FILLER PIC X(20) VALUE "bread".
         05 FILLER PIC X(20) VALUE "sword".
         05 FILLER PIC X(20) VALUE "dragon".
         05 FILLER PIC X(20) VALUE "knight".
         05 FILLER PIC X(20) VALUE "key".
         05 FILLER PIC X(20) VALUE "plate".
         05 FILLER PIC X(20) VALUE "caNdle".
         05 FILLER PIC X(20) VALUE "matches".
         05 FILLER PIC X(20) VALUE "door".
         05 FILLER PIC X(20) VALUE "exit".
      *    this bit allows programmatic acces to the table values
       01 LST-NOUNS-REDEF REDEFINES LST-NOUNS-INTERNAL.
         05 LST-NOUNS OCCURS 21 TIMES PIC X(20).

       01 LST-ADJECTIVES-INTERNAL.
         05 FILLER PIC X(20) VALUE "new".
         05 FILLER PIC X(20) VALUE "old".
         05 FILLER PIC X(20) VALUE "box".
         05 FILLER PIC X(20) VALUE "first".
         05 FILLER PIC X(20) VALUE "last".
         05 FILLER PIC X(20) VALUE "current".
         05 FILLER PIC X(20) VALUE "low".
         05 FILLER PIC X(20) VALUE "high".
         05 FILLER PIC X(20) VALUE "partial".
         05 FILLER PIC X(20) VALUE "full".
         05 FILLER PIC X(20) VALUE "common".
         05 FILLER PIC X(20) VALUE "late\".
         05 FILLER PIC X(20) VALUE "early".
         05 FILLER PIC X(20) VALUE "on".
         05 FILLER PIC X(20) VALUE "used".
         05 FILLER PIC X(20) VALUE "alert".
         05 FILLER PIC X(20) VALUE "away".
         05 FILLER PIC X(20) VALUE "forward".
         05 FILLER PIC X(20) VALUE "backward".
         05 FILLER PIC X(20) VALUE "left".
         05 FILLER PIC X(20) VALUE "right".
      *    this bit allows programmatic acces to the table values
       01 LST-ADJECTIVES-REDEF REDEFINES LST-ADJECTIVES-INTERNAL.
         05 LST-ADJECTIVES OCCURS 21 TIMES PIC X(20).

       01 LST-PREPOSITIONS-INTERNAL.
         05 FILLER PIC X(20) VALUE "in".
         05 FILLER PIC X(20) VALUE "of".
         05 FILLER PIC X(20) VALUE "with".
         05 FILLER PIC X(20) VALUE "to".
         05 FILLER PIC X(20) VALUE "behind".
         05 FILLER PIC X(20) VALUE "when".
         05 FILLER PIC X(20) VALUE "why".
         05 FILLER PIC X(20) VALUE "while".
         05 FILLER PIC X(20) VALUE "kind".
         05 FILLER PIC X(20) VALUE "by".
         05 FILLER PIC X(20) VALUE "under".
         05 FILLER PIC X(20) VALUE "before".
         05 FILLER PIC X(20) VALUE "up".
         05 FILLER PIC X(20) VALUE "down".
         05 FILLER PIC X(20) VALUE "between".
      *    this bit allows programmatic acces to the table values
       01 LST-PREPOSITION-REDEF REDEFINES LST-PREPOSITIONs-INTERNAL.
         05 LST-PREPOSITIONS OCCURS 15 TIMES PIC X(20).

       01 LST-DIRECTIONS-INTERNAL.
         05 FILLER PIC X(20) VALUE "north".
         05 FILLER PIC X(20) VALUE "south".
         05 FILLER PIC X(20) VALUE "east".
         05 FILLER PIC X(20) VALUE "west".
      *    this bit allows programmatic acces to the table values
       01 LST-DIRECTIONS-REDEF REDEFINES LST-DIRECTIONS-INTERNAL.
         05 LST-DIRECTIONS OCCURS 4 TIMES PIC X(20).

      *    list/table sizes
       77 INT-NUMVERBS PIC 99 VALUE 38.
       77 INT-NUMPREPOSITIONS PIC 99 VALUE 15.
       77 INT-NUMDIRECTIONS PIC 9 VALUE 4.
       77 INT-NUMVADJECTIVES PIC 99 VALUE 21.
       77 INT-NUMNOUNS PIC 99 VALUE 21.

      *    handle EOF for each file
       77 ENDOFINTROFILE1 PIC 9(01) VALUE 0.
       77 ENDOFINTROFILE2 PIC 9(01) VALUE 0.
       77 ENDOFLEVELFILE PIC 9(01) VALUE 0.

      *    used by 0500-GETSTRINGLENGTH
       77 INT-LENGTH PIC 999 VALUE ZEROS.
       77 INT-COUNTOFLEADINGSPACES PIC 999 VALUE ZEROS.
       77 STR-GETLENGTH PIC X(20) VALUE SPACES.

      *    public vars
       77 STR-NOUN PIC X(20) VALUE SPACES.
       77 STR-VERB PIC X(20) VALUE SPACES.
       77 STR-ADJECTIVE PIC X(20) VALUE SPACES.
       77 STR-PREPOSITION PIC X(20) VALUE SPACES.
       77 STR-DIRECTION PIC X(20) VALUE SPACES.

      *    used for parsing user input
       77 INT-COUNT PIC 9 VALUE ZERO.
       77 INT-COUNTFOUND PIC 9 VALUE ZERO.
       77 STR-TEMP1 PIC X(20) VALUE SPACES.
       77 STR-RESULT1 PIC X(20) VALUE SPACES.
       77 STR-RESULT2 PIC X(20) VALUE SPACES.
       77 STR-RESULT3 PIC X(20) VALUE SPACES.
       77 STR-RESULT4 PIC X(20) VALUE SPACES.

      *    these used by the WAIT procedure
       77 INT-WAIT5SECONDS PIC 99999999 VALUE 50000000.
       77 INT-WAIT3SECONDS PIC 99999999 VALUE 30000000.
       77 BLN-WAIT3SECONDS PIC X VALUE "Y".
       77 BLN-WAIT5SECONDS PIC X VALUE "Y".
      *    read by showroom if 1 show first screen
       77 INT-ROOMNUMBER PIC 99 VALUE 1.
      *    end of game xhexker
       77 BLN-ENDOFGAME PIC X VALUE "N".

      *    other vars
       77 BLN-ISOK PIC X VALUE "Y".
       77 BLN-VALIDDIRECTION PIC X VALUE "N".
       77 BLN-OK PIC X VALUE "Y".
       77 BYT-WHAT PIC 9 VALUE ZERO.
       77 BYT-VALID PIC 99 VALUE 1.
       77 STR-TEMP2 PIC X(255) VALUE SPACES.
       77 INT-NUM PIC 99 VALUE 1.
       77 INT-NUM1 PIC 99 VALUE ZEROS.
       77 INT-NUM2 PIC 99 VALUE ZEROS.
       77 INT-NUM3 PIC 99 VALUE ZEROS.
       77 INT-NUMWAIT PIC 99999999 VALUE ZEROS.
       77 INT-START PIC 99 VALUE ZEROS.
       77 STR-BLANKLINE PIC X(80) VALUE SPACES.
       77 STR-OUTPUT PIC X(80) VALUE SPACES.
       77 STR-INPUT PIC X(20) VALUE SPACES.
       77 INT-LINESPRINTED PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    open intro files to test they exist
           OPEN INPUT FILE-INTRO1.
           OPEN INPUT FILE-INTRO2.

      *    configure eof trap
           READ FILE-INTRO1
                 AT END MOVE 1 TO ENDOFINTROFILE1
           END-READ.

      *    check file has data
           IF ENDOFINTROFILE1 EQUALS 1
              DISPLAY "ERROR OPENING FILE"
              STOP RUN
           END-IF.

      *    configure eof trap
           READ FILE-INTRO2
                 AT END MOVE 1 TO ENDOFINTROFILE2
           END-READ.

      *    check file has data
           IF ENDOFINTROFILE2 EQUALS 1
              DISPLAY "ERROR OPENING FILE"
              STOP RUN
           END-IF.

           PERFORM 0006-INIT.

      *     main routine
           MOVE SPACES TO STR-INPUT.

      *    loop till user wants to leave
           PERFORM TEST AFTER UNTIL STR-INPUT EQUALS "exit"
      *    reset valid direction VALUE
           MOVE "N" TO BLN-VALIDDIRECTION
      *    get user instruction
           ACCEPT STR-INPUT
      *    convert to lowercase
           MOVE FUNCTION LOWER-CASE(STR-INPUT) TO STR-INPUT
      *   'validate entry
           PERFORM 0200-PARSEINPUT
      *      'is it ok?
            IF BLN-ISOK EQUALS "Y" THEN
      *          'check for movement verb
               IF STR-RESULT1 EQUALS "go" OR
                  STR-RESULT1 EQUALS "move" THEN
      *         'set to current room number - why? because IF the direction is VALID
      *         'the room number will change
                  MOVE INT-CURROOM TO INT-NUM
      *           reset valid direction var
                  MOVE "N" TO BLN-VALIDDIRECTION
      *              'south is forward, north backward, east/west left/right
                 IF STR-DIRECTION EQUALS "north" AND
                     INT-NEXTROOMNORTH(INT-NUM) NOT EQUAL TO 0 THEN
      *                  'move north
                     MOVE INT-NEXTROOMNORTH(INT-NUM) TO INT-CURROOM
                     MOVE "Y" TO BLN-VALIDDIRECTION
                 END-IF
                 IF STR-DIRECTION EQUALS  "south" AND
                    INT-NEXTROOMSOUTH(INT-NUM) NOT EQUAL TO 0 THEN
      *                    'move south
                      MOVE INT-NEXTROOMSOUTH(INT-NUM) TO INT-CURROOM
                      MOVE "Y" TO BLN-VALIDDIRECTION
                 END-IF
                 IF STR-DIRECTION EQUALS "east" AND
                    INT-NEXTROOMEAST(INT-NUM) NOT EQUAL TO 0 THEN
      *                     'move east
                    MOVE INT-NEXTROOMEAST(INT-NUM) TO INT-CURROOM
                    MOVE "Y" TO BLN-VALIDDIRECTION
                 END-IF
                 IF STR-DIRECTION EQUALS "west" AND
                    INT-NEXTROOMWEST(INT-NUM) NOT EQUAL TO 0 THEN
      *                     'move west
                    MOVE INT-NEXTROOMWEST(INT-NUM) TO INT-CURROOM
                    MOVE "Y" TO BLN-VALIDDIRECTION
                 END-IF

                 IF BLN-VALIDDIRECTION EQUALS "N" THEN
      *                  'direction entered does not exist?
                  DISPLAY "Sorry! - Direction entered isnt "
                          "available! Please try again"
      *               'wait before redrawing screen
                  MOVE "N" TO BLN-WAIT5SECONDS
                  PERFORM 0002-WAIT
                 END-IF

      *             'IF command not acceptable dont change rooms
                 IF INT-NUM EQUALS INT-CURROOM THEN
                    DISPLAY "Sorry! - Unregonised Command! "
                            "Please try again"
                    MOVE SPACES TO STR-INPUT
      *                   'wait before redrawing screen
                    MOVE "N" TO BLN-WAIT5SECONDS
                    PERFORM 0002-WAIT
                 END-IF

      *              'shows new or even existing room
      *              clsCurRoom EQUALS lstRooms.Find(Function(clsCurRoomsFind) clsCurRoomsFind.ID EQUALS  INT-CURROOM)
                 MOVE 1 TO INT-NUM1
      *          find room to move to
                 PERFORM 0205-LOOKFORROOM TEST AFTER
                 UNTIL INT-NUM1 GREATER THAN INT-NUMROOMS

      *              'show room to player pass 0 as not first room
                 MOVE 0 TO INT-ROOMNUMBER
                 PERFORM 0003-SHOWROOM

      *              'has user lost/won the game?
                 PERFORM 0004-CHECKIFEND

                 IF BLN-ENDOFGAME EQUALS "Y" THEN
      *                  'set text input to "exit" this causes the game to end
                     MOVE "exit" TO STR-INPUT
                 END-IF
               ELSE
      *              'ignore help and exit commands only show error for commands not understood
      *              'NOTE: check game logic - can this be refactored away?
                 IF STR-RESULT1 NOT EQUAL TO "help" AND
                    STR-RESULT1 NOT EQUAL TO "exit" THEN
                    DISPLAY "Unregonised command, please try again!"

      *              'ignore exit command
                    IF STR-INPUT NOT EQUAL TO "exit" THEN
      *                  'clear last command
                       MOVE SPACES TO STR-INPUT
      *                 'give user time to see error
                       MOVE "N" TO BLN-WAIT5SECONDS
                       PERFORM 0002-WAIT
                       PERFORM 0003-SHOWROOM
                    END-IF
                 END-IF

                 IF STR-RESULT1 EQUAL TO "help" THEN
      *             'clear last command
                     MOVE SPACES TO STR-INPUT
      *              'give user time to see error
                     MOVE "N" TO BLN-WAIT5SECONDS
                     PERFORM 0002-WAIT
                     PERFORM 0003-SHOWROOM
                 END-IF
               END-IF
            ELSE
      *          'IF command not understood and not "exit"
              IF STR-INPUT NOT EQUAL TO "exit" THEN
                 DISPLAY "Unregonised command, please try again!"
      *             'clear last command
                 MOVE SPACES TO STR-INPUT
      *              'give user time to see error
                 MOVE "N" TO BLN-WAIT5SECONDS
                 PERFORM 0002-WAIT
                 PERFORM 0003-SHOWROOM
              END-IF
            END-IF
           END-PERFORM.


            STOP RUN.

      *    ***other paragraphs**************

       0000-CLEARSCREEN.
      *    prints 40 blank lines to the console
           PERFORM 30 TIMES
             DISPLAY STR-BLANKLINE
           END-PERFORM.

       0001-SHOWINTRO.
      *    show intro screen 1
           PERFORM UNTIL ENDOFINTROFILE1 EQUALS 1
              DISPLAY INTRO1-LINE
              READ FILE-INTRO1 INTO REC-INTRO1
                   AT END MOVE 1 TO ENDOFINTROFILE1
              END-READ
           END-PERFORM.

           CLOSE FILE-INTRO1.

      *    let user see screen!
           MOVE "Y" TO BLN-WAIT5SECONDS.

           PERFORM 0002-WAIT.
           PERFORM 0000-CLEARSCREEN.

      *    show intro screen 2
           PERFORM UNTIL ENDOFINTROFILE2 EQUALS 1
              DISPLAY INTRO2-LINE
              READ FILE-INTRO2 INTO REC-INTRO2
                   AT END MOVE 1 TO ENDOFINTROFILE2
              END-READ
           END-PERFORM.

           CLOSE FILE-INTRO2.

      *    LET USER SEE SCREEN
           PERFORM 0002-WAIT.
           PERFORM 0000-CLEARSCREEN.

       0002-WAIT.
      *    Created 01/08/2024 By Roger Williams
      *
      *    waits 5 seconds
           MOVE 0 TO INT-NUMWAIT.

      *     90000000 - 10 seconds
      *    wait so user can see screen
           PERFORM UNTIL INT-NUMWAIT EQUALS 50000000
             ADD 1 TO INT-NUMWAIT
           END-PERFORM.

       0003-SHOWROOM.
      *    Created 01/08/2024 By Roger Williams
      *
      *    shows room to user if INT-ROOMNUMBER EQUALS 1
      *    then shows FIRST room
      *

           IF INT-ROOMNUMBER EQUALS 1 THEN
              DISPLAY STR-DESC-INT1(1)
              DISPLAY STR-DESC-INT2(1)
              DISPLAY STR-DESC-INT3(1)
              DISPLAY STR-DESC-INT4(1)
              DISPLAY STR-DESC-INT5(1)
              DISPLAY STR-DESC-INT6(1)
              DISPLAY STR-DESC-INT7(1)
              DISPLAY STR-DESC-INT8(1)
              DISPLAY STR-DESC-INT9(1)
              DISPLAY STR-DESC-INT10(1)
              DISPLAY STR-DESC-INT11(1)
              DISPLAY STR-DESC-INT12(1)
              DISPLAY STR-DESC-INT13(1)
              DISPLAY STR-DESC-INT14(1)
              DISPLAY STR-DESC-INT15(1)
              DISPLAY STR-DESC-INT16(1)
              DISPLAY STR-DESC-INT17(1)
              DISPLAY STR-DESC-INT18(1)
              DISPLAY STR-DESC-INT19(1)
              DISPLAY STR-DESC-INT20(1)
              DISPLAY STR-DESC-INT21(1)
              DISPLAY STR-DESC-INT22(1)
              DISPLAY STR-DESC-INT23(1)
              DISPLAY STR-DESC-INT24(1)
              DISPLAY STR-DESC-INT25(1)
              MOVE ZERO TO INT-ROOMNUMBER
           ELSE
              DISPLAY STR-DESC-INT1(INT-CURROOM)
              DISPLAY STR-DESC-INT2(INT-CURROOM)
              DISPLAY STR-DESC-INT3(INT-CURROOM)
              DISPLAY STR-DESC-INT4(INT-CURROOM)
              DISPLAY STR-DESC-INT5(INT-CURROOM)
              DISPLAY STR-DESC-INT6(INT-CURROOM)
              DISPLAY STR-DESC-INT7(INT-CURROOM)
              DISPLAY STR-DESC-INT8(INT-CURROOM)
              DISPLAY STR-DESC-INT9(INT-CURROOM)
              DISPLAY STR-DESC-INT10(INT-CURROOM)
              DISPLAY STR-DESC-INT11(INT-CURROOM)
              DISPLAY STR-DESC-INT12(INT-CURROOM)
              DISPLAY STR-DESC-INT13(INT-CURROOM)
              DISPLAY STR-DESC-INT14(INT-CURROOM)
              DISPLAY STR-DESC-INT15(INT-CURROOM)
              DISPLAY STR-DESC-INT16(INT-CURROOM)
              DISPLAY STR-DESC-INT17(INT-CURROOM)
              DISPLAY STR-DESC-INT18(INT-CURROOM)
              DISPLAY STR-DESC-INT19(INT-CURROOM)
              DISPLAY STR-DESC-INT20(INT-CURROOM)
              DISPLAY STR-DESC-INT21(INT-CURROOM)
              DISPLAY STR-DESC-INT22(INT-CURROOM)
              DISPLAY STR-DESC-INT23(INT-CURROOM)
              DISPLAY STR-DESC-INT24(INT-CURROOM)
              DISPLAY STR-DESC-INT25(INT-CURROOM)
            END-IF.


       0004-CHECKIFEND.
      *    Created 01/08/2024 By Roger Williams
      *
      *    checks if game has ended if so sets
      *    BLN-ENDOFGAME to Y

      *    Return clsCurRoom.NextRoomEast EQUALS 0 And clsCurRoom.NextRoomNorth EQUALS 0 And clsCurRoom.NextRoomSouth EQUALS 0 And clsCurRoom.NextRoomWest EQUALS 0
           IF INT-NEXTROOMNORTH(INT-CURROOM) EQUALS ZERO AND
              INT-NEXTROOMSOUTH(INT-CURROOM) EQUALS ZERO AND
              INT-NEXTROOMEAST(INT-CURROOM) EQUALS ZERO AND
              INT-NEXTROOMWEST(INT-CURROOM) EQUALS ZERO THEN
              MOVE "Y" TO BLN-ENDOFGAME.

       0005-LOADLEVEL.
      *  'Created 23/07/2024 By Roger Williams
      *  '
      *  'loads level 1 from level1.txt into lstRooms which is a collection of clsGameRooms
      *  'level text file format matches the class structure
      *  '
           MOVE 1 TO INT-NUMROOMS
           OPEN INPUT FILE-LEVEL1.

      *    configure eof trap
           READ FILE-LEVEL1
                 AT END MOVE 1 TO ENDOFLEVELFILE
           END-READ.
      *    check file has data
           IF ENDOFLEVELFILE EQUALS 1
              DISPLAY "ERROR OPENING LEVEL FILE"
              STOP RUN
           END-IF.

           MOVE 1 TO INT-NUMROOMS.

           PERFORM UNTIL ENDOFLEVELFILE EQUALS 1
      *      store room data
             MOVE REC-ROOM-READ TO REC-ROOM(INT-NUMROOMS)
             INITIALISE REC-ROOM-READ

             READ FILE-LEVEL1 INTO REC-ROOM-READ
                   AT END
                      MOVE 1 TO ENDOFLEVELFILE
             END-READ
      *     increment room counter
             ADD 1 TO INT-NUMROOMS
           END-PERFORM.

           CLOSE FILE-LEVEL1.


       0006-INIT.
           PERFORM 0000-CLEARSCREEN.
           PERFORM 0005-LOADLEVEL.
           PERFORM 0001-SHOWINTRO.
      *    set current room number
           MOVE 1 TO INT-CURROOM.
           MOVE 1 TO INT-ROOMNUMBER.
           PERFORM 0003-SHOWROOM.

      *****************************************************************
      *    clsrogparser converted from visual basic
      *

      *internal lists converted into COBOL tables
      *

       0200-PARSEINPUT.
      *'Created 23/07/2024 By Roger Williams
      *  '
      *  'checks if text contains valid words e.g. nouns sets IsOk accordingly
      *  '
      *  'Rules
      *  '-----
      *  '
      *  'every phrase should contain a verb
      *  'every verb should either have an adjective e.g. open door
      *  'or
      *  'a preposition e.g. while
      *  'or
      *  'a noun e.g. key
      *  '
      *  'also handles user help requests, valid request string are:
      *  '
      *  'HELP
      *  '
      *  'HELP LIST <what>
      *  '
      *  '<what> types:
      *  '
      *  '          VERBS
      *  '          NOUNS
      *  '          ADJECTIVES
      *  '          PREPOSITIONS
      *  '          DIRECTIONS
      *  '
      *  '

           IF FUNCTION LENGTH(STR-INPUT) NOT EQUAL ZERO THEN
      *     clear last data
              MOVE SPACES TO STR-NOUN
              MOVE SPACES TO STR-VERB
              MOVE SPACES TO STR-ADJECTIVE
              MOVE SPACES TO STR-PREPOSITION
              MOVE SPACES TO STR-DIRECTION
              MOVE SPACES TO STR-RESULT1
              MOVE SPACES TO STR-RESULT2
              MOVE SPACES TO STR-RESULT3
              MOVE SPACES TO STR-RESULT4

              MOVE 1 TO INT-START
      *     separate into vars what a cool function!
              UNSTRING STR-INPUT DELIMITED BY ALL SPACES
                       INTO
                       STR-RESULT1,
                       STR-RESULT2,
                       STR-RESULT3,
                       STR-RESULT4
                       POINTER INT-START
                       TALLYING INT-COUNT
              END-UNSTRING
           END-IF.

      *    get STR-RESULT2 length

              MOVE 1 TO INT-LENGTH
              MOVE STR-RESULT2 TO STR-GETLENGTH
              PERFORM 0500-GETSTRINGLENGTH

           IF STR-RESULT1 EQUALS "help" THEN
               IF INT-LENGTH EQUALS ZERO THEN
                  PERFORM 0201-HELP-LIST
                  MOVE "Y" TO BLN-ISOK
               ELSE
      *            if phrase user type starts with HELP has second word
                   IF STR-RESULT2 EQUALS "list" THEN
                      EVALUATE STR-RESULT3
                          WHEN "verbs"
                            MOVE 0 TO BYT-WHAT
                            MOVE "Y" TO BLN-ISOK
                          WHEN "nouns"
                            MOVE 1 TO BYT-WHAT
                            MOVE "Y" TO BLN-ISOK
                          WHEN "adjectives"
                            MOVE 2 TO BYT-WHAT
                            MOVE "Y" TO BLN-ISOK
                          WHEN "prepositions"
                            MOVE 3 TO BYT-WHAT
                            MOVE "Y" TO BLN-ISOK
                          WHEN "directions"
                            MOVE 4 TO BYT-WHAT
                            MOVE "Y" TO BLN-ISOK
                          WHEN OTHER
                            MOVE "N" TO BLN-ISOK
                      END-EVALUATE
      *               show data to user
                      IF BLN-ISOK EQUALS "Y" THEN
                         PERFORM 0203-HELP-LISTVALIDWORDS
                      END-IF
                   END-IF
               END-IF
           ELSE
      *      'every phrase should contain a verb
      *      'every verb should either have an
      *      '
      *      'adjective e.g. door
      *      'or
      *      'a preposition e.g. while
      *      'or
      *      'a noun e.g. key
      *      '

      *      check for each type of word
             MOVE ZERO TO BYT-WHAT
             PERFORM 0202-CONTAINSVALIDWORDS
             MOVE 1 TO BYT-WHAT
             PERFORM 0202-CONTAINSVALIDWORDS
             MOVE 2 TO BYT-WHAT
             PERFORM 0202-CONTAINSVALIDWORDS
             MOVE 3 TO BYT-WHAT
             PERFORM 0202-CONTAINSVALIDWORDS
             MOVE 4 TO BYT-WHAT
             PERFORM 0202-CONTAINSVALIDWORDS

      *      if found
             IF BLN-OK EQUALS "Y" THEN
                 MOVE "Y" TO BLN-ISOK
             ELSE
                 MOVE "N" TO BLN-ISOK
                 DISPLAY "Unrecognised command"
             END-IF
           END-IF.

       0201-HELP-LIST.
      *Created 24/07/2024 By Roger Williams
      *
      *Lists the available help options
      *
      *NOTE: for later phases could all these options be shown in SECOND console?
      *

           PERFORM 0000-CLEARSCREEN.

           DISPLAY "Help Options".
           DISPLAY "=================================================".
           DISPLAY SPACES.
           DISPLAY "List adjectives          - help list adjectives".
           DISPLAY "List verbs               - help list verbs".
           DISPLAY "List nouns               - help list nouns".
           DISPLAY "List prepositions        - help list prepositions".
           DISPLAY "List movement directions - help list directions".
           DISPLAY " ".
           DISPLAY "Type: exit - at any time to end the game".
           DISPLAY SPACES.

           MOVE 1 TO INT-NUM.
           COMPUTE INT-NUM2 = 30 - 12.

      *    "scroll" text to top of display
           PERFORM 0502-PRINTBLANKLINES WITH TEST BEFORE
                   UNTIL INT-NUM GREATER THAN INT-NUM2.

       0202-CONTAINSVALIDWORDS.
      * 'Created 23/07/2024 By Roger Williams
      * '
      * 'checks if strPhrase contains verb,noun,adjective,preposition,direction
      * '
      * 'VARS
      * '
      * 'strWhat    : what to search
      * 'bytWhat    : what to check for (enum) verb,noun etc
      * '
      * 'returns true if finds valid phrase/word
      * 'also populates 05 class vars:
      * '
      * 'noun
      * 'verb
      * 'adjective
      * 'preposition
      * 'direction
      * '
           MOVE "N" TO BLN-ISOK.

             EVALUATE BYT-WHAT
               WHEN 0
                 MOVE 1 TO INT-NUM

                 PERFORM 0204-LOOKFORWORDTYPE WITH TEST AFTER
                 UNTIL INT-NUM GREATER THAN INT-NUMVERBS

               WHEN 1
                 MOVE 1 TO INT-NUM

                 PERFORM 0204-LOOKFORWORDTYPE WITH TEST AFTER
                 UNTIL INT-NUM GREATER THAN INT-NUMNOUNS

               WHEN 2
                 MOVE 1 TO INT-NUM

                 PERFORM 0204-LOOKFORWORDTYPE WITH TEST AFTER
                 UNTIL INT-NUM GREATER THAN INT-NUMVADJECTIVES

               WHEN 3
                 MOVE 1 TO INT-NUM

                 PERFORM 0204-LOOKFORWORDTYPE WITH TEST AFTER
                 UNTIL INT-NUM GREATER THAN INT-NUMPREPOSITIONS

               WHEN 4
                 MOVE 1 TO INT-NUM

                 PERFORM 0204-LOOKFORWORDTYPE WITH TEST AFTER
                 UNTIL INT-NUM GREATER THAN INT-NUMDIRECTIONS
           END-EVALUATE.



       0203-HELP-LISTVALIDWORDS.
      *Created 23/07/2024 By Roger Williams
      *
      *when users types: HELP LIST VERBS
      *
      *runs this sub which shows them on the console
      *
      *VARS
      *
      *bytWhat    : what to show (uses enum) 0EQUALSverb 1EQUALSnoun etc.

      *make sure help is only text on screen
           PERFORM 0000-CLEARSCREEN.

           MOVE 1 TO INT-NUM1.
           MOVE ZERO TO INT-NUM2.
           MOVE 1 TO INT-NUM3.
           MOVE ZERO TO INT-LINESPRINTED.

           MOVE SPACES TO STR-OUTPUT.

           EVALUATE BYT-WHAT
             WHEN ZERO
                MOVE INT-NUMVERBS TO INT-NUM3
                DISPLAY "Valid Verbs"
                DISPLAY "**********************************************"
             WHEN 1
                MOVE INT-NUMNOUNS TO INT-NUM3
                DISPLAY "Valid Nouns"
                DISPLAY "**********************************************"
             WHEN 2
                MOVE INT-NUMVADJECTIVES TO INT-NUM3
                DISPLAY "Valid Adjectives"
                DISPLAY "**********************************************"
             WHEN 3
                MOVE INT-NUMPREPOSITIONS TO INT-NUM3
                DISPLAY "Valid Prepositions"
                DISPLAY "**********************************************"
             WHEN 4
                MOVE INT-NUMDIRECTIONS TO INT-NUM3
                DISPLAY "Valid Directions"
                DISPLAY "**********************************************"
           END-EVALUATE.

      *print list contents to console
           PERFORM 0501-HELP-LISTVALIDWORDSDISPLAY WITH TEST AFTER
                   UNTIL INT-NUM1 GREATER THAN INT-NUM3.

           MOVE 1 TO INT-NUM.
      *    calculate number of blank lines
           COMPUTE INT-NUM2 = 26 -INT-LINESPRINTED.
      *    "scroll" text to top of display
           PERFORM 0502-PRINTBLANKLINES WITH TEST AFTER
                   UNTIL INT-NUM EQUAL TO INT-NUM2.

       0204-LOOKFORWORDTYPE.
      *    Created 12/08/2024 By Roger Williams
      *
      *    Compares table list at index INT-NUM with each of the strings
      *    populated by the UNSTRING command to look for a match
      *
      *    Repeatedly called by: 0202-CONTAINSVALIDWORDS
      *
           EVALUATE BYT-WHAT
               WHEN 0
                    IF LST-VERBS(INT-NUM) EQUALS STR-RESULT1 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT1 TO STR-VERB
                    END-IF
                    IF LST-VERBS(INT-NUM) EQUALS STR-RESULT2 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT2 TO STR-VERB
                    END-IF
                    IF LST-VERBS(INT-NUM) EQUALS STR-RESULT3 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT3 TO STR-VERB
                    END-IF
                    IF LST-VERBS(INT-NUM) EQUALS STR-RESULT4 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT4 TO STR-VERB
                    END-IF

                 ADD 1 TO INT-NUM
               WHEN 1
                    IF LST-NOUNS(INT-NUM) EQUALS STR-RESULT1 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT1 TO STR-NOUN
                    END-IF
                    IF LST-NOUNS(INT-NUM) EQUALS STR-RESULT2 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT2 TO STR-NOUN
                    END-IF
                    IF LST-NOUNS(INT-NUM) EQUALS STR-RESULT3 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT3 TO STR-NOUN
                    END-IF
                    IF LST-NOUNS(INT-NUM) EQUALS STR-RESULT4 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT4 TO STR-NOUN
                    END-IF

                 ADD 1 TO INT-NUM
               WHEN 2
                    IF LST-ADJECTIVES(INT-NUM) EQUALS STR-RESULT1 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT1 TO STR-ADJECTIVE
                    END-IF
                    IF LST-ADJECTIVES(INT-NUM) EQUALS STR-RESULT2 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT2 TO STR-ADJECTIVE
                    END-IF
                    IF LST-ADJECTIVES(INT-NUM) EQUALS STR-RESULT3 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT3 TO STR-ADJECTIVE
                    END-IF
                    IF LST-ADJECTIVES(INT-NUM) EQUALS STR-RESULT4 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT4 TO STR-ADJECTIVE
                    END-IF

                 ADD 1 TO INT-NUM

               WHEN 3
                    IF LST-PREPOSITIONS(INT-NUM) EQUALS STR-RESULT1 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT1 TO STR-PREPOSITION
                    END-IF
                    IF LST-PREPOSITIONS(INT-NUM) EQUALS STR-RESULT2 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT2 TO STR-PREPOSITION
                    END-IF
                    IF LST-PREPOSITIONS(INT-NUM) EQUALS STR-RESULT3 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT3 TO STR-PREPOSITION
                    END-IF
                    IF LST-PREPOSITIONS(INT-NUM) EQUALS STR-RESULT4 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT4 TO STR-PREPOSITION
                    END-IF

                    ADD 1 TO INT-NUM
               WHEN 4
                    IF LST-DIRECTIONS(INT-NUM) EQUALS STR-RESULT1 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT1 TO STR-DIRECTION
                    END-IF
                    IF LST-DIRECTIONS(INT-NUM) EQUALS STR-RESULT2 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT2 TO STR-DIRECTION
                    END-IF
                    IF LST-DIRECTIONS(INT-NUM) EQUALS STR-RESULT3 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT3 TO STR-DIRECTION
                    END-IF
                    IF LST-DIRECTIONS(INT-NUM) EQUALS STR-RESULT4 THEN
                       MOVE "Y" TO BLN-OK
                       MOVE STR-RESULT4 TO STR-DIRECTION
                    END-IF

                    ADD 1 TO INT-NUM
           END-EVALUATE.

       0205-LOOKFORROOM.
      *     created 12/08/2024 By Roger Williams
      *
      *    looks through the room ID list for a value matching
      *    INT-CURROOM if found sets INT-CURROOM to the room id value
      *
      *    NOTE: this is a safeguard as not all rooms will be in straight
      *          list index order the level layout is like a binary tree
      *          so pays to double check
      *
           IF INT-CURROOM EQUALS INT-ROOMID(INT-NUM1) THEN
              MOVE INT-NUM1 TO INT-CURROOM
           END-IF

           ADD 1 TO INT-NUM1.

      *    custom function
       0500-GETSTRINGLENGTH.
      *    created 12/08/2024 By Roger Williams
      *
      *    COBOL equivalent of modern LENGTH function
      *
      *    takes string put into STR-TEMP1 and returns length in
      *    INT-LENGTH
      *
      *
           MOVE ZERO TO INT-COUNTOFLEADINGSPACES.
      *    get amount of UNUSED characters in the string
           INSPECT FUNCTION REVERSE (STR-GETLENGTH)
                    TALLYING INT-COUNTOFLEADINGSPACES
           FOR LEADING SPACE.
      *    use that value to get string length
           SUBTRACT INT-COUNTOFLEADINGSPACES
                   FROM FUNCTION LENGTH(STR-GETLENGTH)
           GIVING INT-LENGTH.

       0501-HELP-LISTVALIDWORDSDISPLAY.
      *    created 12/08/2024 by Roger Williams
      *
      *    repeatedly called by 0203-HELP-LISTVALIDWORDS
      *    shows contents of required list oneline at a time
      *
           EVALUATE BYT-WHAT
             WHEN ZERO
               IF INT-NUM2 NOT EQUAL TO 6 THEN
                  DISPLAY LST-VERBS(INT-NUM1) WITH NO ADVANCING
                  ADD 1 TO INT-NUM2
               END-IF

      *print string when 10 commands in it to stop unwanted word wrap
               IF INT-NUM2 EQUALS 6 THEN
      *reset vars
                   MOVE ZERO TO INT-NUM2
                   ADD 1 TO INT-LINESPRINTED
               END-IF
             WHEN 1
               IF INT-NUM2 NOT EQUAL TO 6 THEN
                  DISPLAY LST-NOUNS(INT-NUM1) WITH NO ADVANCING
                  ADD 1 TO INT-NUM2
               END-IF

      *print string when 10 commands in it to stop unwanted word wrap
               IF INT-NUM2 EQUALS 6 THEN
      *reset vars
                   MOVE ZERO TO INT-NUM2
                   ADD 1 TO INT-LINESPRINTED
               END-IF
             WHEN 2
               IF INT-NUM2 NOT EQUAL TO 6 THEN
                  DISPLAY LST-ADJECTIVES(INT-NUM1) WITH NO ADVANCING
                  ADD 1 TO INT-NUM2
               END-IF

      *print string when 10 commands in it to stop unwanted word wrap
               IF INT-NUM2 EQUALS 6 THEN
      *reset vars
                   MOVE ZERO TO INT-NUM2
                   ADD 1 TO INT-LINESPRINTED
               END-IF
             WHEN 3
               IF INT-NUM2 NOT EQUAL TO 6 THEN
                  DISPLAY LST-PREPOSITIONS(INT-NUM1) WITH NO ADVANCING
                  ADD 1 TO INT-NUM2
               END-IF

      *print string when 10 commands in it to stop unwanted word wrap
               IF INT-NUM2 EQUALS 6 THEN
      *reset vars
                   MOVE ZERO TO INT-NUM2
                   ADD 1 TO INT-LINESPRINTED
               END-IF
             WHEN 4
               IF INT-NUM2 NOT EQUAL TO 6 THEN
                  DISPLAY LST-DIRECTIONS(INT-NUM1) WITH NO ADVANCING
                  ADD 1 TO INT-NUM2
               END-IF

      *print string when 10 commands in it to stop unwanted word wrap
               IF INT-NUM2 EQUALS 6 THEN
      *reset vars
                   MOVE ZERO TO INT-NUM2
                   ADD 1 TO INT-LINESPRINTED
               END-IF
           END-EVALUATE.

           ADD 1 TO INT-NUM1.


       0502-PRINTBLANKLINES.
      *    Created 12/08/2024 By Roger Williams
      *
      *    prints blank line repeatedly called by:
      *    0201-HELP-LIST
      *    0201-HELP-LISTVALIDWORDS
           DISPLAY SPACES.
           ADD 1 TO INT-NUM.


       END PROGRAM ROGsTextAdventureCOBOL.
