       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAPR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIC-FILE ASSIGN TO "GPL_3.TXT"
               ORGANIZATION IS SEQUENTIAL.       

       DATA DIVISION.
       FILE SECTION.
       FD  LIC-FILE
               LABEL RECORDS ARE STANDARD.
       01  LIC-CONTENT.
           05  LIC-CHAR     PIC X.

       WORKING-STORAGE SECTION.
       77  EMPTY-LINE       PIC X      VALUE " ".

       01  START-INFO.
           05  ST-NAME       PIC X(28)  
               VALUE "Handle Project Version Alpha".
           05  ST-COPYRIGHT  PIC X(34)  
               VALUE "Copyright (C) 2013 Marcus Pedersen".
           05  ST-LIC1       PIC X(39)      
               VALUE "This program is free software: you can ".
           05  ST-LIC1-2      PIC X(29) 
               VALUE "redistribute it and/or modify".
           05  ST-LIC2       PIC X(38)
               VALUE "it under the terms of the GNU General ".
           05  ST-LIC2-2     PIC X(30)
               VALUE "Public License as published by".
           05  ST-LIC3       PIC X(37)
               VALUE "the Free Software Foundation, either ". 
           05  ST-LIC3-2     PIC X(28)
               VALUE "version 3 of the License, or".
           05  ST-LIC4       PIC X(35)
               VALUE "(at your option) any later version.".

       01  MAIN-MENU.
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)     VALUE ALL "*".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)     
               VALUE "*         MAIN MENU:         *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)     VALUE ALL "*".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)         
               VALUE "* 1 - List all projects      *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)
               VALUE "* 2 - Add new project        *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)
               VALUE "* 3 - List closed projects   *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)
               VALUE "* 4 - Show Licence (GPL V3)  *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)     VALUE ALL "*".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)
               VALUE "* 0 - Exit application       *".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(30)     VALUE ALL "*".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  MM-ERROR       PIC X(50)     VALUE " ".
           05  FILLER         PIC X(2)      VALUE X'0D0A'.
           05  FILLER         PIC X(15)     VALUE "Choose number: ".
 
       77  MAIN-MENU-ACCEPT   PIC X.
       77  ACCEPT-DUMMY       PIC X.



       01  LIC-FILE-VARIABLES.
           05  LIC-EOF            PIC X         VALUE "N".
           05  LIC-CHAR-COUNT     PIC 999       VALUE ZERO.

       01  PROJECT-ENTRY.
           05  PE-ACCEPT           PIC X(80)     VALUE SPACE.
           05  PE-DATE.
               10  PE-DATE-YEAR    PIC 9(4)      VALUE ZERO.
               10  PE-DATE-MONTH   PIC 9(2)      VALUE ZERO.
               10  PE-DATE-DAY     PIC 9(2)      VALUE ZERO.
           05  PE-YEAR             PIC 9(4)      VALUE ZERO.
           05  PE-LEAP-YEAR        PIC 9         VALUE ZERO.
           05  PE-DATE-SWITCH      PIC X         VALUE "N".
               88  PE-DATE-OK                    VALUE "Y".    
           05  PE-DESCRIPTION      PIC X(800)    VALUE SPACE.
           05  PE-CANCEL-SWITCH    PIC X         VALUE "N".
               88 PE-CANCEL                      VALUE "Y".
  
       PROCEDURE DIVISION.
           PERFORM WRITE-START-INFO.
           PERFORM WRITE-MAIN-MENU.

       STOP RUN.

       WRITE-START-INFO.
           DISPLAY EMPTY-LINE.
           DISPLAY ST-NAME.
           DISPLAY ST-COPYRIGHT.
           DISPLAY EMPTY-LINE.
           DISPLAY ST-LIC1 ST-LIC1-2.
           DISPLAY ST-LIC2 ST-LIC2-2.
           DISPLAY ST-LIC3 ST-LIC3-2.
           DISPLAY ST-LIC4.
           DISPLAY EMPTY-LINE.

       WRITE-MAIN-MENU.
           DISPLAY MAIN-MENU.
           ACCEPT MAIN-MENU-ACCEPT.

           EVALUATE MAIN-MENU-ACCEPT
               WHEN "1"
                   MOVE EMPTY-LINE TO MM-ERROR
                   PERFORM VIEW-PROJECTS
                   PERFORM WRITE-MAIN-MENU
	       WHEN "2"
                   MOVE EMPTY-LINE TO MM-ERROR
                   PERFORM ADD-NEW-PROJECT
                   PERFORM WRITE-MAIN-MENU
               WHEN "3"
                   DISPLAY "YOU CHOOSE: " MAIN-MENU-ACCEPT
                   MOVE EMPTY-LINE TO MM-ERROR
                   PERFORM WRITE-MAIN-MENU
               WHEN "4"
                   OPEN INPUT LIC-FILE
                   PERFORM PRINT-LICENCE-FILE
                       UNTIL LIC-EOF = "Y"
                   CLOSE LIC-FILE
                   MOVE "N" TO LIC-EOF
                   PERFORM WRITE-MAIN-MENU
               WHEN "0"
                   DISPLAY "Exit..."
               WHEN OTHER
                   MOVE "Wrong entry ... try again" TO MM-ERROR
                   PERFORM WRITE-MAIN-MENU.

       ADD-NEW-PROJECT.
           CALL "PROJADD" USING PE-CANCEL-SWITCH.
           IF PE-CANCEL
               MOVE "Add new project canceled..." TO MM-ERROR
           ELSE
               MOVE "New project added." TO MM-ERROR.

       VIEW-PROJECTS.
           CALL "PROJVIEW".

       PRINT-LICENCE-FILE.
           READ LIC-FILE
               AT END
               MOVE "Y" TO LIC-EOF.
           ADD 1 TO LIC-CHAR-COUNT.

           IF LIC-EOF NOT = "Y"
               DISPLAY LIC-CHAR WITH NO ADVANCING
               IF LIC-CHAR-COUNT >= 750 AND
                   LIC-CHAR = " "
                   DISPLAY " "
                   DISPLAY " "
                   DISPLAY "Press enter to continue ..."
                   ACCEPT ACCEPT-DUMMY
                   INITIALIZE LIC-CHAR-COUNT.
