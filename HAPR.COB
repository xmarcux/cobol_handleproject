       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAPR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LIC-FILE ASSIGN TO "GPL_3.TXT"
               ORGANIZATION IS SEQUENTIAL.       

           COPY "FCPROJECT.COB".

       DATA DIVISION.
       FILE SECTION.
       FD  LIC-FILE
               LABEL RECORDS ARE STANDARD.
       01  LIC-CONTENT.
           05  LIC-CHAR     PIC X.

           COPY "FDPROJECT.COB".

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
           05  ST-LIC5       PIC X(35)
               VALUE "This program is distributed in the ".
           05  ST-LIC5-2     PIC X(28)
               VALUE "hope that it will be useful,".
           05  ST-LIC6       PIC X(34)
               VALUE "but WITHOUT ANY WARRANTY; without ".
           05  ST-LIC6-2     PIC X(28)
               VALUE "even the implied warranty of".
           05  ST-LIC7       PIC X(33)
               VALUE "MERCHANTABILITY or FITNESS FOR A ".
           05  ST-LIC7-2     PIC X(28)
               VALUE "PARTICULAR PURPOSE.  See the".
           05  ST-LIC8       PIC X(44)
               VALUE "GNU General Public License for more details.".
           05  ST-LIC9       PIC X(43)
               VALUE "You should have received a copy of the GNU ".
           05  ST-LIC9-2     PIC X(22)
               VALUE "General Public License".
           05  ST-LIC10      PIC X(34)
               VALUE "along with this program.  If not, ".
           05  ST-LIC10-2    PIC X(35)
               VALUE "see <http://www.gnu.org/licenses/>.".

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
           05  PE-ACCEPT-DATE      PIC 9(8)      VALUE ZERO.
           05  PE-YEAR             PIC 9(4)      VALUE ZERO.
           05  PE-LEAP-YEAR        PIC 9         VALUE ZERO.
           05  PE-MONTH            PIC 9(2)      VALUE ZERO.
           05  PE-DAY              PIC 9(2)      VALUE ZERO.
           05  PE-DATE-SWITCH      PIC X         VALUE "N".
               88  PE-DATE-OK                    VALUE "Y".    
           05  PE-DESCRIPTION      PIC X(800)    VALUE SPACE.
           05  PE-CANCEL-SWITCH    PIC X         VALUE "N".
               88 PE-CANCEL                      VALUE "Y".
  
       PROCEDURE DIVISION.
           PERFORM INIT-PROGRAM.           
           PERFORM WRITE-START-INFO.
           PERFORM WRITE-MAIN-MENU.
           PERFORM CLEAR-PROGRAM.

       STOP RUN.

       INIT-PROGRAM.
           OPEN I-O PROJECT-FILE.

       CLEAR-PROGRAM.
           CLOSE PROJECT-FILE.

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
           DISPLAY ST-LIC5 ST-LIC5-2.
           DISPLAY ST-LIC6 ST-LIC6-2.
           DISPLAY ST-LIC7 ST-LIC7-2.
           DISPLAY ST-LIC8.
           DISPLAY EMPTY-LINE.
           DISPLAY ST-LIC9 ST-LIC9-2.
           DISPLAY ST-LIC10 ST-LIC10-2.
           DISPLAY EMPTY-LINE.

       WRITE-MAIN-MENU.
           DISPLAY MAIN-MENU.
           ACCEPT MAIN-MENU-ACCEPT.

           EVALUATE MAIN-MENU-ACCEPT
               WHEN "1"
                   DISPLAY "YOU CHOOSE: " MAIN-MENU-ACCEPT
                   MOVE EMPTY-LINE TO MM-ERROR
                   PERFORM WRITE-MAIN-MENU
	       WHEN "2"
                   MOVE EMPTY-LINE TO MM-ERROR
                   PERFORM ADD-NEW-PROJECT
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

        ADD-NEW-PROJECT.
            DISPLAY EMPTY-LINE.
            DISPLAY "******************************".
            DISPLAY "*      Add new project       *".
            DISPLAY "*Enter zero (0) and press    *".
            DISPLAY "*enter at any input to cancel*".
            DISPLAY "*and return to main menu.    *".
            DISPLAY "******************************".
            DISPLAY EMPTY-LINE.
            PERFORM GET-PROJECT-INFO.
            PERFORM WRITE-MAIN-MENU.

       GET-PROJECT-INFO.
           PERFORM GET-PROJECT-NUMBER.
           IF NOT PE-CANCEL
               PERFORM GET-PROJECT-NAME.
               IF NOT PE-CANCEL
                   PERFORM GET-PROJECT-DESCRIPTION
                   PERFORM GET-PROJECT-FIRST-NAME.
                   IF NOT PE-CANCEL
                       PERFORM GET-PROJECT-SURNAME.
                       IF NOT PE-CANCEL
                           PERFORM GET-PROJECT-START-DATE.
           IF PE-CANCEL
               MOVE "Add new project canceled..." TO MM-ERROR
           ELSE
               MOVE "New project added." TO MM-ERROR.

           MOVE "N" TO  PE-CANCEL-SWITCH.

       GET-PROJECT-NUMBER.
            DISPLAY "Enter project number:".
            ACCEPT PE-ACCEPT.
            MOVE PE-ACCEPT TO PR-NUMBER.
            PERFORM CHECK-CANCEL-INPUT.

       GET-PROJECT-NAME.
           DISPLAY "Enter project name:"
           ACCEPT PE-ACCEPT.
           MOVE PE-ACCEPT TO PR-NAME.
           PERFORM CHECK-CANCEL-INPUT.

       GET-PROJECT-DESCRIPTION.
           DISPLAY "Enter project description,".
           DISPLAY "continue to add lines until".
           DISPLAY "a line with a zero(0):"
           PERFORM GET-DESCRIPTION-ROWS
               UNTIL PE-CANCEL.
           MOVE PE-DESCRIPTION TO PR-DESCRIPTION.
	   MOVE "N" TO PE-CANCEL-SWITCH.

       GET-DESCRIPTION-ROWS.
           ACCEPT PE-ACCEPT.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT PE-CANCEL
               STRING PE-DESCRIPTION DELIMITED BY SIZE
                      PE-ACCEPT DELIMITED BY SIZE
                   INTO PE-DESCRIPTION.

       GET-PROJECT-FIRST-NAME.
           DISPLAY "Enter project leaders first name:".
           ACCEPT PE-ACCEPT.
           MOVE PE-ACCEPT TO PR-LEADER-FIRST-NAME.
           PERFORM CHECK-CANCEL-INPUT.

       GET-PROJECT-SURNAME.
           DISPLAY "Enter project leaders surname:".
           ACCEPT PE-ACCEPT.
           MOVE PE-ACCEPT TO PR-LEADER-SURNAME.
           PERFORM CHECK-CANCEL-INPUT.

       GET-PROJECT-START-DATE.
           DISPLAY "Enter start date in format YYYYMMDD:".
           PERFORM CHECK-DATE
               WITH TEST AFTER
               UNTIL PE-DATE-OK OR PE-CANCEL.

       CHECK-DATE.
           ACCEPT PE-ACCEPT-DATE.
           PERFORM CHECK-CANCEL-INPUT.
           COMPUTE PE-YEAR = PE-ACCEPT-DATE / 10000.
	   IF PE-YEAR >= 1900 AND <= 3000
               COMPUTE PE-MONTH = 
                       (PE-ACCEPT-DATE - PE-YEAR * 10000) / 100
               IF PE-MONTH >= 1 AND <= 12
                   COMPUTE PE-DAY =
                           (PE-ACCEPT-DATE - (PE-YEAR * 10000 +
                           PE-MONTH * 100))
                   MOVE FUNCTION REM (PE-YEAR 4) TO PE-LEAP-YEAR
                   IF PE-MONTH = 4 AND PE-DAY >= 1 AND <= 30
                       SET PE-DATE-OK TO TRUE
                       DISPLAY "APRIL"
                   ELSE IF PE-MONTH = 6  AND PE-DAY >= 1 AND <= 30
                       SET PE-DATE-OK TO TRUE
                   ELSE IF PE-MONTH = 9  AND PE-DAY >= 1 AND <= 30
                       SET PE-DATE-OK TO TRUE
                   ELSE IF PE-MONTH = 11 AND PE-DAY >= 1 AND <= 30
                       SET PE-DATE-OK TO TRUE
                   ELSE IF PE-MONTH = 2  AND PE-DAY >= 1 AND <= 29 
                           AND PE-LEAP-YEAR = 0
                       SET PE-DATE-OK TO TRUE
                   ELSE IF PE-MONTH = 2  AND PE-DAY >= 1 AND <= 28
                       SET PE-DATE-OK TO TRUE
                   ELSE IF PE-MONTH = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
                           AND PE-DAY >= 1 AND <= 31
                        SET PE-DATE-OK TO TRUE
                       DISPLAY "ALL OTHER MONTHS"
                   ELSE
                       MOVE "N" TO PE-DATE-SWITCH
               ELSE
                   MOVE "N" TO PE-DATE-SWITCH
           ELSE
               MOVE "N" TO PE-DATE-SWITCH.           
           
           IF NOT PE-DATE-OK
               DISPLAY "Wrong date format."
               DISPLAY "Format should be: YYYYMMDD"
               DISPLAY " ... try again.".

       CHECK-CANCEL-INPUT.
           IF PE-ACCEPT = "0"
               SET PE-CANCEL TO TRUE.
