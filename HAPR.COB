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

       01  NEW-PROJECT.
           05  NP-NUMBER      PIC X(10)     VALUE SPACES.
           05  NP-NAME        PIC X(20)     VALUE SPACES.
           05  NP-DESCRIPTION PIC X(200)    VALUE SPACES.
           05  NP-START-DATE  PIC X(10)     VALUE SPACES.
           05  NP-END-DATE    PIC X(10)     VALUE SPACES.
  
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
                   DISPLAY "YOU CHOOSE: " MAIN-MENU-ACCEPT
                   MOVE EMPTY-LINE TO MM-ERROR
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

