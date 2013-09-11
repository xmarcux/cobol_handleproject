       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJVIEW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FCPROJECT.COB".

       DATA DIVISION.
       FILE SECTION.
           COPY "FDPROJECT.COB".

       WORKING-STORAGE SECTION.
       01  CURRENT-DATE.
           05  CD-YEAR        PIC 9(4).
           05  CD-MONTH       PIC 9(2).
           05  CD-DAY         PIC 9(2).

       77  PROJECT-COUNT      PIC 999      VALUE 1.
       77  ACCEPT-DUMMY       PIC X.
       77  ACCEPT-PROJECT-NO  PIC X(80)    VALUE SPACE.
       77  DESC-COUNT         PIC 99       VALUE ZERO.

       01  SWITCHES.
           05  CANCEL-VIEW-SW PIC X        VALUE "N".
               88  CANCEL-VIEW             VALUE "Y".

       01  PROJECT-FILE-SW    PIC X        VALUE "N".
           88  PROJECT-FILE-AT-END         VALUE "Y".

       01  DISPLAY-ONE-PROJECT-NO.
           05  DOP-BUDGET     PIC SZ(9)9.99 VALUE ZERO.
           05  DOP-COST       PIC SZ(9)9.99 VALUE ZERO.
           05  DOP-PROFIT     PIC SZ(9)9.99 VALUE ZERO.

       01  DISPLAY-PROJECT-NUMBER.
           05  DPN-SYMBOL     PIC X(4)     VALUE "  - ".
           05  DPN-NUMBER     PIC X(69)    VALUE SPACE.

       01  DISPLAY-PROJECT.
           05  FILLER         PIC X(4)     VALUE SPACE.
           05  DP-NAME        PIC X(20)    VALUE SPACE.
           05  FILLER         PIC X        VALUE SPACE.
           05  DP-DESCRIPTION PIC X(35)    VALUE SPACE.
           05  FILLER         PIC X        VALUE SPACE.
           05  DP-START-DATE  PIC ZZ9(8)   VALUE ZERO.

       01  PROJ-NUMBER-HEAD.
           05  PROJ-NUMBER    PIC X(18)    VALUE "* - Project number".
           05  FILLER         PIC X(53)    VALUE SPACE.
           05  FILLER         PIC X        VALUE "*".
      
       01  HEAD-LINE.
           05  FILLER         PIC X(4)     VALUE "*   ".
           05  HL-NAME        PIC X(12)    VALUE "Project name".
           05  FILLER         PIC X(9)     VALUE SPACE.
           05  HL-DESCRIPTION PIC X(11)
                   VALUE "Description".
           05  FILLER         PIC X(25)    VALUE SPACE.
           05  HL-START-DATE  PIC X(11)
                   VALUE "Start date*".

       77  UNDERLINE-STAR      PIC X(72)    VALUE ALL "*".

       01  UNDERLINE-HEAD.
            05  FILLER         PIC X(4)     VALUE SPACE.
            05  UH-LINE        PIC X(68)    VALUE ALL "-".

       PROCEDURE DIVISION.
 
       PERFORM INIT-PROGRAM.
       PERFORM DISPLAY-HEADER.
       PERFORM READ-PROJECT
           UNTIL PROJECT-FILE-AT-END.
       PERFORM DISPLAY-MENU-EXPLAIN. 
       PERFORM PROGRAM-CLEANUP.

       EXIT PROGRAM.

       INIT-PROGRAM.
           OPEN INPUT PROJECT-FILE.
           MOVE 0 TO PROJECT-COUNT.
           MOVE "N" TO PROJECT-FILE-SW.
           MOVE SPACE TO DPN-NUMBER.
           MOVE SPACE TO ACCEPT-PROJECT-NO.
           MOVE "N" TO CANCEL-VIEW-SW.
           MOVE 1 TO DESC-COUNT.
           MOVE ZERO TO DISPLAY-ONE-PROJECT-NO.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE.

       PROGRAM-CLEANUP.
           CLOSE PROJECT-FILE.

       DISPLAY-HEADER.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY UNDERLINE-STAR.
           DISPLAY PROJ-NUMBER-HEAD.
           DISPLAY HEAD-LINE.
           DISPLAY UNDERLINE-STAR.

       DISPLAY-MENU-EXPLAIN.
           DISPLAY " ".
           DISPLAY "! = Project has past end date!".
           DISPLAY " ".
           DISPLAY "End of projects ...".
           PERFORM DISPLAY-MENU.

       DISPLAY-MENU.
           DISPLAY " ".
           DISPLAY 
            "To show project details enter project number:".
           DISPLAY "(Enter -1 to get back to main menu)".
           PERFORM GET-PROJECT-NO
               UNTIL CANCEL-VIEW.

       READ-PROJECT.
           IF PROJECT-COUNT NOT = 6
               READ PROJECT-FILE NEXT
                   AT END SET PROJECT-FILE-AT-END TO TRUE.
	       
           IF NOT PROJECT-FILE-AT-END
              AND PROJECT-COUNT NOT = 6
               PERFORM IS-PROJECT-LATE
               PERFORM DISPLAY-ONE-PROJECT

           IF PROJECT-COUNT = 6 AND
              NOT PROJECT-FILE-AT-END
               PERFORM DISPLAY-MORE-PROJECTS.

       IS-PROJECT-LATE.
           IF PR-DATE (2) <= CURRENT-DATE
               MOVE "! - " TO DPN-SYMBOL
           ELSE
               MOVE "  - " TO DPN-SYMBOL.

       DISPLAY-ONE-PROJECT.
           ADD 1 TO PROJECT-COUNT.

           MOVE PR-NUMBER          TO DPN-NUMBER.
           MOVE PR-NAME            TO DP-NAME.
           MOVE PR-DESCRIPTION (1) TO DP-DESCRIPTION.
           MOVE PR-DATE (1)        TO DP-START-DATE.

           DISPLAY DISPLAY-PROJECT-NUMBER.
           DISPLAY DISPLAY-PROJECT.
           DISPLAY UNDERLINE-HEAD.

       DISPLAY-MORE-PROJECTS.
           MOVE 0 TO PROJECT-COUNT.
           DISPLAY 
           "! = Project has past end date!  Enter to continue ...".
           ACCEPT ACCEPT-DUMMY.
           PERFORM DISPLAY-HEADER.

       GET-PROJECT-NO.
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1"
               SET CANCEL-VIEW TO TRUE
           ELSE
               MOVE ACCEPT-PROJECT-NO
                    TO PR-NUMBER
               READ PROJECT-FILE
                   INVALID KEY PERFORM READ-KEY-ERROR
                   NOT INVALID KEY
                       PERFORM SHOW-COMPLETE-PROJECT.

       READ-KEY-ERROR.
           DISPLAY "Project number not found, please try again ...".

       SHOW-COMPLETE-PROJECT.
           DISPLAY "Project number:".
           DISPLAY PR-NUMBER.
           DISPLAY "Project name:".
           DISPLAY PR-NAME.
           DISPLAY "Description:".
           PERFORM SHOW-DESCRIPTION
               UNTIL DESC-COUNT = 10.
           MOVE 1 TO DESC-COUNT.
           DISPLAY "Project leader name:".
           DISPLAY PR-LEADER-NAME.
           DISPLAY "Project leader surname:".
           DISPLAY PR-LEADER-SURNAME.
           DISPLAY " ".
           DISPLAY "Start date:".
           DISPLAY PR-DATE (1).
           DISPLAY "End date:".
           DISPLAY PR-DATE (2).
           DISPLAY "Enter to continue ...".
           ACCEPT ACCEPT-DUMMY.
           DISPLAY "Budget:".
           MOVE PR-BUDGET TO DOP-BUDGET.
           DISPLAY DOP-BUDGET.
           DISPLAY "Cost:".
           MOVE PR-COST TO DOP-COST.
           DISPLAY DOP-COST.
           COMPUTE DOP-PROFIT = PR-BUDGET - PR-COST.
           DISPLAY "Profit:".
           DISPLAY DOP-PROFIT.

           PERFORM DISPLAY-MENU.

       SHOW-DESCRIPTION.
           DISPLAY PR-DESCRIPTION (DESC-COUNT).
           ADD 1 TO DESC-COUNT.