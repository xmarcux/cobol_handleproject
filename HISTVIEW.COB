       IDENTIFICATION DIVISION.
       PROGRAM-ID. HISTVIEW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FCHISTORY.COB".

       DATA DIVISION.
       FILE SECTION.
           COPY "FDHISTORY.COB".

       WORKING-STORAGE SECTION.
       01  CURRENT-DATE.
           05  CD-YEAR        PIC 9(4).
           05  CD-MONTH       PIC 9(2).
           05  CD-DAY         PIC 9(2).

       01  STATISTIC-VALUES.
           05  SV-PROJ-COUNT  PIC 9(7)     VALUE ZERO.
           05  SV-NO-LATE     PIC 9(7)     VALUE ZERO.
           05  SV-NO-PROFIT   PIC 9(7)     VALUE ZERO.
           05  SV-TOT-BUDGET  PIC 9(15)V99 VALUE ZERO.
           05  SV-TOT-COST    PIC 9(15)V99 VALUE ZERO.

       01  STATISTIC-VALUES-DISPLAY.
           05  SVD-PROJ-COUNT      PIC Z(6)9      VALUE ZERO.
           05  SVD-NO-LATE         PIC Z(6)9      VALUE ZERO.
           05  SVD-NO-PROFIT       PIC Z(6)9      VALUE ZERO.
           05  SVD-NO-PROFIT-PRCNT PIC SZZ9.99    VALUE ZERO.
           05  SVD-TOT-BUDGET      PIC Z(14)9.99  VALUE ZERO.
           05  SVD-TOT-COST        PIC Z(14)9.99  VALUE ZERO.
           05  SVD-TOT-PROFIT      PIC Z(14)9.99  VALUE ZERO.
           05  SVD-PROFIT-PRCNT    PIC SZZ9.99    VALUE ZERO.

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

       01  HEAD-HISTORY.
           05  FILLER         PIC X        VALUE "*".
           05  FILLER         PIC X(27)    VALUE SPACE.
           05  HH-LINE        PIC X(15)    VALUE "PROJECT HISTORY".      
           05  FILLER         PIC X(28)    VALUE SPACE.
           05  FILLER         PIC X        VALUE "*".

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
           OPEN INPUT HISTORY-FILE.
           MOVE 0 TO PROJECT-COUNT.
           MOVE 0 TO SV-PROJ-COUNT.
           MOVE 0 TO SV-NO-LATE.
           MOVE 0 TO SV-NO-PROFIT.
           MOVE 0 TO SV-TOT-BUDGET.
           MOVE 0 TO SV-TOT-COST.
           MOVE "N" TO PROJECT-FILE-SW.
           MOVE SPACE TO DPN-NUMBER.
           MOVE SPACE TO ACCEPT-PROJECT-NO.
           MOVE "N" TO CANCEL-VIEW-SW.
           MOVE 1 TO DESC-COUNT.
           MOVE ZERO TO DISPLAY-ONE-PROJECT-NO.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE.

       PROGRAM-CLEANUP.
           CLOSE HISTORY-FILE.

       DISPLAY-HEADER.
           DISPLAY UNDERLINE-STAR.
           DISPLAY HEAD-HISTORY.
           DISPLAY UNDERLINE-STAR.
           DISPLAY PROJ-NUMBER-HEAD.
           DISPLAY HEAD-LINE.
           DISPLAY UNDERLINE-STAR.

       DISPLAY-MENU-EXPLAIN.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "End of projects ...".
           DISPLAY " ".
           DISPLAY "Press enter to display statistics...".
           ACCEPT ACCEPT-DUMMY.
           PERFORM SHOW-STATISTIC.
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
               READ HISTORY-FILE NEXT
                   AT END SET PROJECT-FILE-AT-END TO TRUE.
	       
           IF NOT PROJECT-FILE-AT-END
              AND PROJECT-COUNT NOT = 6
               PERFORM DISPLAY-ONE-PROJECT

           IF PROJECT-COUNT = 6 AND
              NOT PROJECT-FILE-AT-END
               PERFORM DISPLAY-MORE-PROJECTS.

       DISPLAY-ONE-PROJECT.
           ADD 1 TO PROJECT-COUNT.

           MOVE HI-NUMBER          TO DPN-NUMBER.
           MOVE HI-NAME            TO DP-NAME.
           MOVE HI-DESCRIPTION (1) TO DP-DESCRIPTION.
           MOVE HI-DATE (1)        TO DP-START-DATE.

           DISPLAY DISPLAY-PROJECT-NUMBER.
           DISPLAY DISPLAY-PROJECT.
           DISPLAY UNDERLINE-HEAD.
           PERFORM CALC-STATISTIC.

       DISPLAY-MORE-PROJECTS.
           MOVE 0 TO PROJECT-COUNT.
           DISPLAY 
           "Enter to continue ...".
           ACCEPT ACCEPT-DUMMY.
           PERFORM DISPLAY-HEADER.

       GET-PROJECT-NO.
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1"
               SET CANCEL-VIEW TO TRUE
           ELSE
               MOVE ACCEPT-PROJECT-NO
                    TO HI-NUMBER
               READ HISTORY-FILE
                   INVALID KEY PERFORM READ-KEY-ERROR
                   NOT INVALID KEY
                       PERFORM SHOW-COMPLETE-PROJECT.

       READ-KEY-ERROR.
           DISPLAY "Project number not found, please try again ...".

       SHOW-COMPLETE-PROJECT.
           DISPLAY "Project number:".
           DISPLAY HI-NUMBER.
           DISPLAY "Project name:".
           DISPLAY HI-NAME.
           DISPLAY "Description:".
           PERFORM SHOW-DESCRIPTION
               UNTIL DESC-COUNT = 10.
           MOVE 1 TO DESC-COUNT.
           DISPLAY "Project leader name:".
           DISPLAY HI-LEADER-NAME.
           DISPLAY "Project leader surname:".
           DISPLAY HI-LEADER-SURNAME.
           DISPLAY " ".
           DISPLAY "Start date:".
           DISPLAY HI-DATE (1).
           DISPLAY "End date:".
           DISPLAY HI-DATE (2).
           DISPLAY "Enter to continue ...".
           ACCEPT ACCEPT-DUMMY.
           DISPLAY "Actual end date:".
           DISPLAY HI-ACTUAL-END-DATE.
           DISPLAY "Budget:".
           MOVE HI-BUDGET TO DOP-BUDGET.
           DISPLAY DOP-BUDGET.
           DISPLAY "Actual budget:".
           MOVE HI-ACTUAL-BUDGET TO DOP-BUDGET.
           DISPLAY DOP-BUDGET.
           DISPLAY "Cost:".
           MOVE HI-COST TO DOP-COST.
           DISPLAY DOP-COST.
           DISPLAY "Actual cost:".
           MOVE HI-ACTUAL-COST TO DOP-COST.
           DISPLAY DOP-COST.
           COMPUTE DOP-PROFIT = HI-BUDGET - HI-COST.
           DISPLAY "Project profit:".
           DISPLAY DOP-PROFIT.
           COMPUTE DOP-PROFIT = HI-ACTUAL-BUDGET - HI-COST.
           DISPLAY "Actual profit:".
           DISPLAY DOP-PROFIT.

           PERFORM DISPLAY-MENU.

       SHOW-DESCRIPTION.
           DISPLAY HI-DESCRIPTION (DESC-COUNT).
           ADD 1 TO DESC-COUNT.

       SHOW-STATISTIC.
           DISPLAY "Total number of projects in history:".
           MOVE SV-PROJ-COUNT TO SVD-PROJ-COUNT.
           DISPLAY SVD-PROJ-COUNT.
           DISPLAY "Number of projects finished late:".
           MOVE SV-NO-LATE TO SVD-NO-LATE.
           DISPLAY SVD-NO-LATE.
           DISPLAY "Number of projects with negative profit:".
           MOVE SV-NO-PROFIT TO SVD-NO-PROFIT.
           DISPLAY SVD-NO-PROFIT.
           DISPLAY 
           "Avarage no of projects with negative profit(procent):".
           COMPUTE SVD-NO-PROFIT-PRCNT = 
               (SV-NO-PROFIT/SV-PROJ-COUNT)*100.
           DISPLAY SVD-NO-PROFIT-PRCNT.
           DISPLAY "Total budget for all projects:".
           MOVE SV-TOT-BUDGET TO SVD-TOT-BUDGET.
           DISPLAY SVD-TOT-BUDGET.
           DISPLAY "Total cost for all projects:".
           MOVE SV-TOT-COST TO SVD-TOT-COST.
           DISPLAY SVD-TOT-COST.
           DISPLAY "Total profit for all projects:".
           COMPUTE SVD-TOT-PROFIT = SV-TOT-BUDGET - SV-TOT-COST.
           DISPLAY SVD-TOT-PROFIT.
           DISPLAY "Avarage profit per project(procent):".
           COMPUTE SVD-PROFIT-PRCNT =
               ((SV-TOT-BUDGET - SV-TOT-COST)/SV-TOT-BUDGET)*100.
           DISPLAY SVD-PROFIT-PRCNT.

       CALC-STATISTIC.
           ADD 1 TO SV-PROJ-COUNT.

           IF HI-ACTUAL-END-DATE > HI-DATE (2)
               ADD 1 TO SV-NO-LATE.

           IF HI-ACTUAL-BUDGET < HI-ACTUAL-COST
               ADD 1 TO SV-NO-PROFIT.

           ADD HI-ACTUAL-BUDGET TO SV-TOT-BUDGET.
           ADD HI-ACTUAL-COST TO SV-TOT-COST.

