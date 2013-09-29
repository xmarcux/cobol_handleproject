       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJADD.

      ****************************
      * Program accept inputs from
      * user on detals for a new
      * project, validates data 
      * and saves new project 
      * data to projectfile.
      * Control is returned to 
      * calling program.
      ****************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FCPROJECT.COB".

       DATA DIVISION.
       FILE SECTION.
           COPY "FDPROJECT.COB".

       WORKING-STORAGE SECTION.
       77  ACCEPT-DATA             PIC X(80)      VALUE SPACE.
       77  ACCEPT-NUM-DATA         PIC S9(10)V99  VALUE ZERO.
       77  DESC-ROWS               PIC 99         VALUE ZERO.
       01  VALID-DATA-SW           PIC X          VALUE "N".
           88  VALID-DATA                         VALUE "Y".
       01  GET-START-DATE-SW       PIC X          VALUE "Y".
           88  GET-START-DATE                     VALUE "Y".

       LINKAGE SECTION.
       01  CANCEL-INPUT-SW          PIC X.
           88  CANCEL-INPUT                       VALUE "Y".

       PROCEDURE DIVISION USING CANCEL-INPUT-SW.

       PERFORM INIT-INPUT.
       PERFORM DISPLAY-MENU.
       PERFORM GET-PROJECT-INFO.
       IF NOT CANCEL-INPUT
           PERFORM SAVE-TO-FILE. 
       PERFORM EXIT-PROGRAM.

       EXIT PROGRAM.

       INIT-INPUT.
           MOVE SPACE TO ACCEPT-DATA.
           MOVE ZERO TO ACCEPT-NUM-DATA.
           MOVE ZERO TO DESC-ROWS.
           MOVE "N" TO VALID-DATA-SW.
           MOVE "N" TO CANCEL-INPUT-SW.
           MOVE 0 TO PR-ACTUAL-COST.
           MOVE 0 TO PR-ACTUAL-BUDGET.
           OPEN I-O PROJECT-FILE.

       EXIT-PROGRAM.
           CLOSE PROJECT-FILE.

       DISPLAY-MENU.
           DISPLAY "".
           DISPLAY "******************************".
           DISPLAY "*      Add new project       *".
           DISPLAY "*  Enter minus one(-1) and   *".
           DISPLAY "*press enter at any input to *".
           DISPLAY "*    cancel and return to    *".
           DISPLAY "*         main menu.         *".
           DISPLAY "******************************".

       GET-PROJECT-INFO.
           PERFORM GET-PROJECT-NUMBER.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-NAME
                   UNTIL VALID-DATA.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-DESCRIPTION.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-FIRST-NAME.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-SURNAME.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-START-DATE.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-END-DATE.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-BUDGET.
           IF NOT CANCEL-INPUT
               PERFORM GET-PROJECT-COST.

       GET-PROJECT-NUMBER.
           DISPLAY "Enter project number:".
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT
               MOVE ACCEPT-DATA TO PR-NUMBER
               READ PROJECT-FILE
                   INVALID KEY CONTINUE
                   NOT INVALID KEY 
                       PERFORM PROJECT-NUMBER-EXIST.

       PROJECT-NUMBER-EXIST.
           DISPLAY "Project number aready exists!".
           DISPLAY "Use another project number and".
           DISPLAY "try again ...".
           PERFORM GET-PROJECT-NUMBER.

       GET-PROJECT-NAME.
           DISPLAY "Enter project name:".
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-VALID-DATA.
           IF NOT VALID-DATA
               DISPLAY "Project must have a name...please try again."
           ELSE
               PERFORM CHECK-CANCEL-INPUT.
           IF VALID-DATA AND NOT CANCEL-INPUT
               MOVE ACCEPT-DATA TO PR-NAME.

       GET-PROJECT-DESCRIPTION.
           DISPLAY "Enter project description,".
           DISPLAY "continue to add lines until".
           DISPLAY "a line with minus one(-1) or maximum".
           DISPLAY "ten(10) rows:".
           PERFORM GET-DESCRIPTION-ROWS
               UNTIL CANCEL-INPUT OR
                     DESC-ROWS >= 10.
           MOVE "N" TO CANCEL-INPUT-SW.

       GET-DESCRIPTION-ROWS.
           ADD 1 TO DESC-ROWS.
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT
               MOVE ACCEPT-DATA TO 
                    PR-DESCRIPTION (DESC-ROWS).

       GET-PROJECT-FIRST-NAME.
           DISPLAY "Enter project leaders first name:".
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT
               MOVE ACCEPT-DATA TO PR-LEADER-NAME.

       GET-PROJECT-SURNAME.
           DISPLAY "Enter project leaders surname:".
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT
               MOVE ACCEPT-DATA TO PR-LEADER-SURNAME.

       GET-PROJECT-START-DATE.
           MOVE "N" TO VALID-DATA-SW.
           DISPLAY "Enter start date in format YYYYMMDD:".
           SET GET-START-DATE TO TRUE.
           PERFORM ACCEPT-DATE
               WITH TEST AFTER
               UNTIL CANCEL-INPUT OR VALID-DATA.

       GET-PROJECT-END-DATE.
           MOVE "N" TO VALID-DATA-SW.
           DISPLAY "Enter end date in format YYYYMMDD:".
           MOVE "N" TO GET-START-DATE-SW.
           PERFORM ACCEPT-DATE
               WITH TEST AFTER
               UNTIL CANCEL-INPUT OR VALID-DATA.

       ACCEPT-DATE.
           ACCEPT ACCEPT-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF GET-START-DATE
               MOVE ACCEPT-DATA TO PR-DATE (1)
               CALL "DATEOK" USING VALID-DATA-SW
                                   PR-DATE (1)
           ELSE
               MOVE ACCEPT-DATA TO PR-DATE (2)
               CALL "DATEOK" USING VALID-DATA-SW
                                   PR-DATE (2). 

           IF NOT VALID-DATA AND NOT CANCEL-INPUT
               DISPLAY "Wrong date format."
               DISPLAY "Format should be: YYYYMMDD"
               DISPLAY " ... try again.".

           IF PR-DATE (1) > PR-DATE (2) AND
              VALID-DATA AND NOT CANCEL-INPUT AND
              NOT GET-START-DATE
               MOVE "N" TO VALID-DATA-SW
               DISPLAY "End date must be equal"
               DISPLAY "or larger then start date."
               DISPLAY " ... try again.".

       GET-PROJECT-BUDGET.
           DISPLAY "Enter project budget:".
           ACCEPT ACCEPT-NUM-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT
               MOVE ACCEPT-NUM-DATA TO PR-BUDGET.

       GET-PROJECT-COST.
           DISPLAY "Enter project cost:".
           ACCEPT ACCEPT-NUM-DATA.
           PERFORM CHECK-CANCEL-INPUT.
           IF NOT CANCEL-INPUT 
               MOVE ACCEPT-NUM-DATA TO PR-COST.

       SAVE-TO-FILE.
           DISPLAY "Saving ... ".
           WRITE PROJECT-RECORD.

       CHECK-VALID-DATA.
           IF ACCEPT-DATA = SPACE OR = LOW-VALUE
               MOVE "N" TO VALID-DATA-SW
           ELSE 
               SET VALID-DATA TO TRUE.

       CHECK-CANCEL-INPUT.
           IF ACCEPT-DATA = "-1" OR
              ACCEPT-NUM-DATA = -1
               SET CANCEL-INPUT TO TRUE.
