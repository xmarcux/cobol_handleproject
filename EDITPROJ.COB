       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDITPROJ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FCPROJECT.COB".
           COPY "FCHISTORY.COB".

       DATA DIVISION.
       FILE SECTION.
           COPY "FDPROJECT.COB".
           COPY "FDHISTORY.COB".

       WORKING-STORAGE SECTION.
       01  ACCEPT-PROJECT.
           05  ACCEPT-PROJECT-NO    PIC X(80)      VALUE SPACE.
           05  ACCEPT-MENU-INPUT    PIC XX         VALUE SPACE.
           05  ACCEPT-PROJECT-DATE  PIC 9(8)       VALUE ZERO.
      
       77  SHOW-MONEY               PIC SZ(10).99  VALUE ZERO.
       77  DESC-ROWS                PIC 99         VALUE ZERO.

       01  ERROR-TEXT.
           05 ET-NUMBER-ENTRY       PIC X(80)      VALUE SPACE.
           05 ET-EDIT-ENTRY         PIC X(80)      VALUE SPACE.

       01  SWITCHES.
           05  CANCEL-INPUT-SW      PIC X          VALUE "N".
               88  CANCEL-INPUT                    VALUE "Y".
           05  CANCEL-EDIT-SW       PIC X          VALUE "N".
               88  CANCEL-EDIT                     VALUE "Y".
           05  CANCEL-EDIT-DATE-SW  PIC X          VALUE "N".
               88 CANCEL-EDIT-DATE                 VALUE "Y".
           05  VALID-DATA-SW        PIC X          VALUE "N".
               88  VALID-DATA                      VALUE "Y".
           05  EDIT-START-DATE-SW   PIC X          VALUE "N".
               88  E-START-DATE                    VALUE "Y".

       PROCEDURE DIVISION.

       PERFORM INIT-PROGRAM.
       PERFORM SHOW-MENU.
       PERFORM PROGRAM-CLEANUP.

       EXIT PROGRAM.

       INIT-PROGRAM.
           OPEN I-O PROJECT-FILE.
           MOVE SPACE TO ACCEPT-PROJECT-NO.
           MOVE "N" TO CANCEL-INPUT-SW.
           MOVE "N" TO CANCEL-EDIT-SW.
           MOVE SPACE TO ACCEPT-MENU-INPUT.
           MOVE SPACE TO ERROR-TEXT.
           MOVE ZERO TO DESC-ROWS.
           MOVE ZERO TO ACCEPT-PROJECT-DATE.

       PROGRAM-CLEANUP.
           CLOSE PROJECT-FILE.

       SHOW-MENU.
           PERFORM GET-PROJECT-NO
               UNTIL CANCEL-INPUT.

       GET-PROJECT-NO.
           DISPLAY "*********************".
           DISPLAY "*   Edit project    *".
           DISPLAY "*********************".
           DISPLAY ET-NUMBER-ENTRY.
           DISPLAY "Enter project number:".
           DISPLAY "(Enter -1 to return to main menu)".

           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1"
               SET CANCEL-INPUT TO TRUE
           ELSE
               MOVE ACCEPT-PROJECT-NO TO PR-NUMBER
               READ PROJECT-FILE
                   INVALID KEY PERFORM READ-KEY-ERROR
                   NOT INVALID KEY PERFORM SHOW-EDIT.

       READ-KEY-ERROR.
           MOVE "Project number not found, please try again ..."
               TO ET-NUMBER-ENTRY.

       SHOW-EDIT.
           MOVE SPACE TO ET-NUMBER-ENTRY.
           PERFORM SHOW-EDIT-MENU
               UNTIL CANCEL-EDIT.
           MOVE "N" TO CANCEL-EDIT-SW.

       SHOW-EDIT-MENU.
           DISPLAY " ".
           DISPLAY "****************************".
           DISPLAY "*       Edit project       *".
           DISPLAY "****************************".
           DISPLAY "* -1 - Cancel edit         *".
           DISPLAY "****************************".
           DISPLAY "*  1 - Edit number         *".
           DISPLAY "*  2 - Edit name           *".
           DISPLAY "*  3 - Edit description    *".
           DISPLAY "*  4 - Edit leader name    *".
           DISPLAY "*  5 - Edit leader surname *".
           DISPLAY "*  6 - Edit start date     *".
           DISPLAY "*  7 - Edit end date       *".
           DISPLAY "*  8 - Edit budget         *".
           DISPLAY "*  9 - Edit cost           *".
           DISPLAY "****************************".
           DISPLAY "* 10 - Close project       *".
           DISPLAY "* 11 - Delete project      *".
           DISPLAY "****************************".
           DISPLAY ET-EDIT-ENTRY.
           DISPLAY "Choose number:".
           PERFORM PERFORM-EDIT.

       PERFORM-EDIT.
           ACCEPT ACCEPT-MENU-INPUT.

           EVALUATE ACCEPT-MENU-INPUT
               WHEN "-1"
                   SET CANCEL-EDIT TO TRUE
                   MOVE SPACE TO ET-EDIT-ENTRY
               WHEN "1"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-NUMBER
               WHEN "2"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-NAME
               WHEN "3"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-DESCRIPTION
               WHEN "4"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-LEADER-NAME
               WHEN "5"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-LEADER-SURNAME
               WHEN "6"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-START-DATE
               WHEN "7"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-END-DATE
               WHEN "8"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-BUDGET
               WHEN "9"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-COST
               WHEN "10"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM CLOSE-PROJECT
               WHEN "11"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM DELETE-PROJECT
               WHEN OTHER 
                   MOVE "Wrong entry ... try again" TO ET-EDIT-ENTRY.

       EDIT-NUMBER.
           DISPLAY "Current project number is:".
           DISPLAY PR-NUMBER.
           DISPLAY "Change project number and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               PERFORM UPDATE-FILE-NO
               MOVE "Project number updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update project number canceled" TO ET-EDIT-ENTRY.

       EDIT-NAME.
           DISPLAY "Current project name is:".
           DISPLAY PR-NAME.
           DISPLAY 
               "Change project name and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               MOVE ACCEPT-PROJECT-NO TO PR-NAME
               PERFORM UPDATE-FILE
               MOVE "Project name updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update project name canceled" 
                   TO ET-EDIT-ENTRY.

       EDIT-DESCRIPTION.
           DISPLAY "Current description is:".
           DISPLAY PR-DESCRIPTION (1).
           DISPLAY PR-DESCRIPTION (2).
           DISPLAY PR-DESCRIPTION (3).
           DISPLAY PR-DESCRIPTION (4).
           DISPLAY PR-DESCRIPTION (5).
           DISPLAY PR-DESCRIPTION (6).
           DISPLAY PR-DESCRIPTION (7).
           DISPLAY PR-DESCRIPTION (8).
           DISPLAY PR-DESCRIPTION (9).
           DISPLAY PR-DESCRIPTION (10).
           DISPLAY "Change discription,".
           DISPLAY "continue to add lines until".
           DISPLAY "a line with minus one(-1) or maximum".
           DISPLAY "ten(10) rows.".
           DISPLAY "Minus one(-1) at first line cancel input:".
           PERFORM GET-DESCRIPTION-ROWS
                UNTIL CANCEL-EDIT OR
                      DESC-ROWS >= 10.
      
           IF DESC-ROWS < 10 AND
              DESC-ROWS > 1 AND CANCEL-EDIT
               PERFORM CLEAR-DESC-END
                   UNTIL DESC-ROWS > 10.

           PERFORM UPDATE-FILE.

           IF DESC-ROWS <=1 AND CANCEL-EDIT
               MOVE "Description canceled" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Description updated" TO ET-EDIT-ENTRY.

           MOVE "N" TO CANCEL-EDIT-SW.
           MOVE ZERO TO DESC-ROWS.

       GET-DESCRIPTION-ROWS.
            ADD 1 TO DESC-ROWS.
            ACCEPT ACCEPT-PROJECT-NO.
            IF ACCEPT-PROJECT-NO NOT = "-1"
                MOVE ACCEPT-PROJECT-NO TO
                     PR-DESCRIPTION(DESC-ROWS)
            ELSE
                SET CANCEL-EDIT TO TRUE.

       CLEAR-DESC-END.
           MOVE SPACE TO PR-DESCRIPTION (DESC-ROWS).
           ADD 1 TO DESC-ROWS.

       EDIT-LEADER-NAME.
           DISPLAY "Current project leader name is:".
           DISPLAY PR-LEADER-NAME.
           DISPLAY 
               "Change project leader name and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               MOVE ACCEPT-PROJECT-NO TO PR-LEADER-NAME
               PERFORM UPDATE-FILE
               MOVE "Project leader name updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update project leader name canceled" 
                   TO ET-EDIT-ENTRY.

       EDIT-LEADER-SURNAME.
           DISPLAY "Current project leader surname is:".
           DISPLAY PR-LEADER-SURNAME.
           DISPLAY 
               "Change project leader surname and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               MOVE ACCEPT-PROJECT-NO TO PR-LEADER-SURNAME
               PERFORM UPDATE-FILE
               MOVE "Project leader surname updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update project leader surname canceled" 
                   TO ET-EDIT-ENTRY.

       EDIT-START-DATE.
           DISPLAY "Current start date is:".
           DISPLAY PR-DATE (1).
           DISPLAY "Change start date and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           MOVE "Y" TO EDIT-START-DATE-SW.
           PERFORM ACCEPT-DATE
               WITH TEST AFTER
               UNTIL CANCEL-EDIT-DATE OR VALID-DATA.

           IF NOT CANCEL-EDIT-DATE AND VALID-DATA
               MOVE ACCEPT-PROJECT-DATE TO PR-DATE (1)
               PERFORM UPDATE-FILE
               MOVE "Project start date updated" TO ET-EDIT-ENTRY.
           
           IF CANCEL-EDIT-DATE
               MOVE "Update start date canceled" TO ET-EDIT-ENTRY.


       EDIT-END-DATE.
           DISPLAY "Current end date is:".
           DISPLAY PR-DATE (2).
           DISPLAY "Change end date and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           MOVE "N" TO EDIT-START-DATE-SW.
           PERFORM ACCEPT-DATE
               WITH TEST AFTER
               UNTIL CANCEL-EDIT-DATE OR VALID-DATA.

           IF NOT CANCEL-EDIT-DATE AND VALID-DATA
               MOVE ACCEPT-PROJECT-DATE TO PR-DATE (2)
               PERFORM UPDATE-FILE
               MOVE "Project end date updated" TO ET-EDIT-ENTRY.
           
           IF CANCEL-EDIT-DATE
               MOVE "Update end date canceled" TO ET-EDIT-ENTRY.


       ACCEPT-DATE.
           ACCEPT ACCEPT-PROJECT-NO.
           PERFORM CHECK-CANCEL-EDIT-DATE.
           MOVE ACCEPT-PROJECT-NO TO ACCEPT-PROJECT-DATE.
           CALL "DATEOK" USING VALID-DATA-SW
                               ACCEPT-PROJECT-DATE.

           IF NOT VALID-DATA AND NOT CANCEL-EDIT-DATE
               DISPLAY "Wrong date format."
               DISPLAY "Format should be: YYYYMMDD"
               DISPLAY " ... try again.".

           IF E-START-DATE AND VALID-DATA AND
              NOT CANCEL-EDIT-DATE AND
              ACCEPT-PROJECT-DATE > PR-DATE (2)
               MOVE "N" TO VALID-DATA-SW
               DISPLAY "Start date must be less"
               DISPLAY "or equal to end date."
               DISPLAY "End date:"
               DISPLAY PR-DATE (2)
               DISPLAY " ... try again.".

           IF NOT E-START-DATE AND VALID-DATA AND
              NOT CANCEL-EDIT-DATE AND
              ACCEPT-PROJECT-DATE < PR-DATE (1)
               MOVE "N" TO VALID-DATA-SW
               DISPLAY "End date must be equal"
               DISPLAY "or larger then start date."
               DISPLAY "Start date:"
               DISPLAY PR-DATE (1)
               DISPLAY " ... try again.".

       EDIT-BUDGET.
           DISPLAY "Current budget is:".
           MOVE PR-BUDGET TO SHOW-MONEY.
           DISPLAY SHOW-MONEY.
           DISPLAY 
               "Change budget and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               MOVE ACCEPT-PROJECT-NO TO PR-BUDGET
               PERFORM UPDATE-FILE
               MOVE "Project budget updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update budget canceled" 
                   TO ET-EDIT-ENTRY.


       EDIT-COST.
           DISPLAY "Current cost is:".
           MOVE PR-COST TO SHOW-MONEY.
           DISPLAY SHOW-MONEY.
           DISPLAY 
               "Change cost and press enter to save:".
           DISPLAY "(Enter -1 to cancel input)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO NOT = "-1"
               MOVE ACCEPT-PROJECT-NO TO PR-COST
               PERFORM UPDATE-FILE
               MOVE "Project cost updated" TO ET-EDIT-ENTRY
           ELSE
               MOVE "Update cost canceled" 
                   TO ET-EDIT-ENTRY.

       DELETE-PROJECT.
           DISPLAY "Do you want to delete project,".
           DISPLAY "with number:".
           DISPLAY PR-NUMBER.
           DISPLAY "with name:".
           DISPLAY PR-NAME.
           DISPLAY "Answer (Y/N):".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "Y"
               DELETE PROJECT-FILE
                   INVALID KEY 
                       MOVE "Error deleting project!" TO
                           ET-EDIT-ENTRY
                   NOT INVALID KEY
                       PERFORM PROJECT-DELETED
           ELSE
               MOVE "Delete project canceled" TO ET-EDIT-ENTRY.

       PROJECT-DELETED.
           DISPLAY "Project deleted!".
           SET CANCEL-EDIT TO TRUE.

       EDIT-DATE.

       UPDATE-FILE.
           REWRITE PROJECT-RECORD.

       UPDATE-FILE-NO.
           DELETE PROJECT-FILE
               INVALID KEY DISPLAY "Can not change project number".
           MOVE ACCEPT-PROJECT-NO TO PR-NUMBER.
           WRITE PROJECT-RECORD.

       CHECK-CANCEL-EDIT-DATE.
           IF ACCEPT-PROJECT-NO = "-1"
               SET CANCEL-EDIT-DATE TO TRUE
           ELSE
               MOVE "N" TO CANCEL-EDIT-DATE-SW.

       CLOSE-PROJECT.
           DISPLAY "Do you want to close project,".
           DISPLAY "and move project to history?".
           DISPLAY "Answer (Y/N):".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "Y"
               PERFORM INIT-CLOSE-PROJECT
           ELSE
               MOVE "Close project canceled" TO ET-EDIT-ENTRY.

       INIT-CLOSE-PROJECT.
           OPEN I-O HISTORY-FILE.
           MOVE PR-NUMBER TO HI-NUMBER.
           MOVE PR-NAME TO HI-NAME.
           MOVE PR-DESCRIPTION (1) TO HI-DESCRIPTION (1).
           MOVE PR-DESCRIPTION (2) TO HI-DESCRIPTION (2).
           MOVE PR-DESCRIPTION (3) TO HI-DESCRIPTION (3).
           MOVE PR-DESCRIPTION (4) TO HI-DESCRIPTION (4).
           MOVE PR-DESCRIPTION (5) TO HI-DESCRIPTION (5).
           MOVE PR-DESCRIPTION (6) TO HI-DESCRIPTION (6).
           MOVE PR-DESCRIPTION (7) TO HI-DESCRIPTION (7).
           MOVE PR-DESCRIPTION (8) TO HI-DESCRIPTION (8).
           MOVE PR-DESCRIPTION (9) TO HI-DESCRIPTION (9).
           MOVE PR-DESCRIPTION (10) TO HI-DESCRIPTION (10).
           MOVE PR-LEADER-NAME TO HI-LEADER-NAME.
           MOVE PR-LEADER-SURNAME TO HI-LEADER-SURNAME.
           MOVE PR-DATE (1) TO HI-DATE (1).
           MOVE PR-DATE (2) TO HI-DATE (2).
           MOVE PR-BUDGET TO HI-BUDGET.
           MOVE PR-COST TO HI-COST.
           PERFORM EDIT-CLOSE-PROJECT.

       EDIT-CLOSE-PROJECT.
           DISPLAY "Did budget change from".
           DISPLAY "original budget?".
           DISPLAY "Enter final budget:".
           DISPLAY "(Enter minus one(-1) if".
           DISPLAY "budget has not changed)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1" OR
              ACCEPT-PROJECT-NO = SPACE OR
              ACCEPT-PROJECT-NO = LOW-VALUE
               MOVE PR-BUDGET TO HI-ACTUAL-BUDGET
           ELSE
               MOVE ACCEPT-PROJECT-NO TO HI-ACTUAL-BUDGET.

           DISPLAY "".
           DISPLAY "Did cost change from".
           DISPLAY "original cost?".
           DISPLAY "Enter final cost:".
           DISPLAY "(Enter minus one(-1) if".
           DISPLAY "cost has not changed)".
           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1" OR
              ACCEPT-PROJECT-NO = SPACE OR
              ACCEPT-PROJECT-NO = LOW-VALUE
               MOVE PR-COST TO HI-ACTUAL-COST
           ELSE
               MOVE ACCEPT-PROJECT-NO TO HI-ACTUAL-COST.

           DISPLAY "".
           DISPLAY "Did end date change from".
           DISPLAY "original end date?".
           DISPLAY "Enter final end date:".
           DISPLAY "(Enter minus one(-1) if".
           DISPLAY "end date has not changed)".
           MOVE "N" TO VALID-DATA-SW.
           PERFORM ACCEPT-FINAL-END-DATE
               UNTIL VALID-DATA

           PERFORM WRITE-TO-HISTORY.
           SET CANCEL-EDIT TO TRUE.
           MOVE "Project moved to history" TO ET-NUMBER-ENTRY.

       WRITE-TO-HISTORY.
           WRITE HISTORY-RECORD
               INVALID KEY DISPLAY "Error writing project to history".
           CLOSE HISTORY-FILE.
           DELETE PROJECT-FILE
               INVALID KEY DISPLAY 
               "Error deleting project at move to history".

       ACCEPT-FINAL-END-DATE.
           MOVE SPACE TO ACCEPT-PROJECT-NO.
           MOVE ZERO TO ACCEPT-PROJECT-DATE.

           ACCEPT ACCEPT-PROJECT-NO.
           IF ACCEPT-PROJECT-NO = "-1"
               MOVE PR-DATE(2) TO HI-ACTUAL-END-DATE
               MOVE PR-DATE(2) TO ACCEPT-PROJECT-DATE
               SET VALID-DATA TO TRUE
           ELSE
               MOVE ACCEPT-PROJECT-NO TO ACCEPT-PROJECT-DATE
               MOVE "N" TO VALID-DATA-SW.
               CALL "DATEOK" USING VALID-DATA-SW
                                   ACCEPT-PROJECT-DATE.

           IF NOT VALID-DATA
               DISPLAY "Wrong date format."
               DISPLAY "Format should be: YYYYMMDD"
               DISPLAY " ... try again.".

           IF VALID-DATA AND
               ACCEPT-PROJECT-DATE < PR-DATE (1)
               MOVE "N" TO VALID-DATA-SW
               DISPLAY "End date must be equal"
               DISPLAY "or larger then start date."
               DISPLAY "Start date:"
               DISPLAY PR-DATE (1)
               DISPLAY " ... try again."
           ELSE
               MOVE ACCEPT-PROJECT-DATE TO HI-ACTUAL-END-DATE.
