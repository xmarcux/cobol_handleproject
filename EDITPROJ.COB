       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDITPROJ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FCPROJECT.COB".

       DATA DIVISION.
       FILE SECTION.
           COPY "FDPROJECT.COB".

       WORKING-STORAGE SECTION.
       01  ACCEPT-PROJECT.
           05  ACCEPT-PROJECT-NO    PIC X(80)      VALUE SPACE.
           05  ACCEPT-MENU-INPUT    PIC XX         VALUE SPACE.
           05  ACCEPT-PROJECT-DATE  PIC 9(8)       VALUE ZERO.
      
       77  SHOW-MONEY               PIC SZ(10).99  VALUE ZERO.

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

       SHOW-EDIT-MENU.
           DISPLAY " ".
           DISPLAY "****************************".
           DISPLAY "*       Edit project       *".
           DISPLAY "****************************".
           DISPLAY "* -1 - Cancel edit         *".
           DISPLAY "****************************".
           DISPLAY "*  1 - Edit number         *".
           DISPLAY "*  2 - Edit description    *".
           DISPLAY "*  3 - Edit leader name    *".
           DISPLAY "*  4 - Edit leader surname *".
           DISPLAY "*  5 - Edit start date     *".
           DISPLAY "*  6 - Edit end date       *".
           DISPLAY "*  7 - Edit budget         *".
           DISPLAY "*  8 - Edit cost           *".
           DISPLAY "****************************".
           DISPLAY "*  9 - Close project       *".
           DISPLAY "* 10 - Delete project      *".
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
                   DISPLAY "2"
               WHEN "3"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-LEADER-NAME
               WHEN "4"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-LEADER-SURNAME
               WHEN "5"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-START-DATE
               WHEN "6"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-END-DATE
               WHEN "7"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-BUDGET
               WHEN "8"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   PERFORM EDIT-COST
               WHEN "9"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   DISPLAY "9"
               WHEN "10"
                   MOVE SPACE TO ET-EDIT-ENTRY
                   DISPLAY "10"
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
           DISPLAY "Change discription.......".

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


       CLOSE-PROJECT.

       DELETE-PROJECT.

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