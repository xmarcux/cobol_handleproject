       FD  PROJECT-FILE
               LABEL RECORDS ARE STANDARD.
       01  PROJECT-RECORD.
           05  PR-NUMBER            PIC X(80).
           05  PR-NAME              PIC X(80).
           05  PR-DESCRIPTION       PIC X(80)  OCCURS 10 TIMES.
           05  PR-LEADER-NAME       PIC X(80).
           05  PR-LEADER-SURNAME    PIC X(80).
           05  PR-DATE                         OCCURS 2 TIMES.
               10  PRD-YEAR         PIC 9(4).
               10  PRD-MONTH        PIC 9(2).
               10  PRD-DAY          PIC 9(2).
           05  PR-BUDGET            PIC S9(10)V99.
           05  PR-COST              PIC S9(10)V99.
           05  PR-ACTUAL-BUDGET     PIC S9(10)V99.
           05  PR-ACTUAL-COST       PIC S9(10)V99. 
