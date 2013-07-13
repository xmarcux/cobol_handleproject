       FD  PROJECT-FILE
               LABEL RECORDS ARE STANDARD.
       01  PROJECT-RECORD.
      *    Index is made up from date and time
      *    in format YYYYMMDDHHMMSSHH.
           05  PR-INDEX               PIC 9(16).
           05  PR-NUMBER              PIC X(30).
           05  PR-NAME                PIC X(50).
           05  PR-DESCRIPTION         PIC X(800).
           05  PR-LEADER-FIRST-NAME   PIC X(30).
           05  PR-LEADER-SURNAME      PIC X(50).
           05  PR-START-DATE          PIC 9(8).
           05  PR-END-DATE            PIC 9(8).
           05  PR-COST                PIC 9(10).
           05  PR-BUDGET              PIC 9(10).
