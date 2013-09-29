       FD  HISTORY-FILE
               LABEL RECORDS ARE STANDARD.
       01  HISTORY-RECORD.
           05  HI-NUMBER            PIC X(80).
           05  HI-NAME              PIC X(80).
           05  HI-DESCRIPTION       PIC X(80)  OCCURS 10 TIMES.
           05  HI-LEADER-NAME       PIC X(80).
           05  HI-LEADER-SURNAME    PIC X(80).
           05  HI-DATE                         OCCURS 2 TIMES.
               10  HID-YEAR         PIC 9(4).
               10  HID-MONTH        PIC 9(2).
               10  HID-DAY          PIC 9(2).
           05  HI-BUDGET            PIC S9(10)V99.
           05  HI-COST              PIC S9(10)V99.
           05  HI-ACTUAL-BUDGET     PIC S9(10)V99.
           05  HI-ACTUAL-COST       PIC S9(10)V99. 
           05  HI-ACTUAL-END-DATE.
               10  HID-END-YEAR     PIC 9(4).
               10  HID-END-MONTH    PIC 9(2).
               10  HID-END-DAY      PIC 9(2).
