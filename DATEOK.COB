       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATEOK.

      ****************************************
      * Module takes a date as argument 
      * and verifies that the date is valid.
      * Year is between 1900 and 3000.
      * Month is between 1 and 12.
      * Day is between 1 and current
      * months maximum.
      * Leap year is considered.
      * Date format YYYYMMDD.
      ****************************************

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       77 LEAP-YEAR            PIC 9     VALUE ZERO.

       LINKAGE SECTION.
       01  DATE-OK-SWITCH      PIC X.
           88  DATE-OK                VALUE "Y".

       01  CURRENT-DATE.
           05  CD-YEAR             PIC 9(4).
           05  CD-MONTH            PIC 9(2).
           05  CD-DAY              PIC 9(2).

       PROCEDURE DIVISION USING DATE-OK-SWITCH
                                CURRENT-DATE.

       CHECK-DATE.
           IF CD-YEAR >= 1900 AND <= 3000
               MOVE FUNCTION REM (CD-YEAR 4) TO LEAP-YEAR
               IF CD-MONTH = 4 AND CD-DAY >= 1 AND <= 30
                   SET DATE-OK TO TRUE
               ELSE IF (CD-MONTH = 6 OR
                        9 OR 11) AND
                       CD-DAY >=1 AND <= 30
                   SET DATE-OK TO TRUE
               ELSE IF CD-MONTH = 2 AND
                       CD-DAY >= 1 AND <= 29
                       AND LEAP-YEAR = 0
                   SET DATE-OK TO TRUE
               ELSE IF CD-MONTH = 2 AND
                       CD-DAY >= 1 AND <=28
                   SET DATE-OK TO TRUE
               ELSE IF (CD-MONTH = 1 OR
                        3 OR 5 OR 7 OR 8 OR 10 OR 12)
                        AND CD-DAY >=1 AND <= 31
                   SET DATE-OK TO TRUE
               ELSE
                   MOVE "N" TO DATE-OK-SWITCH
           ELSE
               MOVE "N" TO DATE-OK-SWITCH.

       EXIT PROGRAM.
