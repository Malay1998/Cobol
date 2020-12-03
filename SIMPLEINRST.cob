       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLEINRST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC A(15) VALUE 'PETER JOHN'.
       01 WS-INP-BILL.
           05 WS-AMT PIC 9(10).
           05 WS-PBA PIC 9(5).
           05 WS-PCA PIC 9(5).
           05 WS-PAMT PIC S99999.
           05 WS-BTA PIC S99999V99.
           05 WS-CAA PIC S99999V99.
           05 WS-ICA PIC 99999V99.
           05 WS-NBA PIC 99999V99 VALUE 10000.00.
           05 WS-CLA PIC 99999.
           05 WS-ACA PIC 99999.
           05 WS-SCD PIC 9(10).
           05 WS-DLBC PIC X(2).
           05 WS-BCD PIC X(6) VALUE '200811'.
           05 WS-CCT PIC A(15) VALUE 'PLATINUM'.
       01 WS-CROSS PIC X(16) VALUE 'XXXXXXXXXXXX1200'.
       01 WS-ACCEPT PIC 9(6).
       01 WS-YR PIC 9(2).
       01 WS-MTH PIC 9(2).
       01 WS-DAY PIC 9(2).
       01 WS-BYR PIC 9(2).
       01 WS-BMTH PIC 9(2).
       01 WS-BDAY PIC 9(2).
       01 WS-DBMTH PIC 9(2).
       01 WS-YRD PIC 9(2).
       01 WS-MTHD PIC 9(2).
       01 WS-DAYD PIC 9(3).
       01 WS-PAY PIC 99999V99.
       PROCEDURE DIVISION.
       BUISNESS-VALIDATION-PARA.
           DISPLAY 'NAME:' WS-NAME.
           DISPLAY 'CREDIT CARD NO:' WS-CROSS.
           DISPLAY 'CARD TYPE:' WS-CCT.
           ACCEPT WS-ACCEPT FROM DATE.
           UNSTRING WS-ACCEPT
           INTO WS-YR, WS-MTH, WS-DAY
           END-UNSTRING.
           DISPLAY 'BILL CYCLE DATE:' WS-BCD.
           UNSTRING WS-BCD
           INTO WS-BYR, WS-BMTH, WS-BDAY.
           COMPUTE WS-DBMTH = WS-BMTH + 01.
           DISPLAY 'PAY DATE:' WS-BYR, WS-DBMTH, WS-BDAY.
           DISPLAY 'BILL PAYMENT DATE:' WS-ACCEPT.
           COMPUTE WS-YRD = WS-YR - WS-BYR.
           COMPUTE WS-MTHD = WS-MTH - WS-DBMTH.
           IF WS-YRD = 00 THEN
               IF WS-MTHD = 00 THEN
                   COMPUTE WS-DAYD = WS-DAY - WS-BDAY
                   IF WS-DAYD < 15 THEN
                   DISPLAY 'BILL:' WS-NBA
                   ELSE
                   COMPUTE WS-DAYD = WS-NBA + (WS-NBA*012*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
                   END-IF
               END-IF
               IF WS-MTHD = 01 THEN
                  COMPUTE WS-DAYD = (31- WS-BDAY) + WS-DAY
                  COMPUTE WS-DAYD = WS-NBA + (WS-NBA*012*WS-DAYD)/365
                  DISPLAY 'LATE DAY:' WS-DAYD
                  DISPLAY 'BILL:' WS-PAY
              END-IF
              IF WS-MTHD = 02 THEN
                  COMPUTE WS-DAYD = (31- WS-BDAY) + 31 + WS-DAY
                  COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                  DISPLAY 'LATE DAY:' WS-DAYD
                  DISPLAY 'BILL:' WS-PAY
              END-IF
              IF WS-MTHD = 03 THEN
                  COMPUTE WS-DAYD = (31- WS-BDAY) + 61 + WS-DAY
                  COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                  DISPLAY 'LATE DAY:' WS-DAYD
                  DISPLAY 'BILL:' WS-PAY
              END-IF
           END-IF.
           IF WS-YRD = 01 THEN
               IF WS-MTHD = 08 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 92 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 07 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 123 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 06 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 151 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 05 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 182 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 04 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 212 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 03 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 243 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 02 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 273 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 01 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 304 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 00 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 335 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 01 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 365 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
               END-IF
               IF WS-MTHD = 02 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 396 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
              END-IF
              IF WS-MTHD = 03 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 426 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
              END-IF
           END-IF.
           IF WS-YRD = 02 THEN
              IF WS-MTHD = 08 THEN
                   COMPUTE WS-DAYD = (31- WS-BDAY) + 457 + WS-DAY
                   COMPUTE WS-PAY = WS-NBA + (WS-NBA*0.12*WS-DAYD)/365
                   DISPLAY 'LATE DAY:' WS-DAYD
                   DISPLAY 'BILL:' WS-PAY
                   END-IF
           END-IF.
           STOP-RUN.
