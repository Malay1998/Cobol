       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADDITION
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\ADD.PAT'

           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-M.

       DATA DIVISION.

       FILE SECTION.
       FD ADDITION.
       01 INP.
           02 WS-I PIC 9(3).

       WORKING-STORAGE SECTION.

       01 WS-M PIC A(2).
       01 WS-J PIC 9(4).
       01 WS-ACCEPT PIC 9(6).
       01 WS-CNT PIC 9(2).

      
       PROCEDURE DIVISION.
           ACCEPT WS-ACCEPT FROM DATE.
           DISPLAY 'DATE:' WS-ACCEPT
           INITIALIZE WS-CNT WS-I .
           OPEN INPUT ADDITION.


           PERFORM UNTIL WS-M = 'X'
           READ ADDITION
           AT END MOVE 'X' TO WS-M
           NOT AT END PERFORM C-PARA
           END-PERFORM
           CLOSE ADDITION.

            DISPLAY '----------------END----------------'
           STOP RUN.

       C-PARA.

               ADD 1 TO WS-CNT
               ADD WS-I TO WS-J GIVING WS-J
               IF WS-CNT = (4 OR 7 OR 10 OR 13 OR 16 OR 19 OR 22 OR
                    25 or 28 )
                   THEN
                   DISPLAY 'DATE:' WS-ACCEPT
                   END-IF

               DISPLAY '      ' WS-I
               IF WS-CNT = ( 3 or 6 or 9 or 12 or 15 or 18 or 21 or 24
                   or 27 or 29  )

                   THEN

               DISPLAY  'SUM: 'WS-J
               INITIALIZE WS-J
               END-IF
               IF  WS-CNT = ( 3 or 6 or 9 or 12 or 15 or 18 or 21 or
                   24 or 27) THEN

               DISPLAY '-------------END OF PAGE-----------'


               END-IF.
