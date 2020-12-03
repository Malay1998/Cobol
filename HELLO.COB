       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9 VALUE 1.
       01 WS-B PIC 9 VALUE 2.
       01 WS-C PIC 99 VALUE 99.
       01 WS-D PIC X(7) VALUE 'FRIENDS'.
       01 WS-E PIC X(7).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "    MY NAME IS MALAY.".

           IF WS-A LESS THAN WS-B
               DISPLAY WS-C
               END-IF.
                   MOVE WS-D TO WS-E
                   DISPLAY WS-E

           STOP RUN.
