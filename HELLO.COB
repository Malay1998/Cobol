       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC X VALUE 'I'.
       01 WS-B PIC X VALUE 'A'.
       01 WS-C PIC X VALUE 'R'.
       01 WS-D PIC X(5) VALUE 'ABCDE'.
       01 WS-E PIC X(3).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "    MY NAME IS MALAY.".

           IF WS-A less THAN WS-B
               display WS-C
               END-IF.
                   MOVE WS-D TO WS-E
                   DISPLAY WS-E

           STOP RUN.
