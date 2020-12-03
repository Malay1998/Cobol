       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP ASSIGN TO
           'C:\LEARNCOBOL\COBOL\FILEHANDLING\SORT.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTP ASSIGN TO
           'C:\LEARNCOBOL\COBOL\FILEHANDLING\ASORT.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT WORK ASSIGN TO
           'C:\LEARNCOBOL\COBOL\FILEHANDLING\WORK.PAT'.
       DATA DIVISION.
       FILE SECTION.
       FD INP.
       01 INPF.
           02 I-ID PIC 9(4).
           02 PIC X.
           02 I-NAME PIC X(15).
       FD OUTP.
       01 EMPLOYEE.
           02 E-ID PIC 9(4).
           02 PIC X.
           02 E-NAME PIC X(20).
       SD WORK.
       01 WORKP.
           02 W-ID PIC 9(4).
           02 PIC X.
           02 W-NAME PIC X(20).

       WORKING-STORAGE SECTION.
       01 WS-A PIC X VALUE SPACE.
       01 WS-M PIC X VALUE SPACE.
       PROCEDURE DIVISION.
           OPEN INPUT INP.
           PERFORM UNTIL WS-M = 'X'
           READ INP
           AT END MOVE 'X' TO WS-M
           NOT AT END PERFORM A-PARA
           END-PERFORM.
           CLOSE INP.
           DISPLAY '---------------------'
           DISPLAY ' AFTER PERFORM SORT  '
           DISPLAY '---------------------'

      *    SORT OPERATION IS DONE BELOW
           SORT WORK ON ASCENDING KEY E-ID
           USING INP GIVING OUTP.
           OPEN INPUT OUTP.
           PERFORM UNTIL WS-A = 'X'
           READ OUTP
           AT END MOVE 'X' TO WS-A
           NOT AT END PERFORM B-PARA
            END-PERFORM.
           CLOSE OUTP.
           STOP RUN.
       A-PARA.
               DISPLAY INPF.
       B-PARA.
           DISPLAY EMPLOYEE.
