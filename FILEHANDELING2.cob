       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEHANDELING2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLING
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\BILL.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\ACCOUNT.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BANKINT
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\BANKINT.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\TRANSACTION.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUTERROR
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\OUTPUTERR.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT A
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\A.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT B
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\B.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT C
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\C.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT D
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\D.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT E
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\E.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD BILLING.
       01 WS-BILL PIC 9(7).
       FD ACCOUNT.
       01 WS-ACCT PIC 9(11).
       FD BANKINT.
       01 WS-BANK PIC X(8).
       FD TRANSACTION.
       01 WS-TRANS PIC 9(9).
       FD OUTPUTERROR.
       01 WS-OUTE PIC A(10).
       FD A.
       01 WS-A PIC 9(7).
       FD B.
       01 WS-B PIC 9(11).
       FD C.
       01 WS-C PIC X(8).
       FD D.
       01 WS-D PIC 9(9).
       FD E.
       01 WS-E PIC A(10).
       WORKING-STORAGE SECTION.
       01 WS-A1 PIC 9(7).
       01 WS-B1 PIC 9(11).
       01 WS-C1 PIC X(8).
       01 WS-D1 PIC 9(9).
       01 WS-E1 PIC A(10).
       01 WS-M PIC A(1) VALUE 'A'.
       01 WS-N PIC A(1) VALUE 'B'.
       01 WS-O PIC A(1) VALUE 'C'.
       01 WS-P PIC A(1) VALUE 'D'.
       01 WS-Q PIC A(1) VALUE 'E'.
       PROCEDURE DIVISION.
           OPEN INPUT BILLING.
           OPEN OUTPUT A.
           PERFORM UNTIL WS-M = 'X'
           READ BILLING INTO WS-A1
           AT END MOVE 'X' TO WS-M
           NOT AT END PERFORM 1000-WRITE-PARA
           END-READ
           END-PERFORM.
           CLOSE BILLING.
           CLOSE A.
           OPEN INPUT ACCOUNT.
           OPEN OUTPUT B.
           PERFORM UNTIL WS-N = 'Y'
           READ ACCOUNT INTO WS-B1
           AT END MOVE 'Y' TO WS-N
           NOT AT END PERFORM 2000-WRITE-PARA
           END-READ
           END-PERFORM.
           CLOSE ACCOUNT.
           CLOSE B.
           OPEN INPUT BANKINT.
           OPEN OUTPUT C.
           PERFORM UNTIL WS-O = 'K'
           READ BANKINT INTO WS-C1
           AT END MOVE 'K' TO WS-O
           NOT AT END PERFORM 3000-WRITE-PARA
           END-READ
           END-PERFORM.
           CLOSE BANKINT.
           CLOSE C.
           OPEN INPUT TRANSACTION.
           OPEN OUTPUT D.
           PERFORM UNTIL WS-P = 'R'
           READ TRANSACTION INTO WS-D1
           AT END MOVE 'R' TO WS-P
           NOT AT END PERFORM 4000-WRITE-PARA
           END-READ
           END-PERFORM.
           CLOSE TRANSACTION.
           CLOSE D.
           OPEN INPUT OUTPUTERROR.
           OPEN OUTPUT E.
           PERFORM UNTIL WS-Q = 'L'
           READ OUTPUTERROR INTO WS-E1
           AT END MOVE 'L' TO WS-Q
           NOT AT END PERFORM 5000-WRITE-PARA
           END-READ
           END-PERFORM.
           CLOSE OUTPUTERROR.
           CLOSE E.
           STOP RUN.
       1000-WRITE-PARA.
           MOVE WS-A1 TO WS-A.
           WRITE  WS-A

           END-WRITE.
       2000-WRITE-PARA.
           MOVE WS-B1 TO WS-B.
           WRITE WS-B
           END-WRITE.
       3000-WRITE-PARA.
           MOVE WS-C1 TO WS-C.
           WRITE WS-C
           END-WRITE.
       4000-WRITE-PARA.
           MOVE WS-D1 TO WS-D.
           WRITE WS-D
           END-WRITE.
       5000-WRITE-PARA.
           MOVE WS-E1 TO WS-E.
           WRITE WS-E
           END-WRITE.
