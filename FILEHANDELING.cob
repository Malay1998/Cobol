       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEHANDLING.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICY
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\POLICY.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT KSDS
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\KSDS.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERR
           ASSIGN TO 'C:\LEARNCOBOL\COBOL\FILEHANDLING\ERROR.PAT'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD POLICY.
       01 INP.
           02 I-NUM PIC X(5).
           02 PIC X.
           02 I-TERM PIC 9(2).
           02 PIC X.
           02 I-LOB PIC X(7).
           02 PIC X.
           02 I-AMT PIC 9(5).9(2).
           02 PIC X.
           02 I-STATUS PIC X(7).
       FD KSDS.
       01 OUTP.
           02 O-NUM PIC X(5).
           02 PIC X.
           02 O-TERM PIC 9(2).
           02 PIC X.
           02 O-LOB PIC X(7).
           02 PIC X.
           02 O-AMT PIC 9(5).9(2).
           02 PIC X.
           02 O-STATUS PIC X(7).
           02 PIC X.
           02 O-SUM PIC 9(6).9(2).
           02 PIC X.
           02 O-LEVEL PIC X(6).
           02 PIC X.
           02 O-RISK PIC X.
           02 PIC X(12).
       FD ERR.
       01 ENP.
           02 E-NUM PIC X(5).
           02 PIC X.
           02 E-TERM PIC 9(2).
           02 PIC X.
           02 E-LOB PIC X(7).
           02 PIC X.
           02 E-AMT PIC 9(5).9(2).
           02 PIC X.
           02 E-STATUS PIC X(7).
       WORKING-STORAGE SECTION.
       01 WS-W PIC A(1) VALUE SPACE.
       01 WS-SUM PIC 9(6)V9(2).
       01 WS-TERM PIC 9(2).
       01 WS-AMT PIC 9(5)V9(2).
       01 WS-PGM PIC X(6).
       01 HEADER.
           02 PIC X(5) VALUE 'P_NUM'.
           02 PIC X.
           02 PIC X(6) VALUE 'P_TERM'.
           02 PIC X.
           02 PIC X(7) VALUE 'P_LOB'.
           02 PIC X.
           02 PIC X(8) VALUE 'P_AMT'.
           02 PIC X.
           02 PIC X(8) VALUE 'P_STATUS'.
           02 PIC X.
           02 PIC X(9) VALUE 'P_SUM'.
           02 PIC X.
           02 PIC X(6) VALUE 'P_LVL'.
           02 PIC X.
           02 PIC X(6) VALUE 'P_RISK'.
       PROCEDURE DIVISION.
           OPEN INPUT POLICY.
           OPEN OUTPUT ERR.
           OPEN OUTPUT KSDS.
           MOVE HEADER TO OUTP
           WRITE OUTP.
           INITIALIZE OUTP.
           PERFORM UNTIL WS-W = 'X'
           READ POLICY
           AT END MOVE 'X' TO WS-W
           NOT AT END PERFORM A-PARA
           END-PERFORM
           CLOSE POLICY.
           CLOSE KSDS.
           CLOSE ERR.
           STOP RUN.
       A-PARA.
           IF
               I-LOB NOT  = SPACES AND
               (I-STATUS  = 'ACTIVE' OR 'EXPIRE') AND
               I-TERM IS NUMERIC AND
               I-AMT(1:5) IS NUMERIC AND
               I-AMT(7:2) IS NUMERIC
               THEN
                  PERFORM B-PARA
               ELSE
                  MOVE INP TO ENP
                  WRITE ENP
           END-IF.
       B-PARA.
           MOVE INP TO OUTP
           EVALUATE TRUE
           WHEN O-STATUS = 'ACTIVE ' AND O-TERM < 1
           MOVE 'LOW' TO O-LEVEL
           WHEN O-STATUS = 'ACTIVE' AND O-TERM = 1
           MOVE 'MEDIUM' TO O-LEVEL
           WHEN O-STATUS = 'ACTIVE' AND O-TERM > 1
           MOVE 'HIGH' TO O-LEVEL
           END-EVALUATE.
           EVALUATE TRUE
           WHEN   O-LOB = 'LIFE'
           MOVE 'H' TO O-RISK
           WHEN O-LOB = 'HEALTH'
           MOVE 'H' TO O-RISK
           WHEN O-LOB = 'CHILD'
           MOVE 'M' TO O-RISK
           WHEN OTHER
           MOVE 'I' TO O-RISK
           END-EVALUATE.
           MOVE O-TERM TO WS-TERM
           MOVE O-AMT TO WS-AMT
           MOVE 'SUBPGM' TO WS-PGM
           CALL WS-PGM USING WS-SUM,WS-TERM,WS-AMT
           MOVE WS-SUM TO O-SUM
           WRITE OUTP.
