       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 LS-SUM PIC 9(6)V9(2).
       01 LS-TERM PIC 9(2).
       01 LS-AMT PIC 9(5)V9(2).
       PROCEDURE DIVISION USING LS-SUM, LS-TERM, LS-AMT.
           COMPUTE LS-SUM = (LS-TERM * LS-AMT)
           EXIT PROGRAM.
