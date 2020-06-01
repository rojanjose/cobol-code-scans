       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SAMPMTCH.
       DATE-COMPILED.
      *---------------------------------------------------------------*
      *                                                               *
      * THIS PROGRAM COMPARES THE CAP INFO AGAINS THE ELIG INFO AND   *
      *  CREATES A FILE OF MATCHED PROVIDER, PERSON, RATE INFO.       *
      *  THE NEW-RATE FILE IS TAB-DELIMITED FOR DOWNLOAD AND USE      *
      *  WITH A WIN-BASED SYSTEM.                                     *
      *                                                               *
      * THIS CODE IS A STRIPPED DOWN VERSION OF AN ACTUAL PRODUCTION  *
      *  PROGRAM - NEARLY ALL OF THE BUSINESS RULES LOGIC HAS BEEN    *
      *  REMOVED TO MAKE THE EXAMPLE EASIER TO READ.                  *
      *                                                               *
      * IF YOU CHOOSE TO USE THIS MODEL, PLEASE MAKE SURE YOU         *
      *  THOROUGHLY TEST YOUR VERSION BEFORE USING FOR SOME BUSINESS  *
      *  REQUIREMENT.                                                 *
      *                                                               *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT COMP-CAP      ASSIGN TO UT-S-CAP.
           SELECT COMP-ELG      ASSIGN TO UT-S-ELIG.
           SELECT NEW-RATE      ASSIGN TO UT-S-NEWRATE.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  COMP-CAP
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01  COMP-CAP-REC.
           05 CCR-PROV          PIC X(9).
           05 CCR-FILL          PIC X.
           05 CCR-RATE          PIC X(5).
           05 FILLER            PIC X(65).
      *
       FD  COMP-ELG
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  COMP-ELG-REC.
           05 CER-PRSN          PIC X(12).
           05 FILLER            PIC X.
           05 CER-PROV          PIC X(9).
           05 FILLER            PIC X(58).

       FD  NEW-RATE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  NEW-RATE-REC         PIC X(80).

       WORKING-STORAGE SECTION.
       77  WKS-MESSAGE         PIC X(23) VALUE
                                   'WORKING-STORAGE SECTION'.

       77  CAP-READ               PIC 9(7) COMP-3 VALUE 0.
       77  ELG-READ               PIC 9(7) COMP-3 VALUE 0.
       77  NEW-RATE-RECS          PIC 9(7) COMP-3 VALUE 0.
       77  RATE-ZEROED            PIC 9(7) COMP-3 VALUE 0.
       77  TOT-MONEY              PIC 9(7)V99 COMP-3 VALUE 0.
      *
       01  MATCH-FILES.
           05 NEED-CAP              PIC X VALUE 'Y'.
           05 NEED-ELG              PIC X VALUE 'Y'.
           05 EOF-CAP               PIC X VALUE 'N'.
           05 EOF-ELG               PIC X VALUE 'N'.
           05 COMP-CAPP             PIC 9(9) VALUE ZEROS.
           05 COMP-ELIG             PIC 9(9) VALUE ZEROS.
      *
       01  NEW-RATE-REC-WORK.
           05 NRR-PROV          PIC X(9).
           05 FILLER            PIC X VALUE X'05'.
           05 NRR-PRSN          PIC X(12).
           05 FILLER            PIC X VALUE X'05'.
           05 NRR-RATE          PIC X(5).
           05 FILLER REDEFINES NRR-RATE.
              10 FILLER         PIC X.
              10 NRR-DLR        PIC 9.
              10 FILLER         PIC X.
              10 NRR-CENTS      PIC 99.
           05 FILLER            PIC X(52).
      *
       01  WORK-MONEY           PIC 9V99.
       01  WORK-MONEY-R REDEFINES WORK-MONEY.
           05 WM-DLR            PIC 9.
           05 WM-CENTS          PIC 99.
      *
       PROCEDURE DIVISION.
       010-OPEN-FILES.
           OPEN INPUT  COMP-CAP
                       COMP-ELG
                OUTPUT NEW-RATE.
      *
       020-READ-CAP-RECORDS.
           IF EOF-CAP = 'Y' OR
              NEED-CAP = 'N'
              GO TO 030-READ-ELG.
           READ COMP-CAP AT END
                MOVE 'Y' TO EOF-CAP
                MOVE 'N' TO NEED-CAP
                MOVE 999999999 TO COMP-CAPP
                MOVE ALL 'Z' TO COMP-CAP-REC
                GO TO 030-READ-ELG.
           IF CCR-PROV NOT NUMERIC
              DISPLAY 'CAP PROVIDER NOT NUMERIC - SKIPPED = '
                      COMP-CAP-REC
              GO TO 020-READ-CAP-RECORDS.
           MOVE CCR-PROV TO COMP-CAPP.
           ADD 1 TO CAP-READ.
           MOVE 'N' TO NEED-CAP.
      *
       030-READ-ELG.
           IF EOF-ELG = 'Y' OR
              NEED-ELG = 'N'
              GO TO 040-MATCH-FILES.
           READ COMP-ELG AT END
                MOVE 'Y' TO EOF-ELG
                MOVE 'N' TO NEED-ELG
                MOVE 999999999 TO COMP-ELIG
                MOVE ALL 'Z' TO COMP-ELG-REC
                GO TO 040-MATCH-FILES.
           IF CER-PROV NOT NUMERIC
              DISPLAY 'ELG PROVIDER NOT NUMERIc - SKIPPED'
              GO TO 030-READ-ELG.
           MOVE CER-PROV TO COMP-ELIG.
           ADD 1 TO ELG-READ.
           MOVE 'N' TO NEED-ELG.


       040-MATCH-FILES.
           IF EOF-CAP = 'Y' AND
              EOF-ELG = 'Y'
              GO TO 990-PUBLISH-STATS.
      *
           IF COMP-CAPP = COMP-ELIG GO TO 100-CAP-ELIG-MATCH.
      * these compares/comments change dependng on requirements.
           IF COMP-CAPP < COMP-ELIG GO TO 120-CAP-NOT-USED.
      *    IF COMP-CAPP < COMP-ELIG
      *       MOVE 'Y' TO NEED-CAP
      *       GO TO 020-READ-CAP-RECORDS.
           IF COMP-CAPP > COMP-ELIG GO TO 140-GET-RATE.
      *    IF COMP-CAPP > COMP-ELIG
      *       MOVE 'Y' TO NEED-ELG
      *       DISPLAY 'MISSING CLAIM DATA '
      *       GO TO 020-READ-CAP-RECORDS.

      *  WE SHOULD NOT BE ABLE TO GET HERE. . . .
           DISPLAY ' 040-MATCH-FILES FATAL ERROR'.
           DISPLAY ' CAP=' CCR-PROV ' ELIG=' CER-PROV.
           DISPLAY ' RUN TERMINATED.'.
           GOBACK.
      *
       100-CAP-ELIG-MATCH.
      * these may change depending on how duplicates are handled.
      *    MOVE 'Y' TO NEED-CAP, NEED-ELG.
           MOVE 'Y' TO NEED-ELG.
      *
           MOVE CER-PROV TO NRR-PROV.
           MOVE CER-PRSN TO NRR-PRSN.
           MOVE CCR-RATE TO NRR-RATE.
           MOVE NRR-DLR TO WM-DLR.
           MOVE NRR-CENTS TO WM-CENTS.
           COMPUTE TOT-MONEY = TOT-MONEY + WORK-MONEY.
           WRITE NEW-RATE-REC FROM NEW-RATE-REC-WORK.
           COMPUTE NEW-RATE-RECS = NEW-RATE-RECS + 1.
      *
           GO TO 020-READ-CAP-RECORDS.
      *
       120-CAP-NOT-USED.
           MOVE 'Y' TO NEED-CAP.
      *
      *    DISPLAY 'CAP RECORD NOT USED = ' COMP-CAP-REC.
      *
      *    MOVE CER-PROV TO NRR-PROV.
      *    MOVE CER-PRSN TO NRR-PRSN.
      *    MOVE CCR-RATE TO NRR-RATE.
      *    WRITE NEW-RATE-REC FROM NEW-RATE-REC-WORK.
      *    COMPUTE NEW-RATE-RECS = NEW-RATE-RECS + 1.
      *
           GO TO 020-READ-CAP-RECORDS.
      *
       140-GET-RATE.
           MOVE 'Y' TO NEED-ELG.
      *
           MOVE CER-PROV TO NRR-PROV.
           MOVE CER-PRSN TO NRR-PRSN.
           MOVE ' 0.00'  TO NRR-RATE.
           WRITE NEW-RATE-REC FROM NEW-RATE-REC-WORK.
           COMPUTE NEW-RATE-RECS = NEW-RATE-RECS + 1.
           COMPUTE RATE-ZEROED   = RATE-ZEROED   + 1.
      *
           GO TO 020-READ-CAP-RECORDS.
      *
       990-PUBLISH-STATS.
           DISPLAY 'CAP RECS READ = ' CAP-READ.
           DISPLAY 'ELG RECS READ = ' ELG-READ.
           DISPLAY 'NEW RECS      = ' NEW-RATE-RECS.
           DISPLAY 'ZEROED RATES  = ' RATE-ZEROED.
           DISPLAY 'TOTAL MONEY   = ' TOT-MONEY.
      *
       9999-STOP.
           CLOSE COMP-CAP COMP-ELG NEW-RATE.
           GOBACK.
 