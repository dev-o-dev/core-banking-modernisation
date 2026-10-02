       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RGSCV010.
       AUTHOR.        K L MORRIS.
       INSTALLATION.  MEGABANK PLC - GROUP IT - REGULATORY REPORTING.
       DATE-WRITTEN.  2010-04-19.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : RGSCV010                                            *
      * TITLE    : FSCS SINGLE CUSTOMER VIEW (SCV) EXTRACT             *
      * TYPE     : BATCH COBOL (NO DB2)                                *
      * JCL      : RGSCV01                                             *
      *----------------------------------------------------------------*
      * *** STUB - PROJECT SCV (PID MBK-2010-004) IN FLIGHT ***        *
      * FSA COBS / FSCS RULES REQUIRE AN SCV FILE PRODUCIBLE WITHIN 72 *
      * HOURS FROM 31/12/2010. THIS VERSION WRITES THE HEADER, ONE     *
      * CUSTOMER RECORD PER PERSONAL CIF AND THE TRAILER. STILL TO DO: *
      *   - AGGREGATE ELIGIBLE BALANCES PER CUSTOMER (ACCTCIF PATH)    *
      *   - APPLY THE 50,000 GBP COMPENSATION LIMIT                    *
      *   - JOINT ACCOUNTS (CIF RELATIONSHIP FILE - NOT YET BUILT)     *
      *   - EXCLUDE INELIGIBLE DEPOSITORS (LARGE CORPORATES)           *
      * DO NOT SUBMIT THE OUTPUT OF THIS VERSION TO THE FSCS.          *
      *----------------------------------------------------------------*
      * INPUT    : CIFMAST  - CIF MASTER (SEQUENTIAL)                  *
      *            DATECARD - BUSINESS DATE CONTROL CARD               *
      * OUTPUT   : SCVOUT   - SCV FILE (FB 300)                        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-04-19 KLM  ORIGINAL VERSION (STUB - CUSTOMER RECORDS ONLY)*
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-ZSERIES.
       OBJECT-COMPUTER. IBM-ZSERIES.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CIFMAST-FILE     ASSIGN TO CIFMAST
                                   ORGANIZATION IS INDEXED
                                   ACCESS MODE IS SEQUENTIAL
                                   RECORD KEY IS CIF-KEY
                                   FILE STATUS IS WS-CIFMAST-STATUS.
           SELECT SCVOUT-FILE      ASSIGN TO SCVOUT
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-SCVOUT-STATUS.
           SELECT DATECARD-FILE    ASSIGN TO DATECARD
                                   ORGANIZATION IS SEQUENTIAL
                                   FILE STATUS IS WS-DATECARD-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CIFMAST-FILE.
           COPY CFCUSREC.
      *
       FD  SCVOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SCVOUT-RECORD                   PIC X(300).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F.
       01  DATECARD-RECORD                 PIC X(80).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'RGSCV010'.
      *
       01  WS-FILE-STATUSES.
           05  WS-CIFMAST-STATUS           PIC X(02).
               88  WS-CIFMAST-OK               VALUE '00'.
               88  WS-CIFMAST-EOF              VALUE '10'.
           05  WS-SCVOUT-STATUS            PIC X(02).
           05  WS-DATECARD-STATUS          PIC X(02).
      *
       01  WS-COUNTERS.
           05  WS-READ-COUNT               PIC 9(09) VALUE 0.
           05  WS-SCV-COUNT                PIC 9(09) VALUE 0.
      *
       01  WS-SCV-RECORD.
           05  SCV-REC-TYPE                PIC X(01).
           05  SCV-DATA                    PIC X(299).
           05  SCV-HEADER REDEFINES SCV-DATA.
               10  SCVH-FIRM-REF           PIC X(06).
               10  SCVH-EXTRACT-DATE       PIC 9(08).
               10  SCVH-VERSION            PIC X(04).
               10  FILLER                  PIC X(281).
           05  SCV-CUSTOMER REDEFINES SCV-DATA.
               10  SCVC-CIF-NO             PIC X(12).
               10  SCVC-TITLE              PIC X(06).
               10  SCVC-FIRST-NM           PIC X(40).
               10  SCVC-LAST-NM            PIC X(40).
               10  SCVC-DOB                PIC 9(08).
               10  SCVC-NINO               PIC X(13).
               10  SCVC-ADDR-LINE1         PIC X(60).
               10  SCVC-TOWN               PIC X(40).
               10  SCVC-POST-CD            PIC X(10).
               10  SCVC-AGG-BALANCE        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  SCVC-COMP-AMOUNT        PIC S9(13)V99
                                           SIGN LEADING SEPARATE.
               10  FILLER                  PIC X(38).
           05  SCV-TRAILER REDEFINES SCV-DATA.
               10  SCVT-CUSTOMER-COUNT     PIC 9(09).
               10  FILLER                  PIC X(290).
      *
           COPY DATECARD.
      *
           COPY ABNDINFO.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE WS-PROGRAM-ID              TO ABN-PROGRAM-ID

           OPEN INPUT  CIFMAST-FILE
                       DATECARD-FILE
                OUTPUT SCVOUT-FILE
           IF NOT WS-CIFMAST-OK
           OR WS-SCVOUT-STATUS NOT = '00'
           OR WS-DATECARD-STATUS NOT = '00'
               MOVE '0000-MAIN'            TO ABN-PARAGRAPH
               MOVE 1001                   TO ABN-ABEND-CODE
               MOVE 'FILE OPEN FAILED'     TO ABN-MESSAGE
               PERFORM Z999-ABEND
           END-IF

           READ DATECARD-FILE INTO DTC-DATE-CARD
           CLOSE DATECARD-FILE

           DISPLAY 'RGSCV010 *** STUB VERSION - BALANCES NOT '
                   'AGGREGATED - DO NOT SUBMIT TO FSCS ***'

           MOVE SPACES                     TO WS-SCV-RECORD
           MOVE 'H'                        TO SCV-REC-TYPE
           MOVE '123456'                   TO SCVH-FIRM-REF
           MOVE DTC-BUSINESS-DATE          TO SCVH-EXTRACT-DATE
           MOVE '0.1S'                     TO SCVH-VERSION
           WRITE SCVOUT-RECORD             FROM WS-SCV-RECORD

           PERFORM 1000-READ-CIF
           PERFORM 2000-WRITE-CUSTOMER
               UNTIL WS-CIFMAST-EOF

           MOVE SPACES                     TO WS-SCV-RECORD
           MOVE 'T'                        TO SCV-REC-TYPE
           MOVE WS-SCV-COUNT               TO SCVT-CUSTOMER-COUNT
           WRITE SCVOUT-RECORD             FROM WS-SCV-RECORD

           CLOSE CIFMAST-FILE
                 SCVOUT-FILE

           DISPLAY 'RGSCV010 - CIF RECORDS READ    ' WS-READ-COUNT
           DISPLAY 'RGSCV010 - SCV CUSTOMERS       ' WS-SCV-COUNT

      *    ALWAYS RC 4 UNTIL THE PROJECT DELIVERS - FLAGS THE JOB AS
      *    "COMPLETED WITH WARNINGS" ON THE SCHEDULE.
           MOVE 4                          TO RETURN-CODE
           GOBACK
           .
      *================================================================*
       1000-READ-CIF.
      *================================================================*
           READ CIFMAST-FILE NEXT RECORD
           IF WS-CIFMAST-OK
               ADD 1                       TO WS-READ-COUNT
           ELSE
               IF NOT WS-CIFMAST-EOF
                   MOVE '1000-READ-CIF'    TO ABN-PARAGRAPH
                   MOVE 1002               TO ABN-ABEND-CODE
                   MOVE 'CIFMAST READ FAILED'
                                           TO ABN-MESSAGE
                   MOVE WS-CIFMAST-STATUS  TO ABN-FILE-STATUS
                   PERFORM Z999-ABEND
               END-IF
           END-IF
           .
      *================================================================*
       2000-WRITE-CUSTOMER.
      *================================================================*
           IF CIF-PERSONAL AND NOT CIF-CLOSED
               MOVE SPACES                 TO WS-SCV-RECORD
               MOVE 'C'                    TO SCV-REC-TYPE
               MOVE CIF-NO                 TO SCVC-CIF-NO
               MOVE CIF-TITLE              TO SCVC-TITLE
               MOVE CIF-FIRST-NM           TO SCVC-FIRST-NM
               MOVE CIF-LAST-NM            TO SCVC-LAST-NM
               MOVE CIF-DOB                TO SCVC-DOB
               MOVE CIF-NAT-INS-NO         TO SCVC-NINO
               MOVE CIF-ADDR-LINE1         TO SCVC-ADDR-LINE1
               MOVE CIF-TOWN               TO SCVC-TOWN
               MOVE CIF-POST-CD            TO SCVC-POST-CD
      *        TODO SCV PHASE 2 - AGGREGATE ELIGIBLE BALANCES
               MOVE ZERO                   TO SCVC-AGG-BALANCE
                                              SCVC-COMP-AMOUNT
               WRITE SCVOUT-RECORD         FROM WS-SCV-RECORD
               ADD 1                       TO WS-SCV-COUNT
           END-IF

           PERFORM 1000-READ-CIF
           .
      *
           COPY ABNDPROC.
