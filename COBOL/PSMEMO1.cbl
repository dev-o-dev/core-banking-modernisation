       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PSMEMO1.
       AUTHOR.        G H THORNTON.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  1994-01-17.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PSMEMO1                                             *
      * TITLE    : REAL-TIME MEMO POSTING - CHANNEL SERVICE MEMOPOST   *
      * TYPE     : CICS - LINKED FROM CHROUTER, PYINIT1, PYFPRSP AND   *
      *            THE BRANCH TELLER SYSTEM                            *
      * FILES    : ACCTMST  READ UPDATE / REWRITE                      *
      *            PSTXREF  WRITE (DUPLICATE CHECK)                    *
      *            MEMOLOG  WRITE (ESDS)                               *
      *            ALL THREE ARE RECOVERABLE (CSD RECOVERY(BACKOUTONLY)*
      * COMMAREA : CHCOMMAR / CH-MEMOPOST-AREA                         *
      *----------------------------------------------------------------*
      * A MEMO POSTING UPDATES THE AVAILABLE BALANCE AND THE MEMO      *
      * TOTALS ON THE ACCOUNT MASTER IMMEDIATELY, AND LOGS THE POSTING *
      * TO THE MEMO LOG. THE LEDGER BALANCE IS NOT CHANGED UNTIL THE   *
      * POSTING IS HARD POSTED BY PSEOD010 IN THE END OF DAY BATCH.    *
      *                                                                *
      * PROCESSING ORDER (DO NOT CHANGE - SEE PIR 2002/031):           *
      *   1. VALIDATE REQUEST                                          *
      *   2. READ ACCOUNT FOR UPDATE (ENQUEUES THE RECORD)             *
      *   3. APPLY STATUS / RESTRICTION / FUNDS RULES                  *
      *   4. WRITE EXTERNAL REFERENCE - DUPREC = RESUBMISSION          *
      *   5. REWRITE ACCOUNT                                           *
      *   6. WRITE MEMO LOG                                            *
      * THE UNIT OF WORK IS COMMITTED BY THE CALLER (SYNCONRETURN).    *
      *                                                                *
      * KNOWN PROBLEM PIR 2006/044 (NOT FIXED): A RESUBMISSION THAT    *
      * WOULD NOW FAIL THE STATUS OR FUNDS RULES IS REJECTED WITH THAT *
      * REASON (E.G. NSF) RATHER THAN DUPL, BECAUSE THE RULES RUN      *
      * BEFORE THE DUPLICATE CHECK.                                    *
      *----------------------------------------------------------------*
      * REASON CODES RETURNED                                          *
      *   ANFD ACCOUNT NOT FOUND       ACLS ACCOUNT CLOSED             *
      *   AFRZ ACCOUNT FROZEN          ADRM DORMANT - DEBIT REFUSED    *
      *   RSTD DEBITS RESTRICTED       RSTC CREDITS RESTRICTED         *
      *   NSF  INSUFFICIENT FUNDS      XCSS EXCESS ALLOWED (WARNING)   *
      *   DUPL DUPLICATE REFERENCE     INVx INVALID REQUEST FIELD      *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 1994-01-17 GHT  ORIGINAL VERSION (BRANCH TELLER ONLY)          *
      * 1998-11-30 MLW  Y2K                                            *
      * 2001-11-05 DKP  CHANNEL INTERFACE (CHCOMMAR) FOR IBANK         *
      * 2002-02-11 DKP  DUPLICATE CHECK VIA PSTXREF (PIR 2002/031 -    *
      *                 IBANK DOUBLE POSTING)                          *
      * 2004-08-09 SAT  OVERDRAFT EXPIRY DATE HONOURED                 *
      * 2006-06-30 AOS  OVERRIDE FOR BRANCH CHANNEL ONLY (IA-06-14)    *
      * 2008-05-27 AOS  FPS TRANSACTION CODES                          *
      * 2010-06-28 JNB  AMOUNT / DIRECTION / TXN CODE KEPT ON PSTXREF  *
      *                 SO PSREV01 CAN REVERSE A SAME-DAY POSTING      *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PSMEMO1 '.
      *
       01  WS-CONSTANTS.
           05  WS-MAX-POSTING-AMT          PIC S9(13)V99 COMP-3
                                           VALUE +9999999.99.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP2                    PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-TIME-HHMMSS              PIC 9(06).
           05  WS-TIME-HHMMSS-X REDEFINES WS-TIME-HHMMSS
                                           PIC X(06).
           05  WS-HUNDREDTHS               PIC 9(02).
           05  WS-POST-TIME.
               10  WS-POST-TIME-HMS        PIC 9(06).
               10  WS-POST-TIME-HH         PIC 9(02).
           05  WS-POST-TIME-N REDEFINES WS-POST-TIME
                                           PIC 9(08).
           05  WS-AMOUNT                   PIC S9(13)V99 COMP-3.
           05  WS-OD-LIMIT                 PIC S9(13)V99 COMP-3.
           05  WS-SPENDING-POWER           PIC S9(13)V99 COMP-3.
           05  WS-NEW-SEQ-NO               PIC 9(09) COMP.
           05  WS-MEMOLOG-RBA              PIC S9(08) COMP.
           05  WS-ACCOUNT-HELD-IND         PIC X(01) VALUE 'N'.
               88  WS-ACCOUNT-HELD             VALUE 'Y'.
           05  WS-EXCESS-IND               PIC X(01) VALUE 'N'.
               88  WS-EXCESS-ALLOWED           VALUE 'Y'.
      *
       01  WS-ACCT-KEY.
           05  WS-KEY-SORT-CODE            PIC X(06).
           05  WS-KEY-ACCT-NO              PIC X(08).
      *
           COPY DDACCREC.
      *
           COPY PSTXNREC.
      *
           COPY PSXREFRC.
      *
           COPY CHCOMMAR.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT
           MOVE ZERO                       TO CHMP-NEW-LEDGER-BAL
                                              CHMP-NEW-AVAIL-BAL
                                              CHMP-POST-SEQ-NO

           PERFORM 1000-VALIDATE-REQUEST

           IF CH-RC-OK
               PERFORM 2000-GET-TIMESTAMP
               PERFORM 3000-READ-ACCOUNT-UPDATE
           END-IF

           IF CH-RC-OK
               PERFORM 4000-APPLY-POSTING-RULES
           END-IF

           IF CH-RC-OK OR CH-RC-WARNING
               PERFORM 5000-WRITE-EXT-REFERENCE
           END-IF

           IF CH-RC-OK OR CH-RC-WARNING
               PERFORM 6000-UPDATE-ACCOUNT
               PERFORM 7000-WRITE-MEMO-LOG
           ELSE
               IF WS-ACCOUNT-HELD
                   EXEC CICS UNLOCK
                             FILE('ACCTMST')
                             RESP(WS-RESP)
                   END-EXEC
               END-IF
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           MOVE CHMP-TXN-CODE              TO PST-TXN-CODE

           EVALUATE TRUE
               WHEN CHMP-SORT-CODE NOT NUMERIC
               WHEN CHMP-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN NOT PST-TXN-VALID
                   MOVE 'INVT'             TO CH-REASON-CD
                   MOVE 'TRANSACTION CODE INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHMP-DR-CR-IND NOT = 'D' AND 'C'
                   MOVE 'INVD'             TO CH-REASON-CD
                   MOVE 'DEBIT/CREDIT INDICATOR INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHMP-AMOUNT NOT NUMERIC
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'AMOUNT NOT NUMERIC'
                                           TO CH-MESSAGE-TEXT
               WHEN CHMP-AMOUNT NOT > ZERO
               WHEN CHMP-AMOUNT > WS-MAX-POSTING-AMT
                   MOVE 'INVM'             TO CH-REASON-CD
                   MOVE 'AMOUNT OUTSIDE PERMITTED RANGE'
                                           TO CH-MESSAGE-TEXT
               WHEN CHMP-EXT-REF = SPACES OR LOW-VALUES
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'EXTERNAL REFERENCE NOT SUPPLIED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           ELSE
               MOVE CHMP-AMOUNT            TO WS-AMOUNT
      *        OVERRIDE IS ONLY HONOURED FOR THE BRANCH CHANNEL.
               IF CHMP-OVERRIDE-IND = 'Y'
               AND CH-CHANNEL-CD NOT = 'BRANCH'
                   MOVE 'N'                TO CHMP-OVERRIDE-IND
               END-IF
           END-IF
           .
      *================================================================*
       2000-GET-TIMESTAMP.
      *================================================================*
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
                     TIME(WS-TIME-HHMMSS-X)
           END-EXEC

      *    ABSTIME IS IN MILLISECONDS - TAKE HUNDREDTHS FOR THE KEY.
           COMPUTE WS-HUNDREDTHS =
                   FUNCTION MOD (WS-ABSTIME, 1000) / 10
           MOVE WS-TIME-HHMMSS             TO WS-POST-TIME-HMS
           MOVE WS-HUNDREDTHS              TO WS-POST-TIME-HH

           IF CHMP-VALUE-DATE NOT NUMERIC
           OR CHMP-VALUE-DATE = ZERO
               MOVE WS-TODAY               TO CHMP-VALUE-DATE
           END-IF
           .
      *================================================================*
       3000-READ-ACCOUNT-UPDATE.
      *================================================================*
           MOVE CHMP-SORT-CODE             TO WS-KEY-SORT-CODE
           MOVE CHMP-ACCT-NO               TO WS-KEY-ACCT-NO

           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   SET WS-ACCOUNT-HELD     TO TRUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE 'ACCTMST'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       4000-APPLY-POSTING-RULES.
      *================================================================*
      *---- ACCOUNT STATUS --------------------------------------------*
           EVALUATE TRUE
               WHEN ACM-OPEN
               WHEN ACM-PENDING-CLOSE
                   CONTINUE
               WHEN ACM-CLOSED
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS CLOSED'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-FROZEN
                   MOVE 'AFRZ'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS FROZEN'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-DORMANT AND CHMP-DR-CR-IND = 'D'
                   MOVE 'ADRM'             TO CH-REASON-CD
                   MOVE 'ACCOUNT DORMANT - DEBITS REFUSED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

      *---- POSTING RESTRICTIONS --------------------------------------*
           IF CH-REASON-CD = SPACES
               IF CHMP-DR-CR-IND = 'D'
               AND (ACM-NO-DEBITS OR ACM-NO-POSTINGS)
                   MOVE 'RSTD'             TO CH-REASON-CD
                   MOVE 'DEBITS RESTRICTED ON THIS ACCOUNT'
                                           TO CH-MESSAGE-TEXT
               END-IF
               IF CHMP-DR-CR-IND = 'C'
               AND (ACM-NO-CREDITS OR ACM-NO-POSTINGS)
                   MOVE 'RSTC'             TO CH-REASON-CD
                   MOVE 'CREDITS RESTRICTED ON THIS ACCOUNT'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF

      *---- FUNDS CHECK (DEBITS ONLY) ---------------------------------*
           IF CH-REASON-CD = SPACES
           AND CHMP-DR-CR-IND = 'D'
               MOVE ACM-OD-LIMIT           TO WS-OD-LIMIT
               IF ACM-OD-EXPIRY-DATE NOT = ZERO
               AND ACM-OD-EXPIRY-DATE < WS-TODAY
                   MOVE ZERO               TO WS-OD-LIMIT
               END-IF
               COMPUTE WS-SPENDING-POWER =
                       ACM-AVAIL-BAL + WS-OD-LIMIT

               IF WS-AMOUNT > WS-SPENDING-POWER
                   IF CHMP-OVERRIDE-IND = 'Y'
                       SET WS-EXCESS-ALLOWED TO TRUE
                   ELSE
                       MOVE 'NSF '         TO CH-REASON-CD
                       MOVE 'INSUFFICIENT FUNDS'
                                           TO CH-MESSAGE-TEXT
                   END-IF
               END-IF
           END-IF

           IF CH-REASON-CD NOT = SPACES
               MOVE '0008'                 TO CH-RETURN-CD
           ELSE
               IF WS-EXCESS-ALLOWED
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'XCSS'             TO CH-REASON-CD
                   MOVE 'POSTED - ACCOUNT NOW IN UNARRANGED EXCESS'
                                           TO CH-MESSAGE-TEXT
               END-IF
           END-IF
           .
      *================================================================*
       5000-WRITE-EXT-REFERENCE.
      *================================================================*
           COMPUTE WS-NEW-SEQ-NO = ACM-TXN-SEQ-NO + 1

           MOVE SPACES                     TO PXR-XREF-RECORD
           MOVE CH-CHANNEL-CD              TO PXR-CHANNEL-CD
           MOVE CHMP-EXT-REF               TO PXR-EXT-REF
           MOVE ACM-SORT-CODE              TO PXR-SORT-CODE
           MOVE ACM-ACCT-NO                TO PXR-ACCT-NO
           MOVE WS-TODAY                   TO PXR-POST-DATE
           MOVE WS-POST-TIME-N             TO PXR-POST-TIME
           MOVE WS-NEW-SEQ-NO              TO PXR-SEQ-NO
           MOVE WS-AMOUNT                  TO PXR-AMOUNT
           MOVE CHMP-DR-CR-IND             TO PXR-DR-CR-IND
           MOVE CHMP-TXN-CODE              TO PXR-TXN-CODE
           MOVE 'N'                        TO PXR-REVERSED-IND

           EXEC CICS WRITE
                     FILE('PSTXREF')
                     FROM(PXR-XREF-RECORD)
                     LENGTH(LENGTH OF PXR-XREF-RECORD)
                     RIDFLD(PXR-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(DUPREC)
                   PERFORM 5100-RETURN-DUPLICATE
               WHEN OTHER
                   MOVE 'PSTXREF'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
           END-EVALUATE
           .
      *================================================================*
       5100-RETURN-DUPLICATE.
      *================================================================*
      *    THE CHANNEL HAS RESENT A POSTING WE HAVE ALREADY TAKEN.
      *    RETURN THE ORIGINAL SEQUENCE NUMBER AND THE CURRENT
      *    BALANCES; DO NOT POST AGAIN.
           EXEC CICS READ
                     FILE('PSTXREF')
                     INTO(PXR-XREF-RECORD)
                     LENGTH(LENGTH OF PXR-XREF-RECORD)
                     RIDFLD(PXR-KEY)
                     RESP(WS-RESP)
           END-EXEC

           MOVE '0008'                     TO CH-RETURN-CD
           MOVE 'DUPL'                     TO CH-REASON-CD
           MOVE 'DUPLICATE REFERENCE - ORIGINAL POSTING RETAINED'
                                           TO CH-MESSAGE-TEXT
           MOVE PXR-SEQ-NO                 TO CHMP-POST-SEQ-NO
           MOVE ACM-LEDGER-BAL             TO CHMP-NEW-LEDGER-BAL
           MOVE ACM-AVAIL-BAL              TO CHMP-NEW-AVAIL-BAL
           .
      *================================================================*
       6000-UPDATE-ACCOUNT.
      *================================================================*
           IF CHMP-DR-CR-IND = 'D'
               ADD WS-AMOUNT               TO ACM-MEMO-DR-TOT
               SUBTRACT WS-AMOUNT          FROM ACM-AVAIL-BAL
           ELSE
               ADD WS-AMOUNT               TO ACM-MEMO-CR-TOT
               ADD WS-AMOUNT               TO ACM-AVAIL-BAL
           END-IF

      *    A CREDIT TO A DORMANT ACCOUNT REACTIVATES IT (DDA OPS
      *    MANUAL 6.2). DORMANCY INDICATOR IS CLEARED BY DDDRM010.
           IF ACM-DORMANT AND CHMP-DR-CR-IND = 'C'
               SET ACM-OPEN                TO TRUE
           END-IF

           MOVE WS-NEW-SEQ-NO              TO ACM-TXN-SEQ-NO
           MOVE WS-TODAY                   TO ACM-LAST-MAINT-DATE
           MOVE WS-TIME-HHMMSS             TO ACM-LAST-MAINT-TIME
           MOVE CH-USER-ID                 TO ACM-LAST-MAINT-USER

           EXEC CICS REWRITE
                     FILE('ACCTMST')
                     FROM(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RESP(WS-RESP)
           END-EXEC

      *    THE EXTERNAL REFERENCE IS ALREADY WRITTEN - ABEND SO THAT IT
      *    IS BACKED OUT WITH THE REST OF THE UOW.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('PSM2')
               END-EXEC
           END-IF

           MOVE 'N'                        TO WS-ACCOUNT-HELD-IND
           MOVE ACM-LEDGER-BAL             TO CHMP-NEW-LEDGER-BAL
           MOVE ACM-AVAIL-BAL              TO CHMP-NEW-AVAIL-BAL
           MOVE WS-NEW-SEQ-NO              TO CHMP-POST-SEQ-NO
           .
      *================================================================*
       7000-WRITE-MEMO-LOG.
      *================================================================*
           MOVE SPACES                     TO PST-POSTING-RECORD
           MOVE ACM-SORT-CODE              TO PST-SORT-CODE
           MOVE ACM-ACCT-NO                TO PST-ACCT-NO
           MOVE WS-TODAY                   TO PST-POST-DATE
           MOVE WS-POST-TIME-N             TO PST-POST-TIME
           COMPUTE PST-SEQ-NO = FUNCTION MOD (WS-NEW-SEQ-NO, 1000000)
           MOVE CHMP-TXN-CODE              TO PST-TXN-CODE
           MOVE CHMP-DR-CR-IND             TO PST-DR-CR-IND
           MOVE WS-AMOUNT                  TO PST-AMOUNT
           MOVE ACM-CCY-CD                 TO PST-CCY-CD
           MOVE CHMP-VALUE-DATE            TO PST-VALUE-DATE
           MOVE CHMP-NARRATIVE-1           TO PST-NARRATIVE-1
           MOVE CHMP-NARRATIVE-2           TO PST-NARRATIVE-2
           MOVE CH-CHANNEL-CD              TO PST-CHANNEL-CD
           MOVE CHMP-EXT-REF               TO PST-EXT-REF
           SET PST-MEMO-POST               TO TRUE
           SET PST-PENDING                 TO TRUE
           MOVE CH-USER-ID                 TO PST-USER-ID

           EXEC CICS WRITE
                     FILE('MEMOLOG')
                     FROM(PST-POSTING-RECORD)
                     LENGTH(LENGTH OF PST-POSTING-RECORD)
                     RIDFLD(WS-MEMOLOG-RBA)
                     RBA
                     RESP(WS-RESP)
           END-EXEC

      *    IF THE MEMO LOG CANNOT BE WRITTEN THE POSTING WOULD BE LOST
      *    AT END OF DAY - ABEND SO THAT THE WHOLE UOW IS BACKED OUT.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('PSM1')
               END-EXEC
           END-IF
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
      *    ON ENTRY WS-FILE-NAME HOLDS THE CICS FILE NAME.
           MOVE WS-RESP                    TO WS-RESP-DISPLAY
           MOVE '0012'                     TO CH-RETURN-CD
           IF WS-RESP = DFHRESP(NOTOPEN)
           OR WS-RESP = DFHRESP(DISABLED)
               MOVE 'FNOP'                 TO CH-REASON-CD
           ELSE
               MOVE 'FERR'                 TO CH-REASON-CD
           END-IF
           MOVE SPACES                     TO CH-MESSAGE-TEXT
           STRING WS-FILE-NAME
                  ' FILE ERROR - EIBRESP '
                  WS-RESP-DISPLAY
                  DELIMITED BY SIZE
                  INTO CH-MESSAGE-TEXT
           END-STRING
           .
