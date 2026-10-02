       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PSREV01.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2010-06-28.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : PSREV01                                             *
      * TITLE    : SAME-DAY POSTING REVERSAL - CHANNEL SERVICE POSTREV *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : PSTXREF  READ / READ UPDATE / REWRITE               *
      * LINKS    : PSMEMO1  (THE REVERSING MEMO POSTING)               *
      * COMMAREA : CHCOMMAR + CHCOMMA2 / CH-POSTREV-AREA               *
      *----------------------------------------------------------------*
      * REVERSES A MEMO POSTING MADE EARLIER THE SAME BUSINESS DAY,    *
      * IDENTIFIED BY THE ORIGINAL CHANNEL AND EXTERNAL REFERENCE.     *
      * THE REVERSAL IS A NEW MEMO POSTING ('REV ') FOR THE SAME       *
      * AMOUNT IN THE OPPOSITE DIRECTION, WITH ITS OWN EXTERNAL        *
      * REFERENCE (SO A RESUBMITTED REVERSAL IS CAUGHT AS A DUPLICATE  *
      * BY PSMEMO1). THE ORIGINAL PSTXREF RECORD IS FLAGGED REVERSED.  *
      *                                                                *
      * ONCE PSEOD010 HAS HARD POSTED THE DAY (PSTXREF IS EMPTIED IN   *
      * PSEOD01 STEP PS030) THE ORIGINAL CAN NO LONGER BE FOUND AND A  *
      * REVERSAL MUST BE DONE AS A MANUAL ADJUSTMENT BY DDA OPERATIONS.*
      *                                                                *
      * REVERSING A CREDIT IS A DEBIT AND IS SUBJECT TO THE NORMAL     *
      * FUNDS CHECK IN PSMEMO1 - IF THE CUSTOMER HAS ALREADY SPENT THE *
      * MONEY THE REVERSAL IS REFUSED (NSF). ONLY THE BRANCH CHANNEL   *
      * CAN FORCE IT THROUGH (OVERRIDE). THIS WAS AGREED WITH FRAUD    *
      * OPERATIONS (FO-2010-06) - ONLINE CHANNELS MUST NOT CREATE      *
      * UNARRANGED OVERDRAFTS.                                         *
      *----------------------------------------------------------------*
      * REASON CODES (PLUS ANY RETURNED BY PSMEMO1)                    *
      *   ONFD ORIGINAL POSTING NOT FOUND (OR NOT TODAY)               *
      *   OACC ORIGINAL WAS FOR A DIFFERENT ACCOUNT                    *
      *   AREV ORIGINAL ALREADY REVERSED                               *
      *   RREV CANNOT REVERSE A REVERSAL                               *
      *   NTDY ORIGINAL NOT POSTED TODAY                               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2010-06-28 JNB  ORIGINAL VERSION                               *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'PSREV01 '.
      *
       01  WS-WORK-FIELDS.
           05  WS-RESP                     PIC S9(08) COMP.
           05  WS-RESP-DISPLAY             PIC 9(08).
           05  WS-FILE-NAME                PIC X(08).
           05  WS-ABSTIME                  PIC S9(15) COMP-3.
           05  WS-TODAY                    PIC 9(08).
           05  WS-TODAY-X REDEFINES WS-TODAY
                                           PIC X(08).
           05  WS-ORIG-AMOUNT              PIC S9(13)V99 COMP-3.
           05  WS-ORIG-DR-CR               PIC X(01).
           05  WS-ORIG-TXN-CODE            PIC X(04).
           05  WS-REV-SEQ                  PIC 9(09).
           05  WS-NEW-LEDGER               PIC S9(13)V99 COMP-3.
           05  WS-NEW-AVAIL                PIC S9(13)V99 COMP-3.
           05  WS-RESULT-RC                PIC X(04).
           05  WS-RESULT-REASON            PIC X(04).
           05  WS-RESULT-MESSAGE           PIC X(80).
      *
      *---- SAVED REQUEST (CH-DATA IS REUSED FOR THE PSMEMO1 LINK) ----*
       01  WS-SAVED-REQUEST.
           05  WS-SR-SORT-CODE             PIC X(06).
           05  WS-SR-ACCT-NO               PIC X(08).
           05  WS-SR-ORIG-CHANNEL          PIC X(06).
           05  WS-SR-ORIG-EXT-REF          PIC X(24).
           05  WS-SR-REV-EXT-REF           PIC X(24).
           05  WS-SR-REASON-CD             PIC X(04).
           05  WS-SR-NARRATIVE             PIC X(35).
      *
       01  WS-SAVED-COMMAREA               PIC X(4000).
      *
           COPY PSXREFRC.
      *
           COPY CHCOMMAR.
           COPY CHCOMMA2.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                     PIC X(4000).
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAIN.
      *================================================================*
           MOVE DFHCOMMAREA                TO CH-COMMAREA
                                              WS-SAVED-COMMAREA
           MOVE '0000'                     TO CH-RETURN-CD
           MOVE SPACES                     TO CH-REASON-CD
                                              CH-MESSAGE-TEXT

           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-TODAY-X)
           END-EXEC

           PERFORM 1000-VALIDATE-REQUEST
           IF CH-RC-OK
               PERFORM 2000-FIND-ORIGINAL THRU 2000-EXIT
           END-IF
           IF CH-RC-OK
               PERFORM 3000-POST-REVERSAL THRU 3000-EXIT
           END-IF
           IF CH-RC-OK OR CH-RC-WARNING
               PERFORM 4000-FLAG-ORIGINAL
           END-IF

           PERFORM 5000-BUILD-RESPONSE

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-VALIDATE-REQUEST.
      *================================================================*
           MOVE CHPR-SORT-CODE             TO WS-SR-SORT-CODE
           MOVE CHPR-ACCT-NO               TO WS-SR-ACCT-NO
           MOVE CHPR-ORIG-CHANNEL          TO WS-SR-ORIG-CHANNEL
           MOVE CHPR-ORIG-EXT-REF          TO WS-SR-ORIG-EXT-REF
           MOVE CHPR-REV-EXT-REF           TO WS-SR-REV-EXT-REF
           MOVE CHPR-REASON-CD             TO WS-SR-REASON-CD
           MOVE CHPR-NARRATIVE             TO WS-SR-NARRATIVE

           EVALUATE TRUE
               WHEN CHPR-SORT-CODE NOT NUMERIC
               WHEN CHPR-ACCT-NO NOT NUMERIC
                   MOVE 'INVA'             TO CH-REASON-CD
                   MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPR-ORIG-CHANNEL = SPACES
               WHEN CHPR-ORIG-EXT-REF = SPACES
                   MOVE 'INVO'             TO CH-REASON-CD
                   MOVE 'ORIGINAL CHANNEL AND REFERENCE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPR-REV-EXT-REF = SPACES
               WHEN CHPR-REV-EXT-REF = CHPR-ORIG-EXT-REF
                   MOVE 'INVR'             TO CH-REASON-CD
                   MOVE 'A NEW REFERENCE IS REQUIRED FOR THE REVERSAL'
                                           TO CH-MESSAGE-TEXT
               WHEN CHPR-REASON-CD = SPACES
                   MOVE 'INVC'             TO CH-REASON-CD
                   MOVE 'REVERSAL REASON CODE REQUIRED'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF CH-REASON-CD NOT = SPACES
               MOVE '0016'                 TO CH-RETURN-CD
           END-IF
           .
      *================================================================*
       2000-FIND-ORIGINAL.
      *================================================================*
           MOVE CHPR-ORIG-CHANNEL          TO PXR-CHANNEL-CD
           MOVE CHPR-ORIG-EXT-REF          TO PXR-EXT-REF
           EXEC CICS READ
                     FILE('PSTXREF')
                     INTO(PXR-XREF-RECORD)
                     LENGTH(LENGTH OF PXR-XREF-RECORD)
                     RIDFLD(PXR-KEY)
                     RESP(WS-RESP)
           END-EXEC
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ONFD'             TO CH-REASON-CD
                   MOVE 'ORIGINAL NOT FOUND - ONLY TODAYS POSTINGS'
                                           TO CH-MESSAGE-TEXT
                   GO TO 2000-EXIT
               WHEN OTHER
                   MOVE 'PSTXREF'          TO WS-FILE-NAME
                   PERFORM 9000-FILE-ERROR
                   GO TO 2000-EXIT
           END-EVALUATE

           EVALUATE TRUE
               WHEN PXR-SORT-CODE NOT = CHPR-SORT-CODE
               WHEN PXR-ACCT-NO NOT = CHPR-ACCT-NO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'OACC'             TO CH-REASON-CD
                   MOVE 'ORIGINAL POSTING IS FOR ANOTHER ACCOUNT'
                                           TO CH-MESSAGE-TEXT
               WHEN PXR-REVERSED
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'AREV'             TO CH-REASON-CD
                   MOVE 'ORIGINAL POSTING HAS ALREADY BEEN REVERSED'
                                           TO CH-MESSAGE-TEXT
               WHEN PXR-TXN-CODE = 'REV '
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'RREV'             TO CH-REASON-CD
                   MOVE 'A REVERSAL CANNOT ITSELF BE REVERSED'
                                           TO CH-MESSAGE-TEXT
               WHEN PXR-POST-DATE NOT = WS-TODAY
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'NTDY'             TO CH-REASON-CD
                   MOVE 'ORIGINAL WAS NOT POSTED TODAY'
                                           TO CH-MESSAGE-TEXT
      *        RECORDS WRITTEN BEFORE THE 2010 CHANGE HAVE NO AMOUNT.
               WHEN PXR-AMOUNT NOT NUMERIC
               WHEN PXR-AMOUNT NOT > ZERO
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ONFD'             TO CH-REASON-CD
                   MOVE 'ORIGINAL AMOUNT NOT HELD - MANUAL REVERSAL'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE PXR-AMOUNT         TO WS-ORIG-AMOUNT
                   MOVE PXR-DR-CR-IND      TO WS-ORIG-DR-CR
                   MOVE PXR-TXN-CODE       TO WS-ORIG-TXN-CODE
           END-EVALUATE
           .
       2000-EXIT.
           EXIT.
      *================================================================*
       3000-POST-REVERSAL.
      *================================================================*
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE 'MEMOPOST'                 TO CH-SERVICE-CD
           MOVE SPACES                     TO CH-DATA
           MOVE WS-SR-SORT-CODE            TO CHMP-SORT-CODE
           MOVE WS-SR-ACCT-NO              TO CHMP-ACCT-NO
           MOVE 'REV '                     TO CHMP-TXN-CODE
           IF WS-ORIG-DR-CR = 'D'
               MOVE 'C'                    TO CHMP-DR-CR-IND
           ELSE
               MOVE 'D'                    TO CHMP-DR-CR-IND
           END-IF
           MOVE WS-ORIG-AMOUNT             TO CHMP-AMOUNT
           IF WS-SR-NARRATIVE NOT = SPACES
               MOVE WS-SR-NARRATIVE        TO CHMP-NARRATIVE-1
           ELSE
               STRING 'REVERSAL OF ' WS-ORIG-TXN-CODE
                      DELIMITED BY SIZE
                      INTO CHMP-NARRATIVE-1
               END-STRING
           END-IF
           STRING 'REF ' WS-SR-ORIG-EXT-REF (1:24) ' ' WS-SR-REASON-CD
                  DELIMITED BY SIZE
                  INTO CHMP-NARRATIVE-2
           END-STRING
           MOVE WS-SR-REV-EXT-REF          TO CHMP-EXT-REF
           MOVE WS-TODAY                   TO CHMP-VALUE-DATE
      *    PSMEMO1 HONOURS THE OVERRIDE FOR CHANNEL BRANCH ONLY.
           MOVE 'Y'                        TO CHMP-OVERRIDE-IND

           EXEC CICS LINK
                     PROGRAM('PSMEMO1')
                     COMMAREA(CH-COMMAREA)
                     LENGTH(LENGTH OF CH-COMMAREA)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE '0012'                 TO CH-RETURN-CD
               MOVE 'LNKF'                 TO CH-REASON-CD
               MOVE 'LINK TO PSMEMO1 FAILED - REVERSAL NOT POSTED'
                                           TO CH-MESSAGE-TEXT
               GO TO 3000-EXIT
           END-IF

           IF CH-RC-OK OR CH-RC-WARNING
               MOVE CHMP-POST-SEQ-NO       TO WS-REV-SEQ
               MOVE CHMP-NEW-LEDGER-BAL    TO WS-NEW-LEDGER
               MOVE CHMP-NEW-AVAIL-BAL     TO WS-NEW-AVAIL
           END-IF
           .
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-FLAG-ORIGINAL.
      *================================================================*
           MOVE WS-SR-ORIG-CHANNEL         TO PXR-CHANNEL-CD
           MOVE WS-SR-ORIG-EXT-REF         TO PXR-EXT-REF
           EXEC CICS READ
                     FILE('PSTXREF')
                     INTO(PXR-XREF-RECORD)
                     LENGTH(LENGTH OF PXR-XREF-RECORD)
                     RIDFLD(PXR-KEY)
                     UPDATE
                     RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Y'                    TO PXR-REVERSED-IND
               EXEC CICS REWRITE
                         FILE('PSTXREF')
                         FROM(PXR-XREF-RECORD)
                         LENGTH(LENGTH OF PXR-XREF-RECORD)
                         RESP(WS-RESP)
               END-EXEC
           END-IF
      *    IF THE FLAG CANNOT BE SET THE WHOLE UOW (INCLUDING THE
      *    REVERSING POSTING) IS BACKED OUT - OTHERWISE A SECOND
      *    REVERSAL WOULD BE POSSIBLE.
           IF WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS ABEND
                         ABCODE('PSR1')
               END-EXEC
           END-IF
           .
      *================================================================*
       5000-BUILD-RESPONSE.
      *================================================================*
      *    RESTORE THE CALLER'S REQUEST AREA AND FILL THE RESPONSE,
      *    KEEPING THE RETURN / REASON / MESSAGE FROM THE PROCESSING.
           MOVE CH-RETURN-CD               TO WS-RESULT-RC
           MOVE CH-REASON-CD               TO WS-RESULT-REASON
           MOVE CH-MESSAGE-TEXT            TO WS-RESULT-MESSAGE
           MOVE WS-SAVED-COMMAREA          TO CH-COMMAREA
           MOVE WS-RESULT-RC               TO CH-RETURN-CD
           MOVE WS-RESULT-REASON           TO CH-REASON-CD
           MOVE WS-RESULT-MESSAGE          TO CH-MESSAGE-TEXT
           IF CH-RC-OK OR CH-RC-WARNING
               MOVE WS-REV-SEQ             TO CHPR-REV-SEQ-NO
               MOVE WS-ORIG-AMOUNT         TO CHPR-ORIG-AMOUNT
               MOVE WS-ORIG-DR-CR          TO CHPR-ORIG-DR-CR-IND
               MOVE WS-NEW-LEDGER          TO CHPR-NEW-LEDGER-BAL
               MOVE WS-NEW-AVAIL           TO CHPR-NEW-AVAIL-BAL
               IF CH-RC-OK
                   MOVE 'POSTING REVERSED' TO CH-MESSAGE-TEXT
               END-IF
           ELSE
               MOVE ZERO                   TO CHPR-REV-SEQ-NO
                                              CHPR-ORIG-AMOUNT
                                              CHPR-NEW-LEDGER-BAL
                                              CHPR-NEW-AVAIL-BAL
               MOVE SPACE                  TO CHPR-ORIG-DR-CR-IND
           END-IF
           .
      *================================================================*
       9000-FILE-ERROR.
      *================================================================*
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
