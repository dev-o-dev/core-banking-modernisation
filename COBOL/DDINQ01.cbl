       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DDINQ01.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2000-06-21.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : DDINQ01                                             *
      * TITLE    : ACCOUNT BALANCE ENQUIRY - CHANNEL SERVICE ACCTINQ   *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : ACCTMST (READ ONLY)                                 *
      * COMMAREA : CHCOMMAR / CH-ACCTINQ-AREA                          *
      *----------------------------------------------------------------*
      * RETURNS BALANCES AS HELD ON THE ACCOUNT MASTER. THE AVAILABLE  *
      * BALANCE RETURNED IS CLEARED FUNDS (EXCLUDING THE OVERDRAFT     *
      * LIMIT); THE CHANNEL ADDS CHAI-OD-LIMIT FOR DISPLAY.            *
      *                                                                *
      * NOTE: THE CALLER IS NOT CHECKED AGAINST THE ACCOUNT HOLDER.    *
      * ENTITLEMENT IS THE CHANNEL'S RESPONSIBILITY (INTERFACE         *
      * AGREEMENT IA-IBK-002 SECTION 3).                               *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-21 DKP  ORIGINAL VERSION                               *
      * 2001-04-30 DKP  IBAN                                           *
      * 2004-08-09 SAT  HOLD AMOUNT RETURNED                           *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'DDINQ01 '.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP-DISPLAY                 PIC 9(08).
       01  WS-ACCT-KEY.
           05  WS-KEY-SORT-CODE            PIC X(06).
           05  WS-KEY-ACCT-NO              PIC X(08).
      *
           COPY DDACCREC.
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
           MOVE SPACES                     TO CHAI-RESP

           IF CHAI-SORT-CODE NOT NUMERIC
           OR CHAI-ACCT-NO   NOT NUMERIC
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'ACCX'                 TO CH-REASON-CD
               MOVE 'SORT CODE / ACCOUNT NUMBER INVALID'
                                           TO CH-MESSAGE-TEXT
           ELSE
               PERFORM 1000-READ-ACCOUNT
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-READ-ACCOUNT.
      *================================================================*
           MOVE CHAI-SORT-CODE             TO WS-KEY-SORT-CODE
           MOVE CHAI-ACCT-NO               TO WS-KEY-ACCT-NO

           EXEC CICS READ
                     FILE('ACCTMST')
                     INTO(ACM-ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACM-ACCOUNT-RECORD)
                     RIDFLD(WS-ACCT-KEY)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2000-BUILD-RESPONSE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'ANFD'             TO CH-REASON-CD
                   MOVE 'ACCOUNT NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN DFHRESP(NOTOPEN)
               WHEN DFHRESP(DISABLED)
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'FNOP'             TO CH-REASON-CD
                   MOVE 'ACCOUNT FILE NOT AVAILABLE (BATCH WINDOW)'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE WS-RESP            TO WS-RESP-DISPLAY
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'FERR'             TO CH-REASON-CD
                   STRING 'ACCTMST READ ERROR - EIBRESP '
                          WS-RESP-DISPLAY
                          DELIMITED BY SIZE
                          INTO CH-MESSAGE-TEXT
                   END-STRING
           END-EVALUATE
           .
      *================================================================*
       2000-BUILD-RESPONSE.
      *================================================================*
           MOVE ACM-CIF-NO                 TO CHAI-CIF-NO
           MOVE ACM-IBAN                   TO CHAI-IBAN
           MOVE ACM-ACCT-TYPE-CD           TO CHAI-ACCT-TYPE-CD
           MOVE ACM-PROD-CD                TO CHAI-PROD-CD
           MOVE ACM-CCY-CD                 TO CHAI-CCY-CD
           MOVE ACM-ACCT-STATUS            TO CHAI-ACCT-STATUS
           MOVE ACM-LEDGER-BAL             TO CHAI-LEDGER-BAL
           MOVE ACM-AVAIL-BAL              TO CHAI-AVAIL-BAL
           MOVE ACM-OD-LIMIT               TO CHAI-OD-LIMIT
           MOVE ACM-HOLD-AMT               TO CHAI-HOLD-AMT
           MOVE ACM-INT-RATE-CR            TO CHAI-INT-RATE-CR
           MOVE ACM-OPEN-DATE              TO CHAI-OPEN-DATE
           MOVE ACM-LAST-TXN-DATE          TO CHAI-LAST-TXN-DATE
           MOVE ACM-POST-RESTRICT          TO CHAI-POST-RESTRICT

           EVALUATE TRUE
               WHEN ACM-CLOSED
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'ACLS'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS CLOSED'
                                           TO CH-MESSAGE-TEXT
               WHEN ACM-FROZEN
                   MOVE '0004'             TO CH-RETURN-CD
                   MOVE 'AFRZ'             TO CH-REASON-CD
                   MOVE 'ACCOUNT IS FROZEN - REFER TO BRANCH'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE '0000'             TO CH-RETURN-CD
           END-EVALUATE
           .
