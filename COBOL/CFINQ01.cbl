       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFINQ01.
       AUTHOR.        D K PATEL.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2000-06-19.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : CFINQ01                                             *
      * TITLE    : CIF CUSTOMER ENQUIRY - CHANNEL SERVICE CIFINQ       *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      * FILES    : CIFMAST (READ ONLY)                                 *
      * COMMAREA : CHCOMMAR / CH-CIFINQ-AREA                           *
      *----------------------------------------------------------------*
      * RETURNS THE FULL CIF RECORD IMAGE. CLOSED AND DECEASED         *
      * CUSTOMERS ARE RETURNED WITH A WARNING (0004) SO THE CHANNEL    *
      * CAN DECIDE WHETHER TO PERMIT LOGON.                            *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2000-06-19 DKP  ORIGINAL VERSION                               *
      * 2003-05-09 SAT  WARN ON DECEASED (MLR 2003)                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'CFINQ01 '.
       01  WS-RESP                         PIC S9(08) COMP.
       01  WS-RESP-DISPLAY                 PIC 9(08).
      *
           COPY CFCUSREC.
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
           MOVE SPACES                     TO CHCI-RESP

           IF CHCI-CIF-NO = SPACES OR LOW-VALUES
               MOVE '0016'                 TO CH-RETURN-CD
               MOVE 'CIFX'                 TO CH-REASON-CD
               MOVE 'CIF NUMBER NOT SUPPLIED'
                                           TO CH-MESSAGE-TEXT
           ELSE
               PERFORM 1000-READ-CIF
           END-IF

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
      *================================================================*
       1000-READ-CIF.
      *================================================================*
           EXEC CICS READ
                     FILE('CIFMAST')
                     INTO(CIF-CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CIF-CUSTOMER-RECORD)
                     RIDFLD(CHCI-CIF-NO)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE CIF-CUSTOMER-RECORD
                                           TO CHCI-CUST-IMAGE
                   EVALUATE TRUE
                       WHEN CIF-DECEASED
                           MOVE '0004'     TO CH-RETURN-CD
                           MOVE 'DCSD'     TO CH-REASON-CD
                           MOVE 'CUSTOMER MARKED AS DECEASED'
                                           TO CH-MESSAGE-TEXT
                       WHEN CIF-CLOSED
                           MOVE '0004'     TO CH-RETURN-CD
                           MOVE 'CLSD'     TO CH-REASON-CD
                           MOVE 'CUSTOMER RELATIONSHIP CLOSED'
                                           TO CH-MESSAGE-TEXT
                       WHEN CIF-BLOCKED
                           MOVE '0004'     TO CH-RETURN-CD
                           MOVE 'BLKD'     TO CH-REASON-CD
                           MOVE 'CUSTOMER BLOCKED - REFER TO BRANCH'
                                           TO CH-MESSAGE-TEXT
                       WHEN OTHER
                           MOVE '0000'     TO CH-RETURN-CD
                   END-EVALUATE
               WHEN DFHRESP(NOTFND)
                   MOVE '0008'             TO CH-RETURN-CD
                   MOVE 'CNFD'             TO CH-REASON-CD
                   MOVE 'CUSTOMER NOT FOUND'
                                           TO CH-MESSAGE-TEXT
               WHEN DFHRESP(NOTOPEN)
               WHEN DFHRESP(DISABLED)
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'FNOP'             TO CH-REASON-CD
                   MOVE 'CIF FILE NOT AVAILABLE (BATCH WINDOW)'
                                           TO CH-MESSAGE-TEXT
               WHEN OTHER
                   MOVE WS-RESP            TO WS-RESP-DISPLAY
                   MOVE '0012'             TO CH-RETURN-CD
                   MOVE 'FERR'             TO CH-REASON-CD
                   STRING 'CIFMAST READ ERROR - EIBRESP '
                          WS-RESP-DISPLAY
                          DELIMITED BY SIZE
                          INTO CH-MESSAGE-TEXT
                   END-STRING
           END-EVALUATE
           .
