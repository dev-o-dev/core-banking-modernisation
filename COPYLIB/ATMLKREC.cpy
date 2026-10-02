      *================================================================*
      * COPYBOOK : ATMLKREC                                            *
      * TITLE    : LINK ATM NETWORK - DAILY SETTLEMENT FILE RECORD     *
      * DATASET  : MBK.PRD.ATM.LINK.SETTLE(0)  RECFM=FB LRECL=200      *
      *            RECEIVED FROM LINK SCHEME LTD VIA CONNECT:DIRECT    *
      *            (PROCESS LNKSTL1, EVENT ATMLINK) BY 04:00 DAILY     *
      *----------------------------------------------------------------*
      * ONE FILE PER LINK SETTLEMENT DAY:                              *
      *   H  HEADER   - ONE, FIRST RECORD                              *
      *   D  DETAIL   - ONE PER ATM TRANSACTION IN WHICH MEGABANK IS   *
      *                 A PARTY:                                       *
      *                 ROLE 'I' ISSUER   - A MEGABANK CARD USED AT    *
      *                                     ANOTHER MEMBER'S ATM       *
      *                 ROLE 'A' ACQUIRER - ANOTHER MEMBER'S CARD USED *
      *                                     AT A MEGABANK ATM          *
      *   T  TRAILER  - ONE, LAST RECORD, WITH LINK'S CONTROL TOTALS   *
      *                 AND THE NET SETTLEMENT FIGURE MEGABANK PAYS    *
      *                 ('P') OR RECEIVES ('R') AT THE BANK OF ENGLAND *
      *                 (RTGS SETTLEMENT CYCLE 1, 11:00 NEXT DAY).     *
      *                                                                *
      * AMOUNTS ARE UNSIGNED STERLING. REVERSALS (TYPE 'RV') CARRY THE *
      * ORIGINAL TRACE NUMBER AND ARE NETTED OFF IN THE TRAILER TOTALS *
      * BY LINK. INTERCHANGE IS THE LINK MULTILATERAL INTERCHANGE FEE  *
      * (MIF) PAID BY THE ISSUER TO THE ACQUIRER - PER WITHDRAWAL OR   *
      * BALANCE ENQUIRY, NOT AD VALOREM.                               *
      *                                                                *
      * THE SURCHARGE (DIRECT CHARGE) IS THE AMOUNT A PAY-TO-USE ATM   *
      * OPERATOR DISCLOSED ON SCREEN AND THE CARDHOLDER ACCEPTED. LINK *
      * COLLECTS IT FROM THE ISSUER FOR THE ACQUIRER. MEGABANK ATMS ARE*
      * FREE-TO-USE SO ACQUIRER RECORDS ALWAYS HAVE ZERO SURCHARGE.    *
      *----------------------------------------------------------------*
      * TRAILER RECONCILIATION (ATMSTL10 - MISMATCH ABENDS U1004):     *
      *   ATLT-RECORD-COUNT  = DETAIL RECORDS ON THE FILE              *
      *   ATLT-ISS-AMOUNT    = ISSUER WD LESS ISSUER RV AMOUNTS        *
      *   ATLT-ACQ-AMOUNT    = ACQUIRER WD LESS ACQUIRER RV AMOUNTS    *
      *   ATLT-NET-SETTLE    = ACQ AMOUNT + ACQ FEES                   *
      *                        - ISS AMOUNT - ISS FEES - ISS SURCHARGE *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2014-09-08 RMC  ORIGINAL VERSION (LINK MEMBERSHIP - REPLACES   *
      *                 THE BILATERAL VOCALINK FILE)                   *
      * 2015-03-16 RMC  SURCHARGE FIELDS (PAY-TO-USE ATMS)             *
      * 2017-01-09 PDQ  ATM COUNTRY FOR CHANNEL ISLANDS / ISLE OF MAN  *
      *                 ATMS (OFF-SHORE FEE RULE - CR 16/0388)         *
      *================================================================*
       01  ATL-LINK-RECORD.
           05  ATL-REC-TYPE                PIC X(01).
               88  ATL-HEADER                  VALUE 'H'.
               88  ATL-DETAIL                  VALUE 'D'.
               88  ATL-TRAILER                 VALUE 'T'.
           05  ATL-BODY                    PIC X(199).
      *
      *---- HEADER ----------------------------------------------------*
           05  ATL-HDR-DATA REDEFINES ATL-BODY.
               10  ATLH-SCHEME-ID          PIC X(08).
                   88  ATLH-LINK-SCHEME        VALUE 'LINK    '.
               10  ATLH-MEMBER-ID          PIC X(06).
               10  ATLH-SETTLE-DATE        PIC 9(08).
               10  ATLH-FILE-SEQ           PIC 9(06).
               10  ATLH-CREATE-DATE        PIC 9(08).
               10  ATLH-CREATE-TIME        PIC 9(06).
               10  FILLER                  PIC X(157).
      *
      *---- DETAIL ----------------------------------------------------*
           05  ATL-DTL-DATA REDEFINES ATL-BODY.
               10  ATLD-ROLE               PIC X(01).
                   88  ATLD-ISSUER             VALUE 'I'.
                   88  ATLD-ACQUIRER           VALUE 'A'.
               10  ATLD-TXN-TYPE           PIC X(02).
                   88  ATLD-WITHDRAWAL         VALUE 'WD'.
                   88  ATLD-BALANCE-ENQ        VALUE 'BE'.
                   88  ATLD-REVERSAL           VALUE 'RV'.
                   88  ATLD-TYPE-VALID         VALUE 'WD' 'BE' 'RV'.
               10  ATLD-TXN-DATE           PIC 9(08).
               10  ATLD-TXN-TIME           PIC 9(06).
               10  ATLD-CARD-NO            PIC X(19).
               10  ATLD-CARD-SORT-CODE     PIC X(06).
               10  ATLD-CARD-ACCT-NO       PIC X(08).
               10  ATLD-ISSUER-MEMBER      PIC X(06).
               10  ATLD-ACQUIRER-MEMBER    PIC X(06).
               10  ATLD-ATM-ID             PIC X(08).
               10  ATLD-ATM-LOCATION       PIC X(30).
               10  ATLD-ATM-CTRY           PIC X(02).
                   88  ATLD-ATM-UK             VALUE 'GB'.
                   88  ATLD-ATM-OFFSHORE       VALUE 'JE' 'GG' 'IM'.
               10  ATLD-AMOUNT             PIC 9(09)V99.
               10  ATLD-SURCHARGE          PIC 9(03)V99.
               10  ATLD-INTERCHANGE        PIC 9(03)V99.
               10  ATLD-CCY-CD             PIC X(03).
               10  ATLD-TRACE-NO           PIC X(12).
               10  ATLD-ORIG-TRACE-NO      PIC X(12).
               10  ATLD-RESP-CD            PIC X(02).
                   88  ATLD-APPROVED           VALUE '00'.
                   88  ATLD-PART-DISPENSE      VALUE '10'.
               10  FILLER                  PIC X(47).
      *
      *---- TRAILER ---------------------------------------------------*
           05  ATL-TRL-DATA REDEFINES ATL-BODY.
               10  ATLT-RECORD-COUNT       PIC 9(09).
               10  ATLT-ISS-COUNT          PIC 9(09).
               10  ATLT-ISS-AMOUNT         PIC 9(13)V99.
               10  ATLT-ISS-FEES           PIC 9(11)V99.
               10  ATLT-ISS-SURCHARGE      PIC 9(11)V99.
               10  ATLT-ACQ-COUNT          PIC 9(09).
               10  ATLT-ACQ-AMOUNT         PIC 9(13)V99.
               10  ATLT-ACQ-FEES           PIC 9(11)V99.
               10  ATLT-NET-SETTLE         PIC 9(13)V99.
               10  ATLT-NET-DIRECTION      PIC X(01).
                   88  ATLT-MEMBER-PAYS        VALUE 'P'.
                   88  ATLT-MEMBER-RECEIVES    VALUE 'R'.
               10  FILLER                  PIC X(87).
