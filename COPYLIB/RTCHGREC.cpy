      *================================================================*
      * COPYBOOK : RTCHGREC                                            *
      * TITLE    : INTEREST RATE CHANGE - INPUT AND PENDING RECORD     *
      * DATASETS : MBK.PRD.INT.RATECHG.INPUT(0)  FB 100 (FROM PRODUCT  *
      *                                          CONTROL, PANEL MBKRT1)*
      *            MBK.PRD.INT.RATEPEND          VSAM KSDS            *
      *                                          RECORDSIZE(100 100)   *
      *                                          KEYS(22 0)            *
      *----------------------------------------------------------------*
      * A RATE CHANGE IS ANNOUNCED (NOTICES SENT, PENDING RECORD       *
      * WRITTEN) AND LATER APPLIED ON ITS EFFECTIVE DATE BY INRAT010.  *
      * RATE TYPE  C = CREDIT TIER RATE (TIER 1-5)  B = BONUS RATE     *
      *            A = ARRANGED OD RATE  U = UNARRANGED OD RATE        *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2004-03-15 SAT  ORIGINAL VERSION (BCOBS-STYLE NOTICE PERIODS)  *
      *================================================================*
       01  RTC-RATE-CHANGE.
           05  RTC-KEY.
               10  RTC-PROD-CD             PIC X(10).
               10  RTC-EFFECTIVE-DATE      PIC 9(08).
               10  RTC-RATE-TYPE           PIC X(01).
                   88  RTC-CREDIT-TIER         VALUE 'C'.
                   88  RTC-BONUS               VALUE 'B'.
                   88  RTC-ARRANGED            VALUE 'A'.
                   88  RTC-UNARRANGED          VALUE 'U'.
                   88  RTC-TYPE-VALID          VALUE 'C' 'B' 'A' 'U'.
               10  RTC-TIER                PIC 9(01).
               10  FILLER                  PIC X(02).
           05  RTC-OLD-RATE                PIC S9(03)V9(04) COMP-3.
           05  RTC-NEW-RATE                PIC S9(03)V9(04) COMP-3.
           05  RTC-DIRECTION               PIC X(01).
               88  RTC-INCREASE                VALUE 'I'.
               88  RTC-DECREASE                VALUE 'D'.
           05  RTC-ANNOUNCE-DATE           PIC 9(08).
           05  RTC-STATUS                  PIC X(01).
               88  RTC-ANNOUNCED               VALUE 'N'.
               88  RTC-APPLIED                 VALUE 'A'.
               88  RTC-CANCELLED               VALUE 'X'.
           05  RTC-REASON-CD               PIC X(04).
           05  RTC-BASE-RATE-IND           PIC X(01).
           05  RTC-NOTICE-COUNT            PIC 9(07) COMP-3.
           05  RTC-APPLIED-DATE            PIC 9(08).
           05  RTC-REQUESTED-BY            PIC X(08).
           05  RTC-APPROVED-BY             PIC X(08).
           05  FILLER                      PIC X(27).
