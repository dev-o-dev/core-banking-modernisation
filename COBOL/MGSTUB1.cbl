       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGSTUB1.
       AUTHOR.        J N BRADSHAW.
       INSTALLATION.  MEGABANK PLC - GROUP IT - CORE SYSTEMS.
       DATE-WRITTEN.  2009-10-02.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM  : MGSTUB1                                             *
      * TITLE    : MORTGAGE ENQUIRY - CHANNEL SERVICE MORTINQ (STUB)   *
      * TYPE     : CICS - LINKED FROM CHROUTER                         *
      *----------------------------------------------------------------*
      * MORTGAGE ACCOUNTS ARE SERVICED BY THE MORTGAGE ADMINISTRATION  *
      * PACKAGE IN REGION CICSMTG1, NOT BY THE DDA SYSTEM. THE         *
      * "MORTGAGES ONLINE" PROJECT (PID MBK-2009-117) WAS TO PROVIDE A *
      * DPL LINK TO THAT REGION. THE PROJECT WAS PAUSED IN 2010-Q1 AND *
      * THIS STUB WAS LEFT IN PLACE SO THAT THE CHANNEL COULD BE       *
      * RELEASED WITH THE SERVICE CODE ALREADY DEFINED.                *
      *                                                                *
      * THE IBANK CHANNEL SHOWS MORTGAGE BALANCES FROM ITS OWN SHADOW  *
      * TABLE, LOADED MONTHLY FROM THE MORTGAGE SYSTEM - NOT VIA HERE. *
      *----------------------------------------------------------------*
      * TO IMPLEMENT: REPLACE 0000-MAIN WITH                           *
      *     EXEC CICS LINK PROGRAM('MTGINQ01') SYSID('MTG1') ...       *
      * AND MAP THE MORTGAGE PACKAGE COMMAREA (MTGCOMM) TO CHMG-RESP.  *
      *----------------------------------------------------------------*
      * HISTORY                                                        *
      * 2009-10-02 JNB  ORIGINAL VERSION (STUB)                        *
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID                   PIC X(08) VALUE 'MGSTUB1 '.
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

           MOVE '0008'                     TO CH-RETURN-CD
           MOVE 'MGNA'                     TO CH-REASON-CD
           MOVE 'MORTGAGE ENQUIRY NOT AVAILABLE ON THIS PLATFORM'
                                           TO CH-MESSAGE-TEXT

           MOVE CH-COMMAREA                TO DFHCOMMAREA
           EXEC CICS RETURN
           END-EXEC
           .
