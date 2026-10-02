      *================================================================*
      * COPYBOOK : ABNDPROC   (PROCEDURE DIVISION COPYBOOK)            *
      * TITLE    : STANDARD BATCH ABEND ROUTINE                        *
      *----------------------------------------------------------------*
      * CALLER SETS ABN-PARAGRAPH, ABN-ABEND-CODE, ABN-MESSAGE AND     *
      * (OPTIONALLY) ABN-FILE-STATUS / ABN-SQLCODE / ABN-KEY-DATA,     *
      * THEN PERFORMS Z999-ABEND. DB2 PROGRAMS MUST ROLLBACK FIRST.    *
      * THE STEP ENDS WITH A USER ABEND SO THE SCHEDULER HALTS THE     *
      * NETWORK - OPERATIONS RUNBOOK SECTION 12 GIVES RESTART ACTIONS. *
      *================================================================*
       Z999-ABEND.
           DISPLAY '*************************************************'
           DISPLAY '* MBK BATCH ABEND                               *'
           DISPLAY '*************************************************'
           DISPLAY '* PROGRAM   : ' ABN-PROGRAM-ID
           DISPLAY '* PARAGRAPH : ' ABN-PARAGRAPH
           DISPLAY '* ABEND CODE: U' ABN-ABEND-CODE
           DISPLAY '* MESSAGE   : ' ABN-MESSAGE
           IF ABN-FILE-STATUS NOT = SPACES
               DISPLAY '* FILE STAT : ' ABN-FILE-STATUS
           END-IF
           IF ABN-SQLCODE NOT = ZERO
               DISPLAY '* SQLCODE   : ' ABN-SQLCODE
           END-IF
           IF ABN-KEY-DATA NOT = SPACES
               DISPLAY '* KEY       : ' ABN-KEY-DATA
           END-IF
           DISPLAY '*************************************************'
           CALL 'CEE3ABD' USING ABN-ABEND-CODE ABN-TIMING
           .
