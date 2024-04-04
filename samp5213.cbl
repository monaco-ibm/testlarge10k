      ******************************************************************
      * Licensed materials - Property of IBM                           *
      * 5724-T07(C) Copyright IBM Corp. 2014                           *
      * All rights reserved                                            *
      * US Government users restricted rights  -  Use, duplication or  *
      * disclosure restricted by GSA ADP schedule contract with IBM    *
      * Corp.                                                          *
      *                                                                *
      * IBM Rational Developer for System z (RDz)                      *
      * IBM z/OS Automated Unit Testing Framework (zUnit)              *
      * Enterprise COBOL zUnit Test Case Sample TOURFILE.cbl           *
      *                                                                *
      * @since   9.1.1.0                                               *
      * @version 9.1.1.0                                               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOURFILE.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT TOURDATA   ASSIGN TO INFILE.
           SELECT TOURMST  ASSIGN TO OUTFILE.
      *
       DATA DIVISION.
       FILE SECTION.
      *Tour data file
      * prereq: all records are sorted by I-TOUR-CODE
       FD TOURDATA RECORDING MODE IS F
           LABEL RECORD IS STANDARD BLOCK CONTAINS 0 RECORDS.
       01 DATA-REC.
          03 I-TOUR.
            05 I-TOUR-CODE             PIC X(03).
            05 I-TOUR-NAME             PIC X(20).
            05 I-TOUR-EXPENSE          PIC 9(07).
            05 I-NUMBER-ENTRIES        PIC 9(03).
      *Tour master file
       FD TOURMST RECORDING MODE IS F
           LABEL RECORD IS STANDARD BLOCK CONTAINS 0 RECORDS.
       01 TOUR-REC.
          03 O-TOUR-CODE               PIC X(03).
          03 O-TOUR-NAME               PIC X(20).
          03 O-TOUR-EXPENSE            PIC 9(07).
          03 O-NUMBER-ENTRIES          PIC 9(04).
          03 O-TOTAL-EXPENSE           PIC 9(09).

       WORKING-STORAGE SECTION.
       01 RECORD-STATUS PIC X(01).
          88 END-OF-RECORD  VALUE 'E'.
          88 NOT-END-OF-RECORD VALUE 'R'.
       01  RESULT-CODE   PIC X(01).
          88 CODE-VALID  VALUE 'V'.
          88 CODE-INVALID VALUE 'I'.
       01 WORK-REC.
          03 W-TOUR-CODE             PIC X(03).
          03 W-TOUR-NAME             PIC X(20).
          03 W-TOUR-EXPENSE          PIC 9(07).
          03 W-NUMBER-ENTRIES        PIC 9(03).
       01  W-TOTAL-EXPENSE  PIC 9(09).
      *
       LINKAGE SECTION.
       01 CHECK-CODE PIC X(01).
          88 CHECK-TOUR-CODE  VALUE 'Y'.
          88 NO-CHECK-TOUR-CODE VALUE 'N'.

       PROCEDURE DIVISION USING CHECK-CODE.
       MAINLINE SECTION.
           DISPLAY 'TOUR1 STARTED...'
           PERFORM INIT.
           PERFORM PROCESS-ALL-RECORDS UNTIL END-OF-RECORD.
           PERFORM END-PROCESS.
           DISPLAY 'TOUR1 SUCCESSFUL'
      *
           GOBACK.
      **********************************************
      * Open files and initialize variables
      **********************************************
       INIT.
           OPEN INPUT TOURDATA
                OUTPUT TOURMST
           SET NOT-END-OF-RECORD TO TRUE
           MOVE SPACE TO WORK-REC
           MOVE ZERO TO W-NUMBER-ENTRIES
           MOVE ZERO TO W-TOUR-EXPENSE
           MOVE ZERO TO W-TOTAL-EXPENSE
           MOVE SPACE TO O-TOUR-CODE
           MOVE ZERO TO O-NUMBER-ENTRIES
           MOVE ZERO TO O-TOUR-EXPENSE
           MOVE ZERO TO O-TOTAL-EXPENSE.

      **********************************************
      * Process all records of tour data file
      **********************************************
       PROCESS-ALL-RECORDS.
           PERFORM COMBINE-AND-WRITE
           READ TOURDATA
           AT END
             SET END-OF-RECORD TO TRUE
           NOT AT END
             MOVE DATA-REC TO WORK-REC
             DISPLAY WORK-REC
             IF CHECK-TOUR-CODE
               CALL 'CHKCODE' USING I-TOUR-CODE RESULT-CODE
               IF CODE-INVALID
                 MOVE SPACES TO TOUR-REC
      * The following line is incorrect
                 MOVE "CODE-INVALID" TO O-TOUR-NAME
      *           MOVE "CODE-INVALID" TO W-TOUR-NAME
                 PERFORM END-PROCESS
                 GOBACK
               END-IF
             END-IF
           END-READ.

      **************************************************
      * Combine records of same tour code together,
      *  and write it to tour master file
      **************************************************
       COMBINE-AND-WRITE.
           IF O-TOUR-CODE NOT EQUAL SPACE THEN
             IF W-TOUR-CODE = O-TOUR-CODE
               COMPUTE O-NUMBER-ENTRIES =
                O-NUMBER-ENTRIES + W-NUMBER-ENTRIES
               COMPUTE W-TOTAL-EXPENSE =
                 W-TOUR-EXPENSE * W-NUMBER-ENTRIES
               COMPUTE O-TOTAL-EXPENSE =
                O-TOTAL-EXPENSE + W-TOTAL-EXPENSE
             ELSE
               WRITE TOUR-REC
               MOVE W-TOUR-CODE TO O-TOUR-CODE
               MOVE W-TOUR-NAME TO O-TOUR-NAME
               MOVE W-TOUR-EXPENSE TO O-TOUR-EXPENSE
               MOVE ZERO TO O-NUMBER-ENTRIES
               MOVE ZERO TO O-TOTAL-EXPENSE
             END-IF
           ELSE
             MOVE W-TOUR-CODE TO O-TOUR-CODE
             MOVE W-TOUR-NAME TO O-TOUR-NAME
             MOVE W-TOUR-EXPENSE TO O-TOUR-EXPENSE
             MOVE W-NUMBER-ENTRIES TO O-NUMBER-ENTRIES
             COMPUTE W-TOTAL-EXPENSE =
                 W-TOUR-EXPENSE * W-NUMBER-ENTRIES
             MOVE W-TOTAL-EXPENSE TO O-TOTAL-EXPENSE
           END-IF.

      **********************************************
      * Write the last record, and close files
      **********************************************
       END-PROCESS.
           PERFORM COMBINE-AND-WRITE
           WRITE TOUR-REC
           CLOSE TOURDATA TOURMST.
       END PROGRAM 'TOURFILE'. 