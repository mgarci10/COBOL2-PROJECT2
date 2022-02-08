      ******************************************************************
      *PROGRAM : PROJECT 2   QTR PAY RPT with SORT  (name within dept) *
      *AUTHOR  : Gail Fulenwider, modified by       Mario Garcia       *
      *DATE    : 01/24/2022                                            *
      *ABSTRACT: Ctrl break on dept, unordered input data file.        *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARCIA-P02-SORT.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-MST ASSIGN TO 'p02-data-unordered.dat'
                          ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PR-RPT  ASSIGN TO 'LNAME-p02-sort.rpt'
                          ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SRT-SPACE  ASSIGN TO 'srt-space.dat'
                          ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  EMP-MST.
       01  EMP-REC.
           03  EMP-DEPT                    PIC 99.
           03  EMP-ID                      PIC X(4).
           03  EMP-NAME.
               05  EMP-LNAME               PIC X(15).
               05  EMP-FNAME               PIC X(15).
           03  EMP-QTR-PAY                 PIC 9(5)V99.

       SD SRT-SPACE.
       01 SRT-REC.
           03 SRT-DEPT                     PIC 99.
           03 SRT-ID                       PIC X(4).
           03  SRT-NAME.
               05  SRT-LNAME               PIC X(15).
               05  SRT-FNAME               PIC X(15).
           03  SRT-QTR-PAY                 PIC 9(5)V99.


       FD  PR-RPT.
       01  PR-RPT-REC                      PIC X(80).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-SYS-DATE.
           03  WS-SYS-YR.
               05  WS-SYS-YR-CENTURY       PIC 99.
               05  WS-SYS-YR-DECADE        PIC 99.
           03  WS-SYS-MO                   PIC 99.
           03  WS-SYS-DAY                  PIC 99.
           03  WS-SYS-HR                   PIC 99.
           03  WS-SYS-MIN                  PIC 99.
           03  WS-SYS-SEC                  PIC 99.

       01  WS-RPT-TITLE-LN.
           03  FILLER      PIC X(28) VALUE "P02-MARIO'S SOLUTION".
           03  FILLER      PIC X(42) VALUE 'QUARTERLY PAYROLL BY DEPT '.
           03  WS-RPT-DATE-TIME.
               05  WS-RPT-MO               PIC 99.
               05  FILLER                  PIC X     VALUE '/'.
               05  WS-RPT-DAY              PIC 99.
               05  FILLER                  PIC X     VALUE '/'.
               05  WS-RPT-YR               PIC 9999.

       01  WS-RPT-BLANK-LN                 PIC X(80) VALUE SPACES.
       01  WS-DTL-HDG.
           03  FILLER                      PIC X(10) VALUE '          '.
           03  FILLER                      PIC X(10) VALUE '  DEPT    '.
           03  FILLER                      PIC X(10) VALUE 'EMP ID  LA'.
           03  FILLER                      PIC X(10) VALUE 'ST NAME   '.
           03  FILLER                      PIC X(10) VALUE '    FIRST '.
           03  FILLER                      PIC X(10) VALUE 'NAME      '.
           03  FILLER                      PIC X(10) VALUE '  QTR PAY '.
           03  FILLER                      PIC X(10) VALUE '          '.
       01  WS-DTL-DASH.
           03  FILLER                      PIC X(10) VALUE '          '.
           03  FILLER                      PIC X(10) VALUE '--------  '.
           03  FILLER                      PIC X(10) VALUE '------  --'.
           03  FILLER                      PIC X(10) VALUE '----------'.
           03  FILLER                      PIC X(10) VALUE '--- ------'.
           03  FILLER                      PIC X(10) VALUE '--------- '.
           03  FILLER                      PIC X(10) VALUE '----------'.
           03  FILLER                      PIC X(10) VALUE '          '.
       01  WS-DTL-LN.
           03  FILLER                      PIC X(10)       VALUE SPACES.
           03  WS-DTL-DEPT-NUM             PIC 99.
           03  FILLER                      PIC X           VALUE SPACES.
           03  WS-DTL-DEPT-NAME            PIC X(5).
           03  FILLER                      PIC X(3)        VALUE SPACES.
           03  WS-DTL-ID                   PIC X(4).
           03  FILLER                      PIC X(3)        VALUE SPACES.
           03  WS-DTL-LNAME                PIC X(15).
           03  FILLER                      PIC X           VALUE SPACES.
           03  WS-DTL-FNAME                PIC X(15).
           03  FILLER                      PIC X           VALUE SPACES.
           03  WS-DTL-PAY                  PIC ZZZ,ZZ9.99.
           03  FILLER                      PIC X(10)       VALUE SPACES.
       01  WS-DTL-TOT-DASH.
           03  FILLER                      PIC X(60) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE '----------'.
           03  FILLER                      PIC X(10) VALUE SPACES.
       01  WS-DTL-TOT-LN.
           03  FILLER                      PIC X(49) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE 'DEPT TOTAL'.
           03  FILLER                      PIC X     VALUE SPACES.
           03  WS-DTL-TOT                  PIC ZZZ,ZZ9.99.
           03  FILLER                      PIC X(10) VALUE SPACES.
       01  WS-RPT-GRAND-TOT-LN.
           03  FILLER                  PIC X(46) VALUE SPACES.
           03  FILLER                  PIC X(14) VALUE 'COMPANY TOTAL '.
           03  WS-RPT-GRAND-TOT        PIC ZZZ,ZZ9.99.
           03  FILLER                  PIC X(10) VALUE SPACES.

       01  WS-FLAGS.
           03  WS-EOF-FLAG                 PIC X       VALUE 'N'.
               88  EOF                                 VALUE 'Y'.
           03  WS-FIRST-FLAG               PIC X       VALUE 'Y'.
               88  FIRST-REC                           VALUE 'Y'.
           03 WS-SRT-SPACE-FLAG            PIC X       VALUE 'N'.
               88  SRT-EOF                             VALUE 'Y'.

       01  WS-TOTALS.
           03  WS-SV-DEPT                  PIC 99      VALUE ZERO.
           03  WS-DEPT-TOT                 PIC 9(6)V99 VALUE ZERO.
           03  WS-GRAND-TOT                PIC 9(6)V99 VALUE ZERO.
           03  WS-REC-CTR                  PIC 9999    VALUE ZERO.

       01  WS-RUN-DATE-TIME.
           03  WS-RUN-DATE.
               05  WS-RUN-MO               PIC 99.
               05  FILLER                  PIC X       VALUE '/'.
               05  WS-RUN-DAY              PIC 99.
               05  FILLER                  PIC X       VALUE '/'.
               05  WS-RUN-YR               PIC 9999.
           03  FILLER                      PIC XX      VALUE SPACES.
           03  WS-RUN-TIME.
               05  WS-RUN-HR               PIC 99.
               05  FILLER                  PIC X       VALUE ':'.
               05  WS-RUN-MIN              PIC 99.

       01  WS-DEPARTMENT-NAMES.
           03  WS-DEPT-NAME-LIST.
               05  FILLER                  PIC X(5)    VALUE 'PROD '.
               05  FILLER                  PIC X(5)    VALUE 'ACCT '.
               05  FILLER                  PIC X(5)    VALUE 'SALES'.
               05  FILLER                  PIC X(5)    VALUE 'MKTG '.
               05  FILLER                  PIC X(5)    VALUE 'MGMT '.
           03  WS-DEPT-NAME-TABLE  REDEFINES WS-DEPT-NAME-LIST.
               05  WS-DEPT-NAME            PIC X(5)    OCCURS 5 TIMES.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
           SORT SRT-SPACE ON ASCENDING KEY SRT-DEPT
                             ASCENDING KEY SRT-LNAME
                        USING EMP-MST
                OUTPUT PROCEDURE 200-PRT-RPT.
           STOP RUN.
      *-----------------------------------------------------------------
       200-PRT-RPT.
           OPEN OUTPUT PR-RPT.
           PERFORM 300-GET-SYS-DATE-TIME.
           PERFORM 400-RPT-HEADING.

           PERFORM UNTIL SRT-EOF
               RETURN SRT-SPACE
                     AT END
                         MOVE 'Y' TO WS-SRT-SPACE-FLAG
                         PERFORM 700-RPT-END

                     NOT AT END
                         ADD 1 TO WS-REC-CTR

                        IF FIRST-REC
                           MOVE EMP-DEPT TO WS-SV-DEPT
                           MOVE 'N'   TO WS-FIRST-FLAG
                        END-IF

                        IF SRT-DEPT NOT EQUAL TO WS-SV-DEPT
                            PERFORM 600-CHG-DEPTS
                        END-IF
                     PERFORM 501-WRITE-DTL
               END-RETURN
           END-PERFORM.

           DISPLAY 'PROJECT 2 SORT - MARIO GARCIA'.
           DISPLAY 'RECORDS PROCESSED: ', WS-REC-CTR.
           DISPLAY 'RUN   ', WS-RUN-DATE-TIME.
           CLOSE PR-RPT.

      *-----------------------------------------------------------------
       300-GET-SYS-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE      TO   WS-SYS-DATE.
           MOVE WS-SYS-MO                  TO   WS-RPT-MO
                                                WS-RUN-MO.
           MOVE WS-SYS-DAY                 TO   WS-RPT-DAY
                                                WS-RUN-DAY.
           MOVE WS-SYS-YR                  TO   WS-RPT-YR
                                                WS-RUN-YR.
           MOVE WS-SYS-HR                  TO   WS-RUN-HR.
           MOVE WS-SYS-MIN                 TO   WS-RUN-MIN.
      *-----------------------------------------------------------------
       400-RPT-HEADING.
           WRITE PR-RPT-REC                FROM WS-RPT-TITLE-LN.
           WRITE PR-RPT-REC                FROM WS-RPT-BLANK-LN.
           WRITE PR-RPT-REC                FROM WS-DTL-HDG.
           WRITE PR-RPT-REC                FROM WS-DTL-DASH.
      *-----------------------------------------------------------------
       501-WRITE-DTL.
           MOVE  SRT-DEPT                  TO   WS-DTL-DEPT-NUM.
           MOVE  SRT-DEPT                  TO   WS-SV-DEPT.
           MOVE  WS-DEPT-NAME (SRT-DEPT)   TO   WS-DTL-DEPT-NAME.
           MOVE  SRT-ID                    TO   WS-DTL-ID.
           MOVE  SRT-LNAME                 TO   WS-DTL-LNAME.
           MOVE  SRT-FNAME                 TO   WS-DTL-FNAME.
           MOVE  SRT-QTR-PAY               TO   WS-DTL-PAY
           WRITE PR-RPT-REC                FROM WS-DTL-LN.
           ADD   SRT-QTR-PAY               TO   WS-DEPT-TOT.
      *-----------------------------------------------------------------
       500-WRITE-DTL.
           MOVE  EMP-DEPT                  TO   WS-DTL-DEPT-NUM.
           MOVE  WS-DEPT-NAME (SRT-DEPT)   TO   WS-DTL-DEPT-NAME.
           MOVE  EMP-ID                    TO   WS-DTL-ID.
           MOVE  EMP-LNAME                 TO   WS-DTL-LNAME.
           MOVE  EMP-FNAME                 TO   WS-DTL-FNAME.
           MOVE  EMP-QTR-PAY               TO   WS-DTL-PAY
           WRITE PR-RPT-REC                FROM WS-DTL-LN.
           ADD   EMP-QTR-PAY               TO   WS-DEPT-TOT.
      *-----------------------------------------------------------------
       600-CHG-DEPTS.
           MOVE SRT-DEPT                   TO WS-SV-DEPT

           IF WS-REC-CTR > 1
                WRITE PR-RPT-REC           FROM WS-DTL-TOT-DASH
                MOVE  WS-DEPT-TOT          TO   WS-DTL-TOT
                WRITE PR-RPT-REC           FROM WS-DTL-TOT-LN
                WRITE PR-RPT-REC           FROM WS-RPT-BLANK-LN
                ADD   WS-DEPT-TOT          TO   WS-GRAND-TOT
                MOVE  ZERO                 TO   WS-DEPT-TOT
           END-IF.
      *-----------------------------------------------------------------
       700-RPT-END.
           PERFORM 600-CHG-DEPTS.
           WRITE   PR-RPT-REC              FROM SPACES.
           MOVE    WS-GRAND-TOT            TO   WS-RPT-GRAND-TOT.
           WRITE   PR-RPT-REC              FROM WS-RPT-GRAND-TOT-LN.
      *-----------------------------------------------------------------
