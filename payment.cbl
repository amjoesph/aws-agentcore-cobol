       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT-RULE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TYPE          PIC X.
       01 WS-AMOUNT-IN     PIC S9(7)V99.
       01 WS-AMOUNT        PIC S9(7)V99 COMP-3.
       01 WS-FEE           PIC S9(7)V99 COMP-3.
       01 WS-NET           PIC S9(7)V99 COMP-3.
       01 WS-STATUS        PIC X(7).

      * DISPLAY FORMATTED OUTPUT FIELDS
       01 DISP-FEE         PIC -(6)9.99.
       01 DISP-NET         PIC -(6)9.99.

       PROCEDURE DIVISION.
           ACCEPT WS-TYPE
           ACCEPT WS-AMOUNT-IN
           MOVE WS-AMOUNT-IN TO WS-AMOUNT

           MOVE ZERO TO WS-FEE WS-NET
           MOVE "OK" TO WS-STATUS

           EVALUATE TRUE
             WHEN WS-AMOUNT < ZERO
               MOVE "INVALID" TO WS-STATUS
             WHEN WS-TYPE = "P"
               COMPUTE WS-FEE ROUNDED = WS-AMOUNT * 0.015
               COMPUTE WS-NET = WS-AMOUNT - WS-FEE
             WHEN WS-TYPE = "R"
               COMPUTE WS-NET = ZERO - WS-AMOUNT
             WHEN OTHER
               MOVE "INVALID" TO WS-STATUS
           END-EVALUATE

           MOVE WS-FEE TO DISP-FEE
           MOVE WS-NET TO DISP-NET

           DISPLAY "STATUS=" FUNCTION TRIM(WS-STATUS)
           DISPLAY "FEE=" FUNCTION TRIM(DISP-FEE)
           DISPLAY "NET=" FUNCTION TRIM(DISP-NET)
           GOBACK.
