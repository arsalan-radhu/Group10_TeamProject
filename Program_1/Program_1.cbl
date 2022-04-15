       identification division.
       program-id. Program_1.
       author. Arsalan Arif Radhu.
       date-written. 15 April 2022.

       environment division.
       configuration section.
       input-output section.

       file-control.

           select input-file
           assign to "../../../../Data/project6.dat"
           organization is line sequential.

      *    Valid records go here.
           select valid-file
           assign to "../../../../Data/valid.dat"
           organization is line sequential.

      *    Invalid records go here.
           select invalid-file
           assign to "../../../../Data/invalid.dat"
           organization is line sequential.

      *    Invalid records to be formatted as a report go here.
           select invalid-report
           assign to "../../../../Data/invalid.out"
           organization is line sequential.

       data division.
       file section.

       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
         05 il-transaction-code        pic x.
           88 il-valid-transac-codes-88
                   value 'S', 'R', 'L'.
         05 il-transaction-amount      pic 9(5)v99.
         05 il-payment-type            pic xx.
           88 il-valid-pay-types-88
                   value 'CA', 'CR', 'DB'.
         05 il-store-number            pic xx.
           88 il-valid-store-nums-88
                   value '01', '02', '03', '04', '05', '12'.
         05 il-invoice-number          pic x(9).
         05 il-invoice-number-r1 redefines il-invoice-number.
           10 il-invoice-number-XX     pic x(2).
           10 il-invoice-number-dash   pic x(1).
             88 il-invoice-number-dash-88
                       value '-'.
           10 il-invoice-number-000000 pic 9(6).
         05 il-invoice-number-r4 redefines il-invoice-number.
           10 il-invoice-number-letter pic x(1).
             88 il-invoice-number-1-letter-88
                       value 'A', 'B', 'C', 'D', 'E'.
           10 il-invoice-number-letter pic x(1).
             88 il-invoice-number-2-letter-88
                       value 'A', 'B', 'C', 'D', 'E'.
           10 filler                   pic x(7).
         05 il-invoice-number-r3 redefines il-invoice-number.
           10 il-invoice-number-letter1
                                       pic x(2).
             88 il-invoice-number-duplicate-88
                       value 'AA', 'BB', 'CC', 'DD', 'EE'.
           10 filler                   pic x(1).
           10 il-invoice-number-num    pic 9(6).
             88 il-invoice-number-invalid-range-88
                       value 0 thru 99999, 900001 thru 999999.
         05 il-sku-code                pic x(15).
           88 il-sku-code-blank-88
                   value spaces.
           88 il-sku-code-valid-88
                   value 'A' thru 'Z', '1' thru '9'.

       fd valid-file
           data record is valid-line
           record contains 36 characters.

       01 valid-line                   pic x(36).

       fd invalid-file
           data record is valid-line
           record contains 36 characters.

       01 invalid-line                 pic x(36).

       fd invalid-report
           data record is invalid-report-line
           record contains 61 characters.

       01 invalid-report-line          pic x(61).

       working-storage section.

      *Used to determine eof (end-of-file).
       01 ws-eof-flag                  pic x value 'N'.

       01 ws-detail-flag               pic 9 value 0.

      *Used for eof flag.
       01 ws-boolean-const.
         05 ws-true-const              pic x value "Y".
         05 ws-false-const             pic x value "N".

      *This header will display the author(s) of this program and the
      *title for this team project.
       01 ws-heading1-name-line.
         05 filler                     pic x(7) value "Author:".
         05 filler                     pic x(18) value 
           "Arsalan Arif Radhu".
         05 filler                     pic x(1) value spaces.
         05 filler                     pic x(35) value
           "MAFD4202 - Team Project: Program #1".

      *Name of the report where all invalid records with error
      *messages.
       01 ws-heading2-report-header-line.
         05 filler                     pic x(11) value spaces.
         05 filler                     pic x(12) value "ERROR REPORT".
         05 filler                     pic x(38) value spaces.

      *Header for the ERRORS report in the invalid.out file.
       01 ws-heading4-invalid-report-line-1.
         05 filler                     pic x(10) value "Record No.".
         05 filler                     pic x(15) value spaces.
         05 filler                     pic x(14) value "Raw Input Data".
         05 filler                     pic x(22) value spaces.
   

       01 ws-heading4-invalid-report-line-2.
         05 filler                     pic x(23) value spaces.
         05 filler                     pic x(20) value 
           "and ERROR MESSAGE(S)".
         05 filler                     pic x(18) value spaces.

      *Header to separate the previous header and both the
      *raw input data and error messages.
       01 ws-heading4-invalid-report-line-3.
         05 filler                     pic x(11) value "----------".
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(36) value
           "------------------------------------".
         05 filler                     pic x(12) value spaces.

       01 ws-spaces-line               pic x(101).

       01 ws-detail-line.
         05 filler                     pic x(2) value spaces.
         05 ws-dl-report-number        pic zz9.
         05 filler                     pic x(9) value spaces.
         05 ws-dl-input-line           pic x(36) value spaces.
         05 filler                     pic x(11) value spaces.

       01 ws-error-message-line.
         05 filler                     pic x(14) value spaces.
         05 ws-error-text              pic x(47) value spaces.

       01 ws-heading5-summary-header.
         05 filler                     pic x(7) value "SUMMARY".
         05 filler                     pic x(54) value spaces.

       01 ws-heading5-summary-line-1.
         05 filler                     pic x(14) value "TOTAL RECORDS:".
         05 filler                     pic x(3) value spaces.
         05 ws-tl-total-records        pic zz9.
         05 filler                     pic x(41) value spaces.

       01 ws-heading5-summary-line-2.
         05 filler                     pic x(14) value "VALID RECORDS:".
         05 filler                     pic x(3) value spaces.
         05 ws-tl-valid-records        pic zz9.
         05 filler                     pic x(41) value spaces.

       01 ws-heading5-summary-line-3.
         05 filler                     pic x(16) value 
           "INVALID RECORDS:".
         05 filler                     pic x value spaces.
         05 ws-tl-invalid-records      pic zz9.
         05 filler                     pic x(42) value spaces.

      *Error messages that will define the type of error(s)
      *encountered in a record.
       01 ws-error-text-constants.
         05 ws-error-text-1-const      pic x(47) value
           "Transaction Code must be 'S', 'R' or 'L'.      ".
         05 ws-error-text-2-const      pic x(47) value
           "Transaction Amount must be numeric.            ".
         05 ws-error-text-3-const      pic x(47) value
           "Payment Type must be 'CA', 'CR', or 'DB'.      ".
         05 ws-error-text-4-const      pic x(47) value
           "Store Number must be 01, 02, 03, 04, 05, or 12.".
         05 ws-error-text-5-const-1    pic x(47) value
           "Invoice Number must be in format XX-000000.    ".
         05 ws-error-text-5-const-2    pic x(47) value
           "Invoice Number XX can only be A, B, C, D, or E.".
         05 ws-error-text-5-const-3    pic x(47) value
           "Invoice Number XX cannot have two same letters.".
         05 ws-error-text-5-const-4    pic x(47) value
           "Invoice Number must be >100000 and <900000.    ".
         05 ws-error-text-5-const-5    pic x(47) value
           "All records should have a dash in position 3.  ".
         05 ws-error-text-7-const-1    pic x(47) value
           "SKU Code cannot be empty.                      ".
         05 ws-error-text-7-const-2    pic x(47) value
           "SKU Code should be alphanumeric.               ".

      *Various counters to keep track of certain variables.
       01 ws-counters.
         05 ws-line-count              pic 99 value 0.
         05 ws-record-number-count     pic 999 value 0.
         05 ws-input-record-count      pic 999 value 0.
         05 ws-valid-record-count      pic 999 value 0.
         05 ws-invalid-record-count    pic 999 value 0.
         05 ws-num-of-errors-count     pic 99 value 0.
         05 ws-total-invalid-records   pic 99 value 0.
         05 ws-total-valid-records     pic 99 value 0.

       

       end program Program_1.