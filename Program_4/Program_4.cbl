       identification division.
       program-id. Program_4.
       author. Sanjivkumar Patel.
       date-written. April 20th 2022.



       environment division.
       configuration section.
       input-output section.
       file-control.
       select returns
              assign to "../../../../Data/returns.dat"
              organization is line sequential.
              
       select report-file
       	 assign to "../../../../Data/report-file.out"
       			  	  organization is line sequential.
       data division.
       file section.
              
       fd returns
              data record is returns-data
              record contains 80 characters.
      *
       
       
      *
       01 returns-data.
         05 il-transaction-code        pic x.
           88 il-sale-and-layaway-transac-88
                                       value 'S', 'L'.
           88 il-sale-transac-88
                                       value 'S'.
           88 il-layaway-transac-88
                                       value 'L'.
           88 il-return-transac-88
                                       value 'R'.
         05 il-transaction-amount      pic 9(5)v99.
         05 il-payment-type pic xx.
           88 il-payment-cash-88
                       value 'CA'.
           88 il-payment-credit-88
                       value 'CR'.
           88 il-payment-debit-88
                       value 'DB'.
         05 il-store-number pic xx.
           88 il-store-01-88
                       value '01'.
           88 il-store-02-88
                       value '02'.
           88 il-store-03-88
                       value '03'.
           88 il-store-04-88
                       value '04'.
           88 il-store-05-88
                       value '05'.
           88 il-store-12-88
                       value '12'.
         05 in-payment-type            pic x(2).
         05 il-invoice-number          pic x(9).
         05 il-sku-code                pic x(15).       
      *
       fd report-file
              data record is report-line.
	   01 report-line pic x(80).      
      *
       working-storage section.
       
       01 ws-page-count.
         05 ws-page-records            pic 99 value 0.
           88 ws-page-full             value 20.
         05 ws-page-num                pic 99 value 0.
         05 ws-total-records           pic 99.
       01 ws-boolean-const.
         05 ws-true-const              pic x value "Y".
         05 ws-false-const             pic x value "N".

       01 ws-eof-flag                  pic x value 'N'.
       77 ws-one                       pic 9 value 1.
       77 ws-zero                      pic 9 value 0.
       77 ws-trans-percent	           pic 9v999 value 0.13.
       77 ws-hundred                   pic 999 value 100.
       
       01 ws-calc.
         05 ws-calc-tax                pic 9999v99.
      *

       01 ws-report-title-line.
         05 filler                     pic x(25)
           value                       " RETURN REPORT".
         05 filler                     pic x(10) value spaces.
         05 filler                     pic x(4) value "PAGE".
         05 ws-current-pg-num         pic zz9.

      *
       01 ws-page-heading.
         05 filler                     pic x(10) value "Trans.".
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(6) value "Trans.".
         05 filler                     pic x(4) value spaces.
		 05 filler				       pic x(7)
           value                       "Payment".
	     05 filler				       pic x value spaces.
         05 filler                     pic x(5) value "Store".
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(9) value "Invoice".
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(3) value "Sku".
         05 filler                     pic x(12) value spaces.
         05 filler                     pic x(3) value "Tax".
         05 filler                     pic x(5) value spaces.

	   01 ws-column-heading-2.
         05 filler                     pic x(1) value spaces.
		 05 filler				       pic x(4) value "Code".
         05 filler                     pic x(8) value spaces.
		 05 filler				       pic x(6) value"Amount".
         05 filler                     pic x(4) value spaces.
		 05 filler				       pic x(4) value "Type".
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(6) value "Number".
		 05 filler				       pic x(3) value spaces.
         05 filler                     pic x(6) value "Number".
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(4) value "Code".
         05 filler                     pic x(11) value spaces.
         05 filler                     pic x(5) value "Owing".
         05 filler                     pic x(3) value spaces.
      *
       01 ws-detail-line.
         05 ws-dl-trans-code           pic x.
         05 filler                     pic x(8) value spaces.
         05 ws-dl-trans-amount         pic Zz,zz9.99.
         05 filler                     pic x(2) value spaces.
         05 ws-dl-pay-type             pic xx.
         05 filler                     pic x(6) value spaces.
         05 ws-dl-store-num            pic xx.
         05 filler                     pic x(5) value spaces.
         05 ws-dl-invoice-num          pic x(9).
         05 filler                     pic x value spaces.
         05 ws-dl-sku-code             pic x(15).
         05 filler                     pic x value spaces.
         05 ws-dl-tax-owing            pic Z,zz9.99 value spaces.

       01 ws-summary-returns-for-store.

         05 ws-summary-returns-for-store1  pic x(5).
         05 filler                         pic x(3) value spaces.
         05 ws-summary-returns-for-store2  pic x(5).
         05 filler                         pic x(3) value spaces.
         05 ws-summary-returns-for-store3  pic x(5).
         05 filler                         pic x(3) value spaces.
         05 ws-summary-returns-for-store4  pic x(5).
         05 filler                         pic x(3) value spaces.
         05 ws-summary-returns-for-store5  pic x(5).
         05 filler                         pic x(3) value spaces.
         05 ws-summary-returns-for-store6  pic x(5).
         05 filler                         pic x(3) value spaces.
                

       01 ws-summary-return-line.
         05 filler                     pic x(35)
           value                       " TOTAL RETURNS: ".
         05 ws-ret-line-total          pic z(7)9.
         05 filler                     pic x(15) value " TOTAL AMOUNTS".
         05 ws-ret-line-amount         pic $z(10)9.99.
      *
       01 ws-summary-total-tax-owned.
         05 filler                      pic x(25)
           value                       "TOTAL TAX OWNED       ".
         05 ws-sum-tot-t-amount        pic $z(11)9.99.

      *
       
       77 ws-line-count                  pic 99 value 0.
       77 ws-lines-per-page          pic 99 value 20.
									   
      *       
       01 ws-calculations.
         05 ws-tax-owned pic 9(5)v99.

       procedure division.
      *
           move ws-false-const to ws-eof-flag.

           open input returns,
             output report-file.
       
		   read  returns
               at end
                   move ws-true-const to ws-eof-flag.
           perform 100-print-headers.

		   perform  200-process-record until ws-eof-flag = 
           ws-true-const.

         perform 800-print-summary.
           close returns,
             report-file.

           goback.

       
       100-print-headers.
           perform 150-print-column-headers.
      *
       150-print-column-headers.
      *Increment page values
           add ws-one to ws-page-num.
           move ws-page-num to ws-current-pg-num.
      *Print Headings
           write report-line from ws-report-title-line after advancing
             ws-one line.
           write report-line from ws-page-heading
             after advancing ws-one line.
           write report-line from ws-column-heading-2.

       200-process-record.

           perform 300-calculate-transaction-amount.
           perform 600-process-transaction-code.
           perform 700-print-detail-line.

           if ws-page-full then
               perform 250-page-full
           end-if

           read returns
               at end
                   move ws-true-const to ws-eof-flag.

       250-page-full.
           write report-line from spaces after advancing page.
           perform 150-print-column-headers.
           move ws-zero to ws-page-records.
	   300-calculate-transaction-amount.
		   compute ws-calc-tax rounded = ws-trans-percent *
			 il-transaction-amount.
		   add ws-calc-tax to  ws-tax-owned.

	   600-process-transaction-code.

       700-print-detail-line.

      *    Move the neccessary values to detail line
           move il-transaction-code
             to ws-dl-trans-code.
           move il-invoice-number
             to ws-dl-invoice-num.
           move il-transaction-amount
             to ws-dl-trans-amount.
           move il-store-number
             to ws-dl-store-num.
           move il-sku-code
             to ws-dl-sku-code.
           move il-payment-type
             to ws-dl-pay-type.
           move ws-calc-tax
             to ws-dl-tax-owing.
      *Print detail line
           write report-line from ws-detail-line.

       
      * Update page values
           add ws-one
             to ws-page-records.
           add ws-one
             to ws-total-records.
             
      
	   800-print-summary.

           write report-line from spaces.

           write report-line from ws-summary-return-line.
           write report-line from spaces.
           write report-line from ws-summary-total-tax-owned.
           
      *    close report-file, returns
           


       end program Program_4.