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
              data record is return-data
              record contains 80 characters.
      *
       01 report-line pic x(80).
       
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
         05 in-pay-type                pic x(2).
         05 il-invoice-number          pic x(9).
         05 il-sku-code                pic x(15).       
      *
       fd report-file
              data record is report-print-line.
       01 report-print-line pic x(80).
       
      *
       working-storage section.
       
       
       01 ws-boolean-const.
         05 ws-true-const              pic x value "Y".
         05 ws-false-const             pic x value "N".

       01 ws-eof-flag                  pic x value 'N'.
       77 ws-one                       pic 9 value 1.
       77 ws-zero                      pic 9 value 0.
       77 ws-trans-percent	           pic 9v999 value 0.13.
        77 ws-hundred                  pic 999 value 100.

       77 ws-trans-percent	           pic 9v999 value 0.13.

       01 ws-calc.
         05 ws-calc-tax                pic 9999v99.
      *

       01 ws-report-title-line.
         05 filler                     pic x(25)
           value                       " RETURN REPORT".
         05 filler pic x(10) value spaces.
         05 filler pic x(4) value "PAGE".
         05 ws-title-page-name pic zz9.

      *
       01 ws-page-heading.
         05 filler                     pic x(10) value "Trans.".
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(6) value "Trans.".
         05 filler                     pic x(4) value spaces.
		 05 filler				       pic x(7)
           value                       "Payment".
	     05 filler				       pic x value spaces.
		 05 filler				       pic x(5) value  "Store".
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(7) value "Invoice".
         05 filler                     pic x(2) value spaces.
         05 filler                     pic x(3) value "Sku".
         05 filler                     pic x(12) value spaces.
         05 filler                     pic x(3) value "Tax".
         05 filler                     pic x(5) value spaces.

	   01 ws-column-heading-2.
         05 filler                     pic x(2) value spaces.
		 05 filler				       pic x(4) value "Code".
		 05 filler				       pic x(5) value spaces.
		 05 filler				       pic x(6) value"Amount".
         05 filler                     pic x(4) value spaces.
		 05 filler				       pic x(4) value "Type".
         05 filler                     pic x(4) value spaces.
         05 filler                     pic x(6) value "Number".
		 05 filler				       pic x(3) value spaces.
		 05 filler				       pic x(6) value "Number".
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(4) value "Code".
         05 filler                     pic x(11) value spaces.
         05 filler                     pic x(5) value "Owing".
		 05 filler				       pic x(3) value spaces.
      *
       01 ws-detail-line.
         05 filler                     pic x(2) value spaces.
         05 ws-dl-trans-code           pic x.
         05 filler                     pic x(8) value spaces.
         05 ws-dl-trans-amount         pic Zz,zz9.99.
         05 filler                     pic x(2) value spaces.
         05 ws-dl-pay-type             pic xx.
         05 filler                     pic x(6) value spaces.
         05 ws-dl-store-num            pic xx.
         05 filler                     pic x(7) value spaces.
         05 ws-dl-invoice-num          pic x(9).
         05 filler                     pic x value spaces.
         05 ws-dl-sku-code             pic x(15).
         05 filler                     pic x value spaces.
         05 ws-dl-tax-owing            pic Z,zz9.99 value spaces.

       01 ws-summary-returns-for-store.

         05 ws-summary-returns-for-store1.
         05 filler pic x(3) value spaces.
         05 ws-summary-returns-for-store2
         05 filler pic x(3) value spaces.
         05 ws-summary-returns-for-store3
         05 filler pic x(3) value spaces.
         05 ws-summary-returns-for-store4
         05 filler pic x(3) value spaces.
         05 ws-summary-returns-for-store5
         05 filler pic x(3) value spaces.
         05 ws-summary-returns-for-store6
         05 filler pic x(3) value spaces.
                

       01 ws-summary-return-line.
         05 filler                     pic x(35)
           value                       " TOTAL RETURNS: ".
         05 ws-ret-line-total          pic z(7)9.
         05 filler                     pic x(15) value " TOTAL AMOUNTS".
         05 ws-ret-line-amount         pic $z(10)9.99.
      *
       01 ws-summary-total-tax-owned.
         05 fille                      pic x(25)
           value                       "TOTAL TAX OWNED       ".
         05 ws-sum-tot-t-amount        pic $z(11)9.99.

      *
       77 ws-current-page              pic 99 value 1.
       77 ws-line-count                pic 99 value 0.
       77 ws-lines-per-page            pic 99 value 20.
      
     
      *       
       01 ws-calculations.
         05 ws-tax-owned pic 9(5)v99.
         
         
         
              
       procedure division.

           goback.

       end program Program_4.