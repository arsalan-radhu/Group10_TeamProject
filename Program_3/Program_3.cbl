       identification division.
       program-id. Program_3.
       author. date-written.

       environment division.
       
       configuration section.
	   input-output section.

	   file-control.

        select sale-and-layaway-file
		  assign to "../../../../Data/s&l.dat"
       	  organization is line sequential.
          
	   select report-file
	   assign to "../../../../Data/salesreport.out"
			  	  organization is line sequential.

	   data division.
	   file section.
       
       fd report-file
       		  data record is report-line
	   	   		   record contains 80 characters.
                   
	   01 report-line              pic x(80).
       
        fd sale-and-layaway-file
	   		   data record is sale-and-layaway-line
	   		   record contains 36 characters.

       01 sale-and-layaway-line .
			05 il-transaction-code	       pic x.
				 88 il-sale-and-layaway-transac-88
				   value 'S', 'L'.
				 88 il-sale-transac-88
				   value 'S'.
				 88 il-layaway-transac-88
				   value 'L'.
				 88 il-return-transac-88
				   value 'R'.
			05 il-transaction-amount   pic 9(5)v99.
			05 il-payment-type		       pic xx.
				 88 il-payment-cash-88
				   value 'CA'.
				 88 il-payment-credit-88
				   value 'CR'.
				 88 il-payment-debit-88
				   value 'DB'.
			05 il-store-number		           pic xx.
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
			05 il-invoice-number pic x(9).
			05 il-sku-code			     pic x(15).

       working-storage section.
	   01 ws-boolean-const.
			05 ws-true-const		    pic x value "Y".
			05 ws-false-const		pic x value "N".
            
	   01 ws-eof-flag                    pic x value 'N'.
       
	   77 ws-one                           pic 9 value 1.
	   77 ws-zero                          pic 9 value 0.
	   77 ws-trans-percent	    pic 9v999 value 0.13.
	    77 ws-hundred                pic 999 value 100.
        
	   01 ws-calc.           	    		               
			05 ws-calc-tax			    pic 9999v99.

	   01 ws-totals.
			05 ws-total-records	   pic 99.
			05 ws-total-sandl		   pic 99.
			05 ws-sanal-amount	   pic 9(6)v99.
			05 ws-total-s			           pic 99.
			05 ws-s-amount			   pic 9(6)v99.
			05 ws-total-l			           pic 99.
			05 ws-l-amount			   pic 9(6)v99.
			05 ws-total-ca			       pic 99.
			05 ws-ca-percent		   pic 99v99.
			05 ws-total-cr			       pic 99.
			05 ws-cr-percent		       pic 99v99.
			05 ws-total-db			       pic 99.
			05 ws-db-percent		   pic 99v99.
			05 ws-total-tax-ow		   pic 9(6)v99.
			05 ws-store-01-total	   pic 9(6)v99.
			05 ws-store-02-total	   pic 9(6)v99.
			05 ws-store-03-total	   pic 9(6)v99.            
			05 ws-store-04-total	   pic 9(6)v99.
            05 ws-store-05-total	   pic 9(6)v99.
			05 ws-store-12-total	   pic 9(6)v99.

	   01 ws-page-count.
			05 ws-page-records		   pic 99 value 0.
			        88 ws-page-full			   value 20.
			05 ws-page-num			       pic 99 value 0.

	   01 ws-heading.                   
			05 filler				   pic x(30) value spaces.
			05 filler				   pic x(10) value 
                       "S&L REPORT".
			05 filler				   pic x(30) value spaces.

	   	01 ws-heading-2.
			05 filler				   pic x(65) value spaces.
			05 filler				   pic x(4) value 
                        "PAGE".
			05 filler				   pic x(4) value spaces.
			05 ws-current-pg-num
                                               pic Z9.
            
	   01 ws-column-heading-1.
			05 filler				   pic x(2) value spaces.
			05 filler                   pic x(6) value
									   "Trans.".
			05 filler				   pic x(3) value spaces.
			05 filler				   pic x(6) value "Trans.".
			05 filler				   pic x(4) value spaces.
			05 filler				   pic x(7) value 
                        "Payment".
			05 filler				   pic x value spaces.
			05 filler				   pic x(5) value 
                        "Store".
			05 filler				   pic x(4) value spaces.
			05 filler				   pic x(7) value "Invoice".
			05 filler				   pic x(2) value spaces.
			05 filler				   pic x(3) value "Sku".
			05 filler				   pic x(12) value spaces.
			05 filler				   pic x(3) value "Tax".
			05 filler				   pic x(5) value spaces.

	   01 ws-column-heading-2.
       		05 filler				   pic x(2) value spaces.
			05 filler				   pic x(4) value "Code".
			05 filler				   pic x(5) value spaces.
			05 filler				   pic x(6) value 
                        "Amount".
			05 filler				   pic x(4) value spaces.
			05 filler				   pic x(4) value "Type".
			05 filler				   pic x(4) value 
                        spaces.
			05 filler				   pic x(6) value "Number".
			05 filler				   pic x(3) value spaces.
			05 filler				   pic x(6) value 
                       "Number".
			05 filler				   pic x(3) value spaces.
			05 filler				   pic x(4) value 
                       "Code".
			05 filler				   pic x(11) value spaces.
			05 filler				   pic x(5) value "Owing".
			05 filler				   pic x(3) value 
                        spaces.

	   01 ws-detail-line.
       	   05 filler				                       pic x(2) 
            value
            spaces.
			05 ws-dl-trans-code		   pic x.
			05 filler				                   pic x(8) value 
            spaces.
			05 ws-dl-trans-amount	   pic Zz,zz9.99.
			05 filler				                   pic x(2) 
            value
            spaces.
			05 ws-dl-pay-type		       pic xx.
			05 filler				                   pic x(6) value 
            spaces.
			05 ws-dl-store-num		   pic xx.
			05 filler				                   pic x(7) value 
            spaces.
			05 ws-dl-invoice-num	   pic x(9).
			05 filler				                   pic x value 
            spaces.
			05 ws-dl-sku-code			   pic x(15).            
			05 filler				                   pic x value 
            spaces.
			05 ws-dl-tax-owing		   pic Z,zz9.99 value spaces.

	   01 ws-summary-line-1.
			05 filler				          pic x(17) value spaces.   
			05 filler				           pic x(12) value 
            "Total Number".
			05 filler				          pic x(9) value spaces.
			05 filler				          pic x(12) value 
            "Total Amount".
            
       01 ws-summary-line-2.
			05 filler				           pic x(12) value 
            "S&L RECORDS".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-sandl-total
											           pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-sandl-amount
											           pic Zzz,zz9.99.
                                               
	   01 ws-summary-line-3.
			05 filler				           pic x(12) value 
            "S    RECORDS".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-s-total
												       pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-s-amount
												       pic Zzz,zz9.99.  
                                                   
	    01 ws-summary-line-4.
			05 filler				           pic x(12) value 
            "L    RECORDS".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-l-total
													   pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-l-amount
													   pic Zzz,zz9.99.

	    01 ws-summary-line-5.
			05 filler				           pic x(17) value spaces.
			05 filler				           pic x(12) value 
            "Total Number".
			05 filler				           pic x(10) value spaces.
			05 filler				           pic x(10) value 
            "Percentage".
            
	   01 ws-summary-line-6.
			05 filler				           pic x(12) value "Cash".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-cash-total
												       pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-cash-percentage
												       pic z9.99.
			05 filler				           pic x value "%".
                                                      
	    01 ws-summary-line-7.
			05 filler				           pic x(12) value 
            "Credit  Card".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-credit-total
													   pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-credit-percentage
													   pic z9.99.
			05 filler				           pic x value "%".
            
	    01 ws-summary-line-8.
			05 filler				           pic x(12) value
               "Debit	Card".
			05 filler				           pic x(9) value spaces.
			05 ws-sm-debit-total
													   pic zz9.
			05 filler				           pic x(17) value spaces.
			05 ws-sm-debit-percentage
													   pic z9.99.
			05 filler				           pic x value "%".
      
       01 ws-summary-line-9.
			05 filler				           pic x(16) value 
            "Total Tax  Owing".
			05 filler				           pic x(5) value 
            spaces.
			05 ws-sm-total-tax	   pic Zzz,zz9.99.

	   01 ws-summary-line-10.
			05 filler				   pic x(39) value
       "Highest S&L Transaction Amount is Store".
			05 filler				   pic x value spaces.
			05 ws-sm-high-store		   pic Z9.
        
	    01 ws-summary-line-11.
			05 filler				   pic x(39) value
							  "Lowest  S&L Transaction Amount is Store".
			05 filler				   pic x value spaces.
			05 ws-sm-low-store		   pic Z9.
            
       procedure division.
	   000-main.
		   move ws-false-const to ws-eof-flag.

		   open input sale-and-layaway-file,
			 output report-file.
             
		   read sale-and-layaway-file
			   at end
				   move ws-true-const to ws-eof-flag.
		   perform 100-print-headers.
		   perform  200-process-record
			 until ws-eof-flag = ws-true-const.
		   perform 800-print-summary.
           goback.
	   100-print-headers.
		  write report-line from ws-heading.
		   perform 150-print-column-headers.
           
	   150-print-column-headers.
      * Increment page values
		   add ws-one to ws-page-num.
		   move ws-page-num to ws-current-pg-num.
           
      * Print Headings    
           write report-line from ws-heading-2 after advancing 
			 ws-one line.
		   write report-line from ws-column-heading-1 after advancing 
			    ws-one line.
           write report-line from ws-column-heading-2.

	   200-process-record.
       
		   perform 300-calculate-transaction-amount.
		   perform 400-process-payment-type.
		   perform 500-process-store.
		   perform 600-process-transaction-code.
		   perform 700-print-detail-line.
       
           if ws-page-full then
           	   perform 250-page-full
		   end-if
       
		   read sale-and-layaway-file
			   at end
					 move ws-true-const to ws-eof-flag.

	   250-page-full.
		   write report-line from spaces after advancing page.
		   perform 150-print-column-headers.
		   move ws-zero to ws-page-records.

	   300-calculate-transaction-amount.
		   compute ws-calc-tax = ws-trans-percent *
			 il-transaction-amount.
		   add ws-calc-tax to ws-total-tax-ow.

	   400-process-payment-type.
       
      * Determine the payment type
		   if il-payment-cash-88 then
			   perform 425-payment-ca
			   if il-payment-credit-88 then
				   perform 450-payment-cr
			   else
				   perform 475-payment-db
			   end-if
		   end-if.

	   425-payment-ca.
		   add ws-one to ws-total-ca.
	   450-payment-cr.
       	  add ws-one   to ws-total-cr.
	   475-payment-db.
		   add ws-one to ws-total-db.

	   500-process-store.
		   if il-store-01-88 then
				   add il-transaction-amount to ws-store-01-total
				   if il-store-02-88 then
						   add il-transaction-amount to
							 ws-store-02-total
						   if il-store-03-88 then
								   add il-transaction-amount to
									 ws-store-03-total
								   if il-store-04-88 then
										   add il-transaction-amount to
											 ws-store-04-total
										   if il-store-05-88 then
												   add
												   il-transaction-amount
													 to
													 ws-store-05-total
												   if il-store-12-88
													 then
														 add
												   il-transaction-amount
														   to
													   ws-store-12-total
												   end-if
										   end-if
								   end-if

						   end-if
				   end-if.

 
       600-process-transaction-code. 
       
      * Update code totals and total amounts
		   if il-sale-and-layaway-transac-88 then
				   add ws-one to ws-total-sandl
                   add il-transaction-amount  to ws-sanal-amount
		   end-if.

		   if il-sale-transac-88 then
               add ws-one to ws-total-s
			   add il-transaction-amount to ws-s-amount
				   if il-layaway-transac-88 then
					    add ws-one to ws-total-l
						add il-transaction-amount to ws-l-amount
				   end-if
		   end-if.


	   700-print-detail-line.
       
      * Move the neccessary values to detail line
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

	  * Update page values
		   add ws-one
             to ws-page-records.
		   add ws-one
             to ws-total-records.

      * Print detail line
		   write report-line from ws-detail-line.
           
           
       800-print-summary.

       end program Program_3.