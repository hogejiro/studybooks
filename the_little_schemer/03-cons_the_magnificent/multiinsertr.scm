(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module conses)
(print (multiinsertR 'fried 'fish '(chips and fish or fish and fried)))
; (chips and fish fried or fish fried and fried)
