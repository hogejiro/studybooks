(add-load-path ".." :relative)
(load "bootstrap.scm")
(select-module bangs)
(print ((Y biz) 5))
; 0
; (print ((Y! biz) 5))
; no answer (infinite loop)
