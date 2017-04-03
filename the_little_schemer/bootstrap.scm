(use file.util)
(add-load-path "lib" :relative)
(map load (filter (lambda (n) (not (file-is-directory? n))) (directory-list "lib")))
