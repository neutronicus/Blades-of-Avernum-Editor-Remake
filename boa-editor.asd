(in-package :asdf)

(defsystem :boa-editor
  :depends-on
  (:cl-ppcre
   :cl-lex
   :yacc
   :binary-data
   :id3v2)
  :serial t
  :components
  ((:file "utilities")
   (:file "boa_file_io")
   (:file "boa-data-structures")))