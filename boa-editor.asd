(in-package :asdf)

(defsystem :boa-editor
  :depends-on
  (:cl-ppcre
   :cl-lex
   :yacc
   :binary-data
   :id3v2
   :closer-mop)
  :serial t
  :components
  ((:file "utilities")
   (:file "parse-header-file")
   (:file "parse-bas-format")
   (:file "scendata-classes")
   (:file "parse-scendata")
   (:file "boa-data-structures")))