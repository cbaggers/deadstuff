;;;; spir-v.asd

(asdf:defsystem #:spir-v
  :description "Describe spir-v here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cl-html-parse
               #:fn_)
  :components ((:file "package")
               (:file "spir-v")))
