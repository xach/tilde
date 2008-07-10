;;;; tilde.asd

(asdf:defsystem #:tilde
  :depends-on (#:sb-posix)
  :components ((:file "tilde")))
