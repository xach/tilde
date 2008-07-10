;;;; tilde.asd

(asdf:defsystem #:tilde
  :depends-on (#:sb-posix)
  :components ((:file "tilde")))

(defmethod perform :after ((o load-op) (c (eql (find-system "tilde"))))
  (funcall (find-symbol "INSTALL-TILDE-EXPANDER" :tilde)))
