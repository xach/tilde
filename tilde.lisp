;;;; tilde.lisp
;;;;
;;;; To use, add this to your ~/.sbclrc:
;;;;
;;;;  (load "/path/to/tilde.lisp")
;;;;  (tilde:install-tilde-expander)
;;;;
;;;; After that, (equal (probe-file "~/") (user-homedir-pathname)) =>
;;;; T, for example.
;;;;

(require 'sb-posix)

(defpackage #:tilde
  (:use #:cl)
  (:export #:expand-tilde-namestring
           #:install-tilde-expander
           #:uninstall-tilde-expander
           #:no-such-user))

(in-package #:tilde)

(defparameter *desktop-directory-namestring* "Desktop/")

(proclaim '(ftype (function (t) t) username))

(define-condition no-such-user (error)
  ((username
    :initarg :username
    :reader username))
  (:report (lambda (condition stream)
             (format stream "No such user ~S" (username condition)))))

(defun posix-passwd (username-designator)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (etypecase username-designator
    (null
     (sb-posix:getpwuid (sb-posix:getuid)))
    (string
     (sb-posix:getpwnam username-designator))))

(defun posix-home-directory (username)
  (let ((passwd (posix-passwd username)))
    (if passwd
        (concatenate 'string (sb-posix:passwd-dir passwd) "/")
        (error 'no-such-user :username username))))

(defun home-directory-namestring (username)
  (posix-home-directory username))

(defun desktop-directory-namestring (username)
  (concatenate 'string (home-directory-namestring username)
               *desktop-directory-namestring*))

(defun tilde-namestring-parts (string &key (start 0) (end (length string)))
  "Return multiple values: username, suffix, desktopp."
  (assert (char= (char string start) #\~))
  (let ((pos (1+ start))
        (suffix "")
        username-start
        suffix-start
        username
        desktopp
        state)
    (labels ((finish ()
               (values username suffix desktopp))
             (maybe-desktop (char)
               (case char
                 (#\~
                  (setf desktopp t)
                  #'maybe-username)
                 (:end
                  (finish))
                 (t
                  (maybe-username char))))
             (maybe-username (char)
               (case char
                 (#\/
                  (setf suffix-start pos)
                  #'more-suffix)
                 (:end
                  (finish))
                 (t
                  (setf username-start pos)
                  #'more-username)))
             (more-suffix (char)
               (case char
                 (:end
                  (setf suffix (subseq string (1+ suffix-start) pos))
                  (finish))
                 (t
                  #'more-suffix)))
             (more-username (char)
               (case char
                 (#\/
                  (setf suffix-start pos)
                  (setf username (subseq string username-start pos))
                  #'more-suffix)
                 (:end
                  (setf username (subseq string username-start pos))
                  (finish))
                 (t
                  #'more-username))))
      (setf state #'maybe-desktop)
      (loop
       (when (= pos end)
         (return (funcall state :end)))
       (setf state (funcall state (char string pos)))
       (incf pos)))))
               

(defun expand-tilde-namestring (string &key (start 0) end)
  "If STRING starts with a tilde \(~), returns an expanded
  namestring. Namestrings are expanded like so:

    ~              expands to the home directory for the current user
    ~<username>    expands to the home directory for <username>
    ~~             expands to the desktop directory for the current user
    ~~<username>   expands to the desktop directory for <username>

  If a username is not found, an error of type NO-SUCH-USER is raised."
  (setf end (or end (length string)))
  (if (char= (char string start) #\~)
      (multiple-value-bind (username suffix desktopp)
          (tilde-namestring-parts string :start start :end end)
        (let ((base (if desktopp
                        (desktop-directory-namestring username)
                        (home-directory-namestring username))))
          (concatenate 'string base suffix)))
      string))

(defun error-not-installed (&rest rest)
  (declare (ignore rest))
  (error "Not installed"))

(defvar *old-parse-namestring* 'error-not-installed)

(locally
    (declare (sb-ext:muffle-conditions style-warning))
  (defun new-parse-namestring (thing 
                               &optional
                               host
                               (defaults *default-pathname-defaults*)
                               &key (start 0) end junk-allowed)
    (when (stringp thing)
      (setf thing (expand-tilde-namestring thing)))
    (funcall *old-parse-namestring* thing host defaults
             :start start
             :end end
             :junk-allowed junk-allowed)))

(defun install-tilde-expander ()
  (when (eql *old-parse-namestring* 'error-not-installed)
    (setf *old-parse-namestring* #'sb-impl::parse-namestring)
    (sb-ext:without-package-locks
      (setf (fdefinition 'sb-impl::parse-namestring) #'new-parse-namestring))
    t))

(defun uninstall-tilde-expander ()
  (unless (eql *old-parse-namestring* 'error-not-installed)
    (sb-ext:without-package-locks
      (setf (fdefinition 'sb-impl::parse-namestring) *old-parse-namestring*))
    t))
