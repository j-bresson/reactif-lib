;;;===================================================
;;; OM-reactive
;;; Author: Jean Bresson (IRCAM - 2012)
;;;===================================================


(in-package :om)

(compile&load (namestring (om-relative-path '("src") "reactive-boxes")))
(compile&load (namestring (om-relative-path '("src") "route")))
(compile&load (namestring (om-relative-path '("src") "collect")))
(compile&load (namestring (om-relative-path '("src") "receive")))

(pushnew :om-reactive *features*)

(om::set-lib-release 0.4)

;;; Use this to set the delay in box evaluation/notification
(defparameter *defcolortime* nil)

(print "
;;;===========================================================================
;;; OM-reactive
;;; prototype library for reactivity in OM
;;; Jean Bresson 2013
;;;===========================================================================
")
