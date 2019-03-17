(in-package :om)


(defclass ReactiveRecieveBox (OMReactiveBox ReceiveBox) ())

(defclass ReactiveReceiveBoxFrame (ReactiveBoxFrame ReceiveBoxFrame) ())
(defmethod get-frame-class ((self ReactiveRecieveBox)) 'ReactiveReceiveBoxFrame)


(defmethod omNG-box-value ((self ReactiveRecieveBox) &optional (numout 0))
  (let ((val (current-box-value self numout)))
    ;(when (push-tag self) ;;; FOR MAXIMAL REACTIVITY (PUSH EVEN WHEN DEMAN_DIREVEN EVALUATED), REMOVE THIS TEST
    ;  (print (list (name self) "PUSH"))
    ;  (mapcar #'(lambda (box) (unless (or (state-lock box) (gen-flag box)) (omr-notify box)))
    ;          (remove-if-not 'active (listeners self))))
    val)
  )

(defmethod set-delivered-value ((box ReactiveRecieveBox) msg)
  (setf (value box) (list msg))
  (self-notify box nil))



(defmethod get-boxcallclass-fun ((self (eql 'osc-receive))) 'ReactiveRecieveBox)
(defmethod get-boxcallclass-fun ((self (eql 'midi-in))) 'ReactiveRecieveBox)


;;;=====================================
;;; ROUTE OSC ("adress" data)
;;;=====================================

(defun address-match (address route-path) 
    (string-equal address route-path))

(defmethod! route-osc ((message list) &rest osc-paths)
  :numouts 1
  :icon '(611)
  (values-list (copy-list (cons message 
                                (mapcar 
                                 #'(lambda (route-path) (if (listp (car message))
                                                            ;;; we have several messages here...
                                                            (let ((rep nil))
                                                              (loop for msg in message while (null rep) do
                                                                    (when (address-match (car msg) route-path) (setf rep (cdr msg))))
                                                              rep)
                                                         (when (address-match (car message) route-path) (cdr message))
                                                       ))
                                 osc-paths)))))

(defmethod get-boxcallclass-fun ((self (eql 'route-osc))) 'ReactiveRouteBox)

