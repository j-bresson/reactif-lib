(in-package :om)


;;;=====================================
;;; ROUTE
;;;=====================================

(defclass ReactiveRouteBox (OMReactiveBox OMBoxcall) 
  ((numouts :initform 1 :accessor numouts)))

(defmethod do-delete-one-input-extra ((self ReactiveRouteBox))
   (setf (numouts self) (- (numouts self) 1)) t)

(defmethod do-add-one-input-extra ((self ReactiveRouteBox))
   (setf (numouts self) (+ (numouts self) 1)) t)

(defmethod omNG-copy ((self ReactiveRouteBox))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))

(defmethod omNG-save ((self ReactiveRouteBox) &optional (values? nil))
  `(let* ((routebox ,(call-next-method)))
     (setf (numouts routebox) ,(numouts self))
     routebox))

;;; NOTIFY ONLY THE ROUTED OUTPUT
(defmethod OMR-Notify ((self ReactiveRouteBox))
  ;(print (list "NOTIFIED BOX" (name self)))
  (box-color self *notify-color*)
  (unless (push-tag self)
    (setf (push-tag self) t)
    (let ((listeners (remove-if-not 'active (listeners self))))
      (omNG-box-value self)
      (setf (state-lock self) t)
      (if (and (active self) listeners)
          (let ;((routed-output (position-if-not 'null (value self) :from-end t)))
              ((routed-outputs (loop for i = 0 then (+ i 1) 
                                     for v in (value self) 
                                     when v collect i)
                               ))
            (setf listeners (loop for lst in listeners
                                  when (remove nil 
                                                (mapcar #'(lambda (in) 
                                                            (and (connected? in) 
                                                                 (equal (first (connected? in)) self)
                                                                 (find (second (connected? in)) routed-outputs)))
                                                        (inputs lst)))
                                   collect lst))
            (mapcar 'omr-notify listeners))
        ;(omNG-box-value self)
        )
      (setf (state-lock self) nil)
      ))
  (box-color self *inactive-color*))



;;;=====================================
;;; ROUTE GENERAL

(defun test-match (data test) 
  (if (functionp test) 
      (funcall test data)
    (equal test data)))

(defmethod! route (message &rest tests)
  :numouts 1
  :icon '(611)
  (values-list (copy-list (cons message 
                                (mapcar 
                                 #'(lambda (route-item) 
                                     (when (test-match message route-item) message))
                                 tests))))
  )

(defmethod get-boxcallclass-fun ((self (eql 'route))) 'ReactiveRouteBox)


