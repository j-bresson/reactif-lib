(in-package :om)

;;; REACTIVE BEHAVIOR (ACTIVE BOXES):
;;; THE BOX AUTOMATICALLY EVALUATES when the INPUT of a box changes
;;;    - DIRECT input change (menu or user input)
;;;    - IF the value of the connected box changed (REQUIRES NOTIFICATION)

;;; WHEN A BOX IS SET REACTIVE OR WHEN IT IS REACTIVE AND GETS CONNECTED TO A BOX, IT REGISTERS AMONG THE CLIENTS

;;; NOTIFICATION BEHAVOR (ALL BOXES):
;;; When the value change (evaluation or user edition, e.g. inthe editors)
;;; ALL CLIENTS ARE NOTIFIED

;;; - DURING NOTIFICATION THE BOX CAN NOT BE REEVALUATED: IT IS TEMPORARILY LOCKED
;;; (works if NOTIFICATIONS ARE SINGLE THREADED AND SYNCHRONOUS)

;;; THE VALUE OF FUNCTION BOXES IS STORED ONLY DURING THE NOTIFICATION(S)

;;;=============================
;;; ReactiveBox:
;;; Any box participating to the reactive process (either "actively" or not)
;;;=============================

(defparameter *inactive-color* (om-make-color 0.8 0.5 0.5))

(defclass OMReactiveBox () 
  ((active :initarg :active :accessor active :initform nil)
   (listeners :initarg :listeners :accessor listeners :initform nil)
   ;;; STATE-LOCK = this box is the event source and his evaluation is locked
   (state-lock :initarg :state-lock :accessor state-lock :initform nil) 
   ;;; GEN-FLAG = this box has already been valuated during this generation
   (gen-flag :initarg :gen-flag :accessor gen-flag :initform nil)
   ;;; PUSH-TAG = this box is tagged as being is in the notification path for the current event
   (push-tag :initarg :push-tag :accessor push-tag :initform nil)
   (color :initarg :color :accessor color :initform nil)
   ))


(defmethod active ((self t)) nil)
(defmethod listeners ((self t)) nil)
(defmethod (setf listeners) (a b) nil)

(defmethod set-active ((self t) react) (om-beep))
(defmethod set-active ((self OMReactiveBox) react) (setf (active self) react))

;;; REactive subclasses for the different types of boxes
;;; If the system is integrated, it will be easier just to add the Reactive superclass to OMBox
(defclass OMReactiveBoxCall (OMReactiveBox OMBoxCall) ())
(defclass OMReactiveBoxlispCall (OMReactiveBox OMBoxlispCall) ())
(defclass OMReactiveBoxEditCall (OMReactiveBox OMBoxEditCall) ())
(defclass OMReactiveDIEBox (OMReactiveBox OMDIEBox) ())
(defclass OMReactiveBoxTypeCall (OMReactiveBox OMBoxTypeCall) ())
(defclass OMReactiveBoxPatch (OMReactiveBox OMBoxPatch) ())
(defclass OMReactiveBoxAbsPatch (OMReactiveBox OMBoxAbsPatch) ())


;;; It is not quite regular how the type of box is determined in creating OM boxes...
(defmethod get-boxlispclass-fun ((self t)) 'OMReactiveBoxlispCall)
(defmethod get-boxcallclass-fun ((self t)) 'OMReactiveBoxCall)
(defmethod get-type-of-ed-box ((self t))  'OMReactiveBoxEditCall)
(defmethod get-type-of-ed-box ((self d-i-box))  'OMReactiveDIEBox)
(setf *def-metaclass-box-patch* 'OMReactiveBoxPatch)
;;; ReactiveBox type call will not have reactivebox frame but still present the reactive attributes internally
(setf *def-metaclass-box-type* 'OMReactiveBoxTypeCall)

(defmethod box-class ((self OMPatchAbs)) 'OMReactiveBoxAbsPatch)


;;; AUTRES 
(defclass box-repeat-n-call (OMReactiveBox OMBoxcall) ())
;;; A FAIRE: OMLOOP, SEQUENCE, OMIF....


(defmethod omNG-save ((self OMReactiveBox) &optional (values? t)) 
  `(let ((box ,(call-next-method)))
     (when (fboundp 'set-active) (set-active box ,(active self)))
     box))

(defmethod omNG-copy ((self OMReactiveBox))
  `(let* ((copy ,(call-next-method)))
     (set-active copy ,(active self))
     copy))

(defmethod handle-key-event ((self patchPanel) (char (eql '#\x)))
  (mapcar #'(lambda (boxframe) 
              (set-active (object boxframe) (not (active (object boxframe))))
              (om-invalidate-view boxframe))
          (get-actives self)))


;(defmethod handle-key-event ((self patchPanel) (char (eql '#\k)))
;  (mapcar #'(lambda (boxframe) 
;              (list (name (object boxframe))
;                           (listeners (object boxframe)))
;              )
;          (get-actives self)))


(defclass ReactiveBoxFrame (boxframe) ())
(defclass ReactiveBoxEditorFrame (ReactiveBoxFrame boxeditorframe) ())
(defclass ReactiveDIEditorframe (ReactiveBoxFrame DIEditorframe) ())
(defclass ReactiveBoxTypeframe (ReactiveBoxFrame BoxTypeframe) ())
(defclass ReactivePatchBoxFrame (ReactiveBoxFrame patchboxframe) ())
(defclass ReactiveAbsPatchBoxFrame (ReactiveBoxFrame patchboxabsFrame) ())

(defmethod get-frame-class ((self OMReactiveBox)) 'ReactiveBoxFrame)
(defmethod get-frame-class ((self OMReactiveBoxEditCall)) 'ReactiveBoxEditorFrame)
(defmethod get-frame-class ((self OMReactiveDIEBox)) 'ReactiveDIEditorframe)
(defmethod get-frame-class ((self OMReactiveBoxTypeCall)) 'ReactiveBoxTypeframe)
(defmethod get-frame-class ((self OMReactiveBoxPatch)) 'ReactivePatchBoxFrame)
(defmethod get-frame-class ((self OMReactiveBoxAbsPatch)) 'ReactiveAbsPatchBoxFrame)

(defclass arrayBox (OMReactiveBox OMBoxEditCall ) ())
(defclass OMaiffFilebox (OMReactiveBox OMBoxEditCall) ())
(defclass OMboxif (OMReactiveBox OMBoxcall) ())

(defclass reactiveArrayBoxframe (ReactiveBoxFrame boxeditorframe) ())
(defclass reactiveSoundBoxframe (ReactiveBoxFrame boxeditorframe) ())
(defclass reactiveIfBoxframe (ReactiveBoxFrame boxframe) ())

(defmethod get-frame-class ((self arrayBox)) 'ReactiveBoxEditorFrame)
(defmethod get-frame-class ((self OMaiffFilebox)) 'ReactiveBoxEditorFrame)
(defmethod get-frame-class ((self OMboxif)) 'reactiveIfBoxframe)



(defmethod draw-before-box ((self ReactiveBoxFrame))
  (call-next-method)
  (om-with-focused-view self
    (when (or (active (object self)) (color (object self)))
      (om-with-fg-color nil (or (color (object self)) (om-make-color 0.96 0.9 0.85))
        ;(if (active (object self))
            (om-fill-rect 0 0 (1- (w self)) (- (h self) 4))
          ;(om-draw-rect 0 4 (1- (w self)) (- (h self) 8))
        ;  )
        ))
    (when (push-tag (object self))
      (om-with-fg-color nil *om-black-color*        
        (om-fill-rect 0 0 6 6)))
    (when (gen-flag (object self))
      (om-with-fg-color nil *om-red-color*        
        (om-fill-rect 8 0 6 6)))
    (when (state-lock (object self))
      (om-with-fg-color nil *om-purple-color*        
        (om-fill-rect 16 0 6 6)))
    ))


;;; CONNECTION/ REGISTRATION OF CLIENTS

;;; called when a connection is made
(defmethod connect-ctrl ((self OMReactiveBox) input numout)
  (call-next-method)
  (let ((box (box-ref input)))
    (if box
        (unless (find box (listeners self) :test 'equal)
          (push box (listeners self)))
      (om-beep-msg (string+ "Error: Listener could not be added to box " (name self) ".")))))

;;; called when a connection is removed
(defmethod remove-connection ((self ReactiveBoxFrame) index)
  (let ((connected (car (connected? (nth index (inputs (object self)))))))
    (setf (listeners connected)
          (remove (object self) (listeners connected) :test 'equal)))
  (call-next-method))

;;; called when box is rmoved
(defmethod delete-connections-with-other-boxes ((self ReactiveBoxFrame) otherframes patchpanel)
  (mapcar #'(lambda (in)
              (when (connected? in)
                (setf (listeners (car (connected? in)))
                      (remove (object self) (listeners (car (connected? in))) :test 'equal))
                ))
              (inputs (object self)))
          (call-next-method))

;;; REDEFS

(defmethod omNG-save ((self OMBoxCall) &optional (values? nil))
  "Save a box"
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (when (equal (allow-lock self) "x") (saveValueinBox (value self)))))
    `(om-load-boxcall ',(saveBox? self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(frame-name self) ,(numouts self))))


(defmethod omNG-box-value ((self OMBoxcall) &optional (numout 0))
   "Eval the output indexed by 'numout' for the box 'self'. In this method we call the generic function reference of 'self'."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                        (om-report-condition c))
                                                  :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) 
             (list (special-lambda-value self (reference self))))
       (car (value self)))
      ((equal (allow-lock self) "o") (fdefinition (reference self)))
      ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth numout (value self)))
      (t (let* ((args (eval-box-inputs self))
                (themethod (compute-applicable-methods (fdefinition (reference self)) args)) rep)
           (if (null themethod)
             (progn (dialog-message (string+ "No method is defined for these types in box " (name self)))
                    (om-abort))
             (progn
               (when (and (EditorFrame (car themethod)) (not (compiled? (car themethod))))
                 (modify-genfun (EditorFrame (car themethod))))
               (setf rep (get-box-value-list self args))
               ))
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           ;;;; TEST
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           ;;;;
           (nth numout rep))))))


(defmethod omNG-box-value ((self OMBoxlispCall) &optional (num-out 0))
   (declare (ignore num-out))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                           (om-report-condition c ))
                                                  :size (om-make-point 300 200))
                               (clear-after-error self)
                               (om-abort)))))
     (cond
      ((equal (allow-lock self) "l") 
       (setf (value self) 
             (list (special-lambda-value self (reference self))))
       (car (value self)))
      ((equal (allow-lock self) "o") (omNG-make-new-lispfun (reference self)))
      ((and (value self) (equal (allow-lock self) "x")) (nth 0 (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
      (t (let* (rep)
           (if (macro-function (reference self))
             (let ((*compiling-macro-boite* t)
                   (oldletlist *let-list*)
                   code)
               (setf *let-list* nil)
               (setf code (decode self))
               (setf rep (multiple-value-list (eval `(let* ,(reverse *let-list*) (,(reference self) ,.code)))))
               (setf *let-list* oldletlist))
             (let* ((args  (loop for input in (inputs self)
                                 when (not (keyword-input-p input)) collect (omNG-box-value  input)))
                    (args (list+ args (eval-keywords self))))
               (setf rep (multiple-value-list (apply (reference self) args)))))
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           ;;;; TEST
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           ;;;;
           (nth 0 rep))))))


(defmethod omNG-box-value ((self OMBoxif) &optional (numout 0))
   (declare (ignore numout))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'omif))
    ((and (equal (allow-lock self) "x") (value self)) (nth 0 (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
    (t (let ((test (omNG-box-value (first (inputs self))))
             rep)
         (if test
           (setf rep (list (omNG-box-value (second (inputs self)))))
           (setf rep (list (when (third (inputs self)) (omNG-box-value (third (inputs self)))))))
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) rep))
         (when (equal (allow-lock self) "x")
           (setf (value self) rep))
         (when (equal (allow-lock self) nil)
           (setf (value self) rep))
         (nth 0 rep)))))





(defmethod remove-lock-button ((self omboxframe))
   "Remove the button subview from self."
   (om-remove-subviews (iconview self) (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil)
   ;(setf (value (object self)) nil)
   )


;;; NOTIFICATION TO CLIENTS
;;; When the box is evaluated


(defparameter *eval-color* (om-make-color 0.9 0.6 0.6))
(defparameter *notify-color* (om-make-color 0.5 0.6 0.7))
(defparameter *defcolortime* 0.)

(defmethod box-color (box color &optional wait)
  (setf (color box) color)
  (when (car (frames box))
    (om-redraw-view (car (frames box)))
    (om-invalidate-view (car (frames box)))
    (when (or wait *defcolortime*)
      (sleep (or wait *defcolortime*)))))
  
(defmethod box-color ((box OMBoxTypeCall) color &optional wait)
  (when (car (frames box))
    (let ((prev-color (om-get-bg-color (iconview (car (frames box))))))
      (om-set-bg-color (iconview (car (frames box))) color)
      (when (or wait *defcolortime*)
        (sleep (or wait *defcolortime*)))
      (om-set-bg-color (iconview (car (frames box))) prev-color)
    ;(om-redraw-view (iconview (car (frames box))))
      (om-invalidate-view (iconview (car (frames box))))
      )))

(defmethod current-box-value ((self OMBoxCall) &optional (numout nil))
  (if numout (nth numout (value self)) (value self)))

(defmethod current-box-value ((self OMBoxEditCall) &optional (numout nil))
  (if numout (rep-editor (value self) numout) (value self)))

(defmethod current-box-value ((self OMBoxTypeCall) &optional num-out)
   (declare (ignore num-out))
   (if (consp (value self))
     (eval (omng-copy (value self)))
     (value self)))

(defmethod clear-ev-once ((self OMReactiveBox))
   (call-next-method)
   (setf (state-lock self) nil)
   (setf (gen-flag self) nil)
   (setf (push-tag self) nil)
   (setf (color self) nil)
   (om-invalidate-view (car (frames self))))


;;; AN EXTERNAL EVENT HAPPENS
(defmethod signal-event ((self OMReactiveBox))
  (setf (state-lock self) t)
  (OMR-Notify self)
  (setf (state-lock self) nil))

;;; SELF-NOTIFICATION (REEVALUATES/SIGNALS ON A SEPARATE THREAD)
(defmethod self-notify (box &optional (separate-thread t))
  (when (active box)
    (let ((panel (and (car (frames box))
                      (panel (om-view-window (car (frames box)))))))
      (funcall 
       (if separate-thread 'om-eval-enqueue 'eval)
       (if panel    
           `(progn
              (setf *cur-eval-panel* ,panel)
              (omng-box-value ,box)
              (signal-event ,box)
              (clear-ev-once ,panel))
         `(omr-notify ,box)
         )))))

;;; when an input is edited
(defmethod exit-from-dialog ((self input-text-enter-view) newtext)
  (call-next-method)
  (let ((box (object (om-view-container (object self)))))
    (self-notify box)))

;;; EDITOR NOTIFICATION (when the editor is modified by user actions)
(defmethod report-modifications :after ((self EditorView))
  (when (is-boxpatch-p (ref self))
    (self-notify (ref self))))

;;; BoxType edited
(defmethod exit-from-dialog ((self BoxType-enter-view) newtext)
  (call-next-method)
  (self-notify (object (om-view-container (object self))))
  )

(defmethod OMR-Notify ((self t)) nil)

;(defun propagate-push-tag (box)
;  (mapcar #'propagate-push-tag-to-listeners (remove-if-not 'active (remove nil (listeners box)))))

;(defun propagate-push-tag-to-listeners (box)
;  (unless (state-lock box)
;    (setf (push-tag box) t)
;    (box-color box (color box) 0)
;    (mapcar #'propagate-push-tag-to-listeners (remove-if-not 'active (remove nil (listeners box))))))

(defmethod eval-box ((self ReactiveBoxFrame)) 
  (omng-box-value (object self))
  (let ((v (current-box-value (object self))))
    (if (consp v) 
        (if (> (length v) 1)
            (format *om-stream* "OMR => [ ~{~S ~}]~%" v)
          (format *om-stream* "OMR => ~S~%" (car v)))
      (format *om-stream* "OMR => ~S~%" v)))
  (signal-event (object self)))

(defmethod eval-box ((self omboxframe))
  
  )



(defmethod omNG-box-value ((self OMReactiveBox) &optional (numout 0)) 
 (if (state-lock self)
     (current-box-value self numout)
   (let (val)
      ;(print (list "EVAL BOX" (name self)))
      (box-color self *eval-color*)   
      (setf val (call-next-method))   
      (setf (gen-flag self) t)
      (box-color self nil) ; *inactive-color*
      val)
    )
  )


;;; REACTIVE BEHAVIOUR
;;; DO NOT NOTIFY WHO JUST CALLED ME
(defmethod OMR-Notify ((self OMReactiveBox))
  ;(print (list "NOTIFIED BOX" (name self)))
  (box-color self *notify-color*)
  (unless (push-tag self)
    (setf (push-tag self) t)
    (let ((listeners (remove-if-not 'active (listeners self))))
      (if (and (active self) listeners)
          (mapcar 'omr-notify listeners)
        (omNG-box-value self))))
  (box-color self nil))


;;;=====================================
;;; SOURCES
;;;=====================================

;;; DI BOXES (redef)

(defmethod set-action ((self button) fun box) 
  (om-set-dialog-item-action-function self #'(lambda (x) 
                                               (when fun
                                                 (let ((panel (get-patchpanel (editor (om-view-window self)))))
                                                   (om-eval-enqueue 
                                                    `(progn
                                                       (setf (di-data ,self) 
                                                             (if (functionp ,fun) 
                                                                 (funcall ,fun) ;;; call it
                                                               (omng-box-value (second (inputs ,box))) ;; reevaluate
                                                               ))
                                                       (setf *cur-eval-panel* ,panel)
                                                       (self-notify ,box nil)
                                                       (clear-ev-once ,panel)
                                                       (setf (di-data ,self) nil)
                                                       )))))
                                      ))


(defmethod set-function ((self slider) fun box) 
  (om-set-dialog-item-action-function self #'(lambda (x) 
                                                    (when fun
                                                      (funcall fun (om-slider-value x))
                                                      )
                                                    (self-notify box))))






(defun om-push (event) (print EVENT))


;;;=====================================
;;; SEND/RECEIVE
;;;=====================================

(defmethod! om-send ((self t) &optional (target :om))
   (let ((boxes (find-receive-boxes target)))
     (mapcar #'(lambda (b)
                 (setf (value b) self)
                 (self-notify b nil))
             boxes)
     self))
                 
(defmethod! om-receive (targetname) :initvals '(:om) t)

(defclass ReactiveOMRecieveBox (OMReactiveBox OMBoxCall) ())
(defmethod get-boxcallclass-fun ((self (eql 'om-receive))) 'ReactiveOMRecieveBox)
(defmethod omNG-box-value ((self ReactiveOMRecieveBox) &optional (numout 0)) (value self))
  

(defun find-boxes (type)
  (loop for win in (remove-if-not 
                    #'(lambda (w) (equal 'patcheditor (type-of (editor w))))
                    (om-get-all-windows 'editorwindow)) append
        (loop for b in (boxes (object (editor win))) 
              when (equal type (reference b))
              collect b)))


(defun find-receive-boxes (target)
  (let ((boxes (find-boxes 'om-receive)))
    (remove-if-not #'(lambda (b) (equal (value (nth 0 (inputs b))) target))
                   boxes)
    ))



