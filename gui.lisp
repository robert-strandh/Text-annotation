(cl:in-package #:text-annotation)

(clim:define-application-frame text-annotation ()
  ((%sentence :initarg :sentence :accessor sentence)
   (%selected-words :initform '() :accessor selected-words))
  (:panes
   (sentence :application
	     :scroll-bars nil
	     :display-function 'display-sentence)
   (predicate-action :application
		     :scroll-bars nil
		     :display-function 'display-predicate-action)
   (predicate :application
	      :scroll-bars nil
	      :display-function 'display-predicate)
   (agent-action :application
		 :scroll-bars nil
		 :display-function 'display-agent-action)
   (agent :application
	  :scroll-bars nil
	  :display-function 'display-agent)
   (experiencer-action :application
		       :scroll-bars nil
		       :display-function 'display-experiencer-action)
   (experiencer :application
		:scroll-bars nil
		:display-function 'display-experiencer)
   (patient-action :application
		   :scroll-bars nil
		   :display-function 'display-patient-action)
   (patient :application
	    :scroll-bars nil
	    :display-function 'display-patient)
   (temporal-action :application
		    :scroll-bars nil
		    :display-function 'display-temporal-action)
   (temporal :application
	     :scroll-bars nil
	     :display-function 'display-temporal)
   (location-action :application
		    :scroll-bars nil
		    :display-function 'display-location-action)
   (location :application
	     :scroll-bars nil
	     :display-function 'display-location)
   (purpose-action :application
		   :scroll-bars nil
		   :display-function 'display-purpose-action)
   (purpose :application
	    :scroll-bars nil
	    :display-function 'display-purpose)
   (manner-action :application
		  :scroll-bars nil
		  :display-function 'display-manner-action)
   (manner :application
	   :scroll-bars nil
	   :display-function 'display-manner)
   (degree-action :application
		  :scroll-bars nil
		  :display-function 'display-degree-action)
   (degree :application
	   :scroll-bars nil
	   :display-function 'display-degree)
   (negation-action :application
		    :scroll-bars nil
		    :display-function 'display-negation-action)
   (negation :application
	     :scroll-bars nil
	     :display-function 'display-negation)
   (model-action :application
		 :scroll-bars nil
		 :display-function 'display-model-action)
   (model :application
	  :scroll-bars nil
	  :display-function 'display-model)
   (other-action :application
		 :scroll-bars nil
		 :display-function 'display-other-action)
   (other :application
	  :scroll-bars nil
	  :display-function 'display-other)
   (interactor :interactor :height 20))
  (:layouts
   (default
    (clim:vertically (:width 500 :height 500)
      sentence
      (clim:horizontally ()
	(clim:vertically (:equalize-width t :width 100)
	  predicate-action
	  agent-action
	  experiencer-action
	  patient-action
	  temporal-action
	  location-action
	  purpose-action
	  manner-action
	  degree-action
	  negation-action
	  model-action
	  other-action)
	(clim:vertically (:equalize-width t :width 400)
	  predicate
	  agent
	  experiencer
	  patient
	  temporal
	  location
	  purpose
	  manner
	  degree
	  negation
	  model
	  other))
      interactor))))

(defmacro define-data-display-function
    (function-name accessor-name)
  `(defun ,function-name (frame pane)
     (loop for word in (,accessor-name (sentence frame))
	   do (format pane "~a " (text word)))))

(define-data-display-function display-predicate predicate)
(define-data-display-function display-agent agent)
(define-data-display-function display-experiencer experiencer)
(define-data-display-function display-patient patient)
(define-data-display-function display-temporal temporal)
(define-data-display-function display-location location)
(define-data-display-function display-purpose purpose)
(define-data-display-function display-manner manner)
(define-data-display-function display-degree degree)
(define-data-display-function display-negation negation)
(define-data-display-function display-model model)
(define-data-display-function display-other other)

(defclass selectable-word (word)
  ())

(define-text-annotation-command (com-select-word) ((word 'selectable-word))
  (setf (selected-words clim:*application-frame*)
	(append (selected-words clim:*application-frame*) (list word))))

(clim:define-presentation-to-command-translator
    select-word
    (selectable-word com-select-word text-annotation)
    (object)
  `(,object))

(defclass action ()
  ((%accessor-name :initarg :accessor-name :reader accessor-name)))

(defmethod print-object ((object action) stream)
  (format stream "to ~a" (accessor-name object)))

(defmacro define-action-display-function
    (function-name accessor-name string)
  `(defun ,function-name (frame pane)
     (declare (ignore frame))
     (clim:with-output-as-presentation
	 (pane
	  (make-instance 'action :accessor-name ',accessor-name)
	  'action)
       (format pane "~a" ,string))))

(define-action-display-function display-predicate-action predicate "predicate")
(define-action-display-function display-agent-action agent "agent")
(define-action-display-function display-experiencer-action experiencer "experiencer")
(define-action-display-function display-patient-action patient "patient")
(define-action-display-function display-temporal-action temporal "temporal")
(define-action-display-function display-location-action location "location")
(define-action-display-function display-purpose-action purpose "purpose")
(define-action-display-function display-manner-action manner "manner")
(define-action-display-function display-degree-action degree "degree")
(define-action-display-function display-negation-action negation "negation")
(define-action-display-function display-model-action model "model")
(define-action-display-function display-other-action other "other")

(defun display-sentence (frame pane)
  (loop for word in (all-words (sentence frame))
	do (clim:with-output-as-presentation (pane word 'selectable-word)
	     (if (member word (selected-words frame))
		 (clim:with-drawing-options (pane :ink clim:+red+)
		   (format pane "~a " (text word)))
		 (format pane "~a " (text word))))))

(defun text-annotation ()
  (clim:run-frame-top-level
   (clim:make-application-frame
    'text-annotation
    :sentence
    (make-instance 'sentence :all-words '()))))

(define-text-annotation-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-text-annotation-command (com-load-file :name t) ((filename 'pathname))
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream))
	   (separated (split-sequence:split-sequence #\Space line))
	   (words (loop for chars in separated
			collect (make-instance 'word :text chars))))
      (setf (sentence clim:*application-frame*)
	    (make-instance 'sentence
	      :all-words words)))))

(define-text-annotation-command (com-move-selection) ((action 'action))
  (funcall (fdefinition `(setf ,(accessor-name action)))
	   (selected-words clim:*application-frame*)
	   (sentence clim:*application-frame*))
  (setf (selected-words clim:*application-frame*) '()))

(clim:define-presentation-to-command-translator
    move-selection
    (action com-move-selection text-annotation)
    (object)
  `(,object))
