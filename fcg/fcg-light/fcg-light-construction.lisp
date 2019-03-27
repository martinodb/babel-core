(in-package :fcg)

;; Main definitions for FCG Light
;; File by Paul - 30/03/2015
;; Structure:
;;     FCG Light Construction
;;     Processing Construction
;;     FCG Light Construction Set
;;     FCG Query
;;     Interface macro's
;;     Parsing an FCG Light feature structure
;;     Helper functions
;;
;; Some modifications by Remi for changing the name FCG-Light into FCG.
;; Katrien added an attributes slot to def-fcg-cxn on 09/12/2015
;;
;; Remi: Removed the function check-feature-types because it did not do anything anymore.
;;       The function, however, was a good idea and maybe we could revive it as some syntax-check
;;       that FCG-users could run if they want.

;; Export symbols
(export '(fcg-set-hierarchy-features
          fcg-construction
          fcg-construction-set
          hashed-fcg-construction-set
          *fcg-constructions*
          def-fcg-cxn
          processing-cxn
          original-cxn
          def-fcg-constructions
          get-original-cxn
          get-processing-cxn
          get-processing-cxn-inventory
          get-original-cxn-inventory
          fcg-light-construction
          *fcg-light-constructions*
          contributing-unit
          conditional-unit
          fcg-unit-feature
          fcg-unit-feature-value
          comprehension-lock
          formulation-lock
          conditional-part
          contributing-part
          --
          cxn-set
          hash
          comprehend
          comprehend-all
          formulate
          formulate-all
          comprehend-and-formulate
          remove-key-arg
          remove-key-args
          find-key-arg
          copy-fcg-construction-set-without-cxns
          corresponding-processing-cxn-inventory-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FCG Constructions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fcg-construction (construction)
  ((name 
    :type symbol :initarg :name :initform (make-id 'CXN) :accessor name )
   (contributing-part :accessor contributing-part
                      :initarg :contributing-part
                      :initform '()
                      :type (or null list)
                      :documentation "contains list of contributing units")
   (conditional-part :accessor conditional-part
                     :initarg :conditional-part
                     :initform '()
                     :type list
                     :documentation "contains list of conditional units")
   (feature-types :type (or list null)
                  :initform nil
                  :initarg
                  :feature-types
                  :accessor feature-types)
   (disable-automatic-footprints :type (or t nil)
                                 :initform nil
                                 :initarg :disable-automatic-footprints
                                 :accessor disable-automatic-footprints)
   (attributes :type (or list null)
               :initform nil
               :initarg :attributes
               :accessor attributes)
   (processing-cxn :type (or construction null)
                   :initform nil
                   :initarg :processing-cxn
                   :accessor processing-cxn
                   :documentation "Slot for adding the translated construction.")
   (description :type (or string null)
                :initform ""
                :initarg :description
                :accessor description)))

(defclass contributing-unit ()
  ((name 
    :type symbol :initarg :name :initform (make-id 'UNIT) :accessor name )
   (unit-structure :accessor unit-structure
                   :initform '()
                   :initarg :unit-structure
                   :type feature-structure
                   :documentation "a contributing unit")))

(defclass conditional-unit ()
  ((name 
    :type symbol :initarg :name :initform (make-id 'UNIT) :accessor name )
   (formulation-lock :accessor formulation-lock
                     :initform '()
                     :initarg :formulation-lock
                     :type feature-structure
                     :documentation "contains all features from the formulation-lock")
   (comprehension-lock :accessor comprehension-lock
                       :initform '()
                       :initarg :comprehension-lock
                       :type feature-structure
                       :documentation "contains all features from the comprehension-lock")))

(defmethod fcg-unit-feature ((unit contributing-unit) name &optional (include-hash nil))
  (let ((found nil))
    (loop for feature in (unit-structure feature)
          until found
          do (cond ((eq (feature-name feature) name)
		    (setq found feature))
		   ((and (eq (feature-name feature) 'hash)
			 (eq (second feature) name))
                    (if include-hash
                      (setq found feature)
                      (setq found (rest feature))))))
    found))

(defmethod fcg-unit-feature ((unit conditional-unit) name &optional (include-hash nil))
  (let ((found nil))
    (loop for feature in (append (formulation-lock unit)
                                 (comprehension-lock unit))
          until found
          do (cond ((eq (feature-name feature) name)
                    (setq found feature))
                   ((and (eq (feature-name feature) 'hash)
                         (eq (second feature) name))
                    (if include-hash
                      (setq found feature)
                      (setq found (rest feature))))))
    found))

(defmethod fcg-unit-feature-value ((unit contributing-unit) name)
  (let ((found (fcg-unit-feature unit name)))
    (when found
      (first (rest found)))))

(defmethod fcg-unit-feature-value ((unit conditional-unit) name)
  (let ((found (fcg-unit-feature unit name)))
    (when found
      (first (rest found)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing Construction    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass processing-construction (construction)
  ((original-cxn :type (or fcg-construction null)
                 :initform nil
                 :initarg :original-cxn
                 :accessor original-cxn
                 :documentation "Slot for adding the translated construction.")))

(defmethod copy-object ((construction processing-construction))
  (let ((copy (make-instance 'construction
			     :name (name construction)
			     :domain (domain construction)
			     :match-source (match-source construction)
			     :left-pole (left-pole-structure construction)
			     :attributes (copy-list (attributes construction))
			     :right-pole (right-pole-structure construction)
                             :original-cxn (original-cxn construction)
                             :cxn-inventory (cxn-inventory construction)
			     )))
    (unless (eq (persistent-cxn construction) construction)
      (setf (persistent-cxn copy) (persistent-cxn construction)))
    copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FCG Light Construction Set    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(feature-types processing-cxn-inventory))

(defclass fcg-construction-set (construction-inventory object-w-learning)
  ((constructions 
    :type (or list null)
    :initform nil
    :initarg :constructions)
   (disable-automatic-footprints
    :type (or t nil)
    :initform nil
    :initarg :disable-automatic-footprints
    :accessor disable-automatic-footprints)
   (feature-types 
    :type (or list null)
    :initform nil
    :initarg :feature-types
    :accessor feature-types)
   (processing-cxn-inventory
    :type (or construction-set null)
    :initform nil
    :initarg :processing-cxn-inventory
    :accessor processing-cxn-inventory)))

(defmethod initialize-instance :after ((fcg-construction-set fcg-construction-set)
                                       &key &allow-other-keys)
  "after method for when an fcg-construction set is created"
  ;; set default feature-types if they were not provided
  (unless (feature-types fcg-construction-set)
    (setf (feature-types fcg-construction-set)
          '((form set-of-predicates)
            (meaning set-of-predicates)
            (subunits set)
            (args sequence)
            (boundaries set-of-predicates)
            (footprints set)))))

(defclass hashed-fcg-construction-set (hashed-construction-set fcg-construction-set)
  ()
  (:documentation "Class for hashed FCG construction sets"))

(defmethod constructions ((fcg-construction-set fcg-construction-set) &key &allow-other-keys)
  "return the constructions of an fcg-light-construction-set"
  (slot-value fcg-construction-set 'constructions))

(defmethod (setf constructions) ((construction-list list)
                                 (construction-set fcg-construction-set))
  (setf (slot-value construction-set 'constructions) construction-list))

(defmethod add-cxn ((fcg-construction fcg-construction)
                    (fcg-construction-set fcg-construction-set)
                    &key
                    (replace-when-equivalent t)
                    (equivalent-test #'eql)
                    (equivalent-key #'name) (processing-cxn-inventory nil)
                    &allow-other-keys)
  (let ((processing-construction (fcg-light-cxn->fcg-2-cxn
                                  fcg-construction
                                  :processing-cxn-inventory
                                  (or processing-cxn-inventory
                                      (processing-cxn-inventory fcg-construction-set)))))
    ;; For much faster access:
    (setf (processing-cxn fcg-construction) processing-construction
          (original-cxn processing-construction) fcg-construction
          ;; Set cxn-inventory slot of cxn
          (cxn-inventory fcg-construction) fcg-construction-set)
    ;; For fcg-construction
    (push fcg-construction (constructions fcg-construction-set))
    ;; For processing-cxn  
    (add-cxn processing-construction 
             (processing-cxn-inventory fcg-construction-set)
             :replace-when-equivalent replace-when-equivalent
             :equivalent-test equivalent-test
             :equivalent-key equivalent-key)
    (values fcg-construction-set fcg-construction)))

(defmethod add-cxn :before ((construction fcg-construction)
                            (construction-inventory fcg-construction-set)
                            &key (replace-when-equivalent t)
                            (equivalent-test #'eql) (equivalent-key #'name))
  (when replace-when-equivalent
    (delete-cxn construction construction-inventory 
                :test equivalent-test :key equivalent-key)))

(defmethod find-cxn ((fcg-construction fcg-construction) (fcg-construction-set fcg-construction-set) 
                     &key (key #'name) (test #'eql))
  (find-cxn fcg-construction (append (constructions fcg-construction-set))
            :key key :test test))

(defmethod find-cxn ((construction t) (fcg-construction-set fcg-construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil))
  (find construction (append (constructions fcg-construction-set)
                             (when search-trash (trash fcg-construction-set))) 
        :key key :test test))

(defmethod find-cxn ((fcg-construction fcg-construction)
                     (hashed-fcg-construction-set hashed-fcg-construction-set) 
                     &key (key #'name) (test #'eql))

  (loop with hashes = (hash fcg-construction (get-configuration hashed-fcg-construction-set :hash-mode))
        for hash in (if (null hashes) (list nil) hashes)
        for cxn = (find fcg-construction (gethash hash (constructions-hash-table hashed-fcg-construction-set))
                        :test test :key key)
        when cxn
        do (return cxn)))

(defmethod find-cxn ((construction t) (hashed-fcg-construction-set hashed-fcg-construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil) (hash-key nil))
  (find construction
        (append (gethash hash-key (constructions-hash-table hashed-fcg-construction-set))
                (when search-trash (trash hashed-fcg-construction-set)))
        :key key :test test))

(defmethod find-cxn ((construction fcg-construction) (constructions list) 
                     &key (key #'name) (test #'eql))
  (find (funcall key construction) constructions
        :key key :test test))

(defmethod clear ((construction-set fcg-construction-set) &key &allow-other-keys)
  (setf (constructions construction-set) nil)
  (setf (constructions (processing-cxn-inventory construction-set)) nil))

(defmethod delete-cxn ((construction fcg-construction) 
                       (construction-set fcg-construction-set)
                       &key (key #'name) (test #'eql))
  "Deletes a construction from the construction inventory."
  (let ((to-delete (find-cxn construction construction-set :test test :key key)))
    (when to-delete
      (setf (constructions construction-set)
            (remove to-delete (constructions construction-set)))
      (delete-cxn (get-processing-cxn to-delete) (processing-cxn-inventory construction-set)
                  :key key :test test)
      to-delete)))

(defmethod delete-cxn ((construction fcg-construction) 
                       (construction-set hashed-fcg-construction-set)
                       &key (key #'identity) (test #'eql) (move-to-trash nil))
  "Deletes a construction from the fcg-construction-set and the
processing construction inventory. Also removes the hashkey if the
value has become NIL."
  (let ((to-delete (find-cxn construction construction-set :test test :key key)))

    (when to-delete
      (when move-to-trash
        (push to-delete (trash construction-set)))
      (loop
       with hashes = (hash to-delete (get-configuration construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do
       (setf (gethash hash (constructions-hash-table construction-set))
                (remove to-delete (gethash hash (constructions-hash-table construction-set))))
       (delete-cxn (get-processing-cxn to-delete) (processing-cxn-inventory construction-set))
       (unless (gethash hash (constructions-hash-table construction-set))
         (remhash hash (constructions-hash-table construction-set))))
      to-delete)))


(defmethod copy-object ((cxn fcg-construction))
  "Copies an fcg-construction object"
  (let ((copy (make-instance 'fcg-construction
			     :name (name cxn)
			     :domain (domain cxn)
			     :match-source (match-source cxn)
			     :left-pole (left-pole-structure cxn)
			     :attributes (copy-list (attributes cxn))
			     :right-pole (right-pole-structure cxn)
                             :cxn-inventory (cxn-inventory cxn)
                             :contributing-part (contributing-part cxn)
                             :conditional-part (conditional-part cxn)
                             :feature-types (feature-types cxn)
                             :description (description cxn)
                             :processing-cxn (processing-cxn cxn)
                             :disable-automatic-footprints (disable-automatic-footprints cxn))))
    copy))

(defmethod copy-object-content ((source fcg-construction-set)
                                (destination fcg-construction-set))
  (let ((configuration-destination (copy-object (configuration source)))
        (viualization-configuration-destination (copy-object (visualization-configuration source)))
        (blackboard-destination (copy-object (blackboard source))))
    (setf (constructions destination) (copy-list (constructions source)))
    (setf (feature-types destination) (copy-list (feature-types source)))
    (setf (configuration destination) configuration-destination)
    (setf (blackboard destination) blackboard-destination)
    (setf (processing-cxn-inventory destination)
          (make-instance 'construction-set ;; deep copy (well, not for the constructions themselves)
                         :original-cxn-set destination
                         :constructions  (copy-list (constructions (processing-cxn-inventory source)))
                         :configuration configuration-destination
                         :visualization-configuration viualization-configuration-destination
                         :blackboard blackboard-destination
                         :trash  (copy-list (trash (processing-cxn-inventory source)))))))

(defun copy-fcg-construction-set-without-cxns (fcg-construction-set)
  "Make a copy of an FCG construction set, without copying the
constructions themselves. Intended to use in meta-layer for applying a
construction on the fly."
  (let ((temporary-cxn-set
         (eval `(def-fcg-constructions ,(gensym)
                  :feature-types ,(feature-types fcg-construction-set)
                  :hierarchy-features ,(hierarchy-features fcg-construction-set)
                  :disable-automatic-footprints ,(disable-automatic-footprints fcg-construction-set)
                  :cxn-inventory temporary-cxn-set))))

    (let ((configuration (copy-object (configuration fcg-construction-set)))
          (visualization-configuration (copy-object (visualization-configuration fcg-construction-set)))
          (expansion-data (copy-object (expansion-data fcg-construction-set)))
          (blackboard (copy-object (blackboard fcg-construction-set))))
      
    (setf (configuration temporary-cxn-set) configuration
          (configuration (processing-cxn-inventory temporary-cxn-set)) configuration
          (visualization-configuration temporary-cxn-set) visualization-configuration
          (visualization-configuration (processing-cxn-inventory temporary-cxn-set)) visualization-configuration
          (expansion-data temporary-cxn-set) expansion-data
          (expansion-data (processing-cxn-inventory temporary-cxn-set)) expansion-data
          (blackboard temporary-cxn-set) blackboard
          (blackboard (processing-cxn-inventory temporary-cxn-set)) blackboard)

    (setf (original-cxn-set (processing-cxn-inventory temporary-cxn-set)) temporary-cxn-set)
    
    temporary-cxn-set)))


(defclass fcg-query ()
  ((input :accessor sentence
          :initarg :sentence
          :initform '()
          :type list
          :documentation "input utterance/meaning representation.
                          either list of strings or list of meaning predicates")
   (direction :accessor direction
              :initarg :direction
              :initform '<-
              :type symbol
              :documentation "Direction of the query: -> or <-")
   (cxn-inventory 
    :type fcg-construction-set :initarg :cxn-inventory :initform '*fcg-constructions* :accessor cxn-inventory)
   (visualisation
    :type (or symbol string) :initarg :visualisation :initform nil :accessor visualisation
    :documentation "slot for determining whether/how you want to visualise the answer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashed FCG construction set    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cxn ((fcg-construction fcg-construction)
                    (fcg-construction-set hashed-fcg-construction-set)
                    &key
                    (replace-when-equivalent t)
                    (equivalent-test #'eql)
                    (equivalent-key #'name) (processing-cxn-inventory nil)
                    (recover-from-trash nil)
                    &allow-other-keys)
  (let ((processing-construction (fcg-light-cxn->fcg-2-cxn
                                  fcg-construction
                                  :processing-cxn-inventory
                                  (or processing-cxn-inventory
                                      (processing-cxn-inventory fcg-construction-set)))))
    ;; Set cxn-inventory slot of cxn
    (setf (processing-cxn fcg-construction) processing-construction
          (original-cxn processing-construction) fcg-construction
          (cxn-inventory fcg-construction) fcg-construction-set)
    ;; Add construction to the hash table
    (if recover-from-trash
      (let* ((trashed-cxn (find (funcall equivalent-key fcg-construction) (trash fcg-construction-set) 
                                :key equivalent-key
                                :test equivalent-test)))
        (if trashed-cxn
          (progn
            (loop for hash in (hash trashed-cxn (get-configuration fcg-construction-set :hash-mode))
                  do (setf (gethash hash (constructions-hash-table fcg-construction-set))
                           (push trashed-cxn
                                 (gethash hash (constructions-hash-table fcg-construction-set)))))
            (setf (trash fcg-construction-set) (remove trashed-cxn (trash fcg-construction-set))))
          (loop for hash in (hash fcg-construction (get-configuration fcg-construction-set :hash-mode))
                do (setf (gethash hash (constructions-hash-table fcg-construction-set))
                         (push fcg-construction
                               (gethash hash (constructions-hash-table fcg-construction-set)))))))
      (loop
       with hashes = (hash fcg-construction
                           (get-configuration fcg-construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do (when replace-when-equivalent
            (setf (gethash hash (constructions-hash-table fcg-construction-set))
                  (remove (funcall equivalent-key fcg-construction)
                          (gethash hash (constructions-hash-table fcg-construction-set)) :test equivalent-test :key equivalent-key)))
       (setf (gethash hash (constructions-hash-table fcg-construction-set))
             (cons fcg-construction
                   (gethash hash (constructions-hash-table fcg-construction-set))))))
  
    ;; For processing-cxn
    (add-cxn processing-construction
             (processing-cxn-inventory fcg-construction-set)
             :replace-when-equivalent replace-when-equivalent
             :equivalent-test equivalent-test
             :equivalent-key equivalent-key
             :recover-from-trash recover-from-trash
             :hash-keys (hash fcg-construction
                              (get-configuration fcg-construction-set :hash-mode)))
    (values fcg-construction-set fcg-construction)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface macro's    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *fcg-constructions* nil)

;; Exploiting a generic function to work as a global variable. Allows FCG users to locally define their own
;; construction inventories to be used with the def-fcg-constructions macro.
(defgeneric corresponding-processing-cxn-inventory-type (cxn-inventory-type))

(defmethod corresponding-processing-cxn-inventory-type ((cxn-inventory-type (eql 'hashed-fcg-construction-set)))
  'hashed-construction-set)

(defmethod corresponding-processing-cxn-inventory-type ((cxn-inventory-type t))
  'construction-set)

(defmacro def-fcg-constructions (name &body keys-and-defs)
  "Creates a new fcg-construction-set, setting configuration, processing-cxn-inventory and configurations"
  ;; Check whether all keywords are known
  (check-def-fcg-constructions-keys keys-and-defs)
  ;; Do some pre-processing
  (let* ((name (eval-when-bound name)) ;; so you can also pass something lik (make-id 'grammar) 
         (creator-fn-name (internal-symb 'make- name '-cxns))
         (cxn-inventory (or (find-key-arg keys-and-defs :cxn-inventory) '*fcg-constructions*))
         (hierarchy-features (or (find-key-arg keys-and-defs :hierarchy-features) '(subunits)))
         (construction-inventory-type (or (find-key-arg keys-and-defs :cxn-inventory-type)
                                          (when (find-key-arg keys-and-defs :hashed) 'hashed-fcg-construction-set)
                                          'fcg-construction-set)))
    `(progn
       (with-disabled-monitor-notifications

         ;; Create function that makes grammar
         (defun ,creator-fn-name ()

           (setf ,cxn-inventory
                 (make-instance ',construction-inventory-type
                                :name ',name
                                :feature-types ',(find-key-arg keys-and-defs :feature-types)
                                :hierarchy-features ',hierarchy-features
                                :disable-automatic-footprints ',(find-key-arg keys-and-defs :disable-automatic-footprints)
                                :diagnostics (loop for diagnostic in ',(find-key-arg keys-and-defs :diagnostics)
                                                   collect (make-instance diagnostic))
                                :repairs (loop for repair in ',(find-key-arg keys-and-defs :repairs)
                                               collect (make-instance repair))
                                :processing-cxn-inventory
                                (make-instance ',(corresponding-processing-cxn-inventory-type
                                                  construction-inventory-type)
                                               :name ',name
                                               :hierarchy-features ',hierarchy-features)))
         
           ;; Make sure that fcg-construction-set and processing-cxn-inventory share certain objects
           ;; namely, configuration, 
           (setf (configuration (processing-cxn-inventory ,cxn-inventory))
                 (configuration ,cxn-inventory))
           (setf (visualization-configuration (processing-cxn-inventory ,cxn-inventory))
                 (visualization-configuration ,cxn-inventory))
           (setf (blackboard (processing-cxn-inventory ,cxn-inventory))
                 (blackboard ,cxn-inventory))
           (setf (expansion-data (processing-cxn-inventory ,cxn-inventory))
                 (expansion-data ,cxn-inventory))

           ;; Set other slots from processing-cxn-inventory
           (setf (original-cxn-set (processing-cxn-inventory ,cxn-inventory)) ,cxn-inventory)

           ;; Set configurations
           ,@(loop for configuration in (find-key-arg keys-and-defs :fcg-configurations)
                   collect
                   `(set-configuration ,cxn-inventory
                                       ,(first configuration) ',(cdr configuration)))
           
           ;; Set visualization-configurations
           ,@(loop for configuration in (find-key-arg keys-and-defs :visualization-configurations)
                   collect
                   `(set-configuration (visualization-configuration ,cxn-inventory)
                                       ,(first configuration) ',(cdr configuration)))

           ;; Add hierarchy features also to visualization-configuration
           (set-configuration (visualization-configuration ,cxn-inventory) :hierarchy-features ',hierarchy-features)
           
           ;; Set selected hierarchy         
           (set-configuration (visualization-configuration ,cxn-inventory) :selected-hierarchy (first ',hierarchy-features))

           (let ((*fcg-constructions* ,cxn-inventory))
             ,@(remove-key-args keys-and-defs)
             *fcg-constructions*))
         (,creator-fn-name))
       (notify added-fcg-cxn-set) ;; TODO: change notification.
       ,cxn-inventory)))

(defmacro def-fcg-cxn (cxn-name fs &key (cxn-inventory '*fcg-constructions*)
                                (cxn-set 'cxn) (score 0.5) (feature-types nil)
                                (attributes nil) (disable-automatic-footprints nil)
                                (description nil))
  "Make an instance of an FCG Light cxn and add it to cxn-inventory"
  `(multiple-value-bind (list-of-contributing-units list-of-conditional-units)
       (parse-fcg-fs ',fs ',cxn-name (feature-types ,cxn-inventory))
     (let* ((combined-feature-types (inherit-feature-types (feature-types ,cxn-inventory)
                                                           ',feature-types))
            (combined-attributes
             (append (loop with standard-attributes = '((:label . ,cxn-set) (:score . ,score))
                           for attr in ',(parse-attributes attributes) ;;user-defined attributes
                           when (member (first attr) standard-attributes :key #'first)
                           do (setf standard-attributes
                                    (remove (assoc (first attr) standard-attributes)
                                            standard-attributes))
                          finally (return standard-attributes))
                    ',(parse-attributes attributes)))
            (fcg-cxn (make-instance 'fcg-construction
                                    :name ',cxn-name
                                    :contributing-part list-of-contributing-units
                                    :conditional-part list-of-conditional-units
                                    :cxn-inventory ,cxn-inventory
                                    :feature-types combined-feature-types
                                    :attributes combined-attributes
                                    :disable-automatic-footprints ',disable-automatic-footprints
                                    :description ,description))
            ;; Remi 15/08/18: Adding for supporting fcg-dev-tools
            (pathname #+:lispworks (lispworks:current-pathname)
                      #-:lispworks nil))
       (set-data fcg-cxn :cxn-pathname pathname)
       (add-cxn fcg-cxn ,cxn-inventory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing an FCG Light Feature Structure    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-fcg-fs (fs cxn-name feature-types)
  "Parses an fcg-light feature structure and returns a list of contributing-units and a list of
   conditional units (objects). Also throws warnings and errors if needed."
  (let ((list-of-contributing-units nil)
        (list-of-conditional-units nil)
        (unit-type 'contributing))
    (dolist (unit fs)
      (cond
       ((symbolp unit);; For the arrow <-
        (setf unit-type 'conditional))
       ((eql unit-type 'contributing)
        (when (and (not (eql (car unit) 'root)) (not (variable-p (car unit))))
          (warn "The unit-name '~a' in the contributing part of construction '~a' is not a variable. Probably you forgot to add a question mark in front of it." (car unit) cxn-name))
        (push (make-instance 'contributing-unit :name (car unit) :unit-structure (cdr unit)) list-of-contributing-units))
       ((eql unit-type 'conditional)
        (when (and (not (eql (car unit) 'root)) (not (variable-p (car unit))))
          (warn "The unit-name '~a' in the conditional part of construction '~a' is not a variable. Probably you forgot to add a question mark in front of it." (car unit) cxn-name))
        (push (parse-conditional-fs unit feature-types cxn-name (car unit)) list-of-conditional-units))))
    (unless (eql (length list-of-contributing-units) (length (remove-duplicates list-of-contributing-units :key #'name)))
      (warn "In the contributing part of construction '~a', you have specified two units with the same unit name. You probably intended to refer to two different units, so you want to give them unique names." cxn-name))
    (unless (eql (length list-of-conditional-units) (length (remove-duplicates list-of-conditional-units :key #'name)))
      (warn "In the conditional part of construction '~a', you have specified two units with the same unit name. You probably intended to refer to two different units, so you want to give them unique names." cxn-name))
    (values (reverse list-of-contributing-units) (reverse list-of-conditional-units))))

(defun parse-conditional-fs (fs feature-types cxn-name unit-name)
  "parses an fcg light feature structure for a conditional unit,
   returns the conditional unit object"
  (declare (ignorable feature-types cxn-name))
  (let ((list-of-formulation-lock-features nil)
        (list-of-comprehension-lock-features nil)
        (feature-type 'formulation))
    (dolist (feature (cdr fs))
      (cond
       ((eql feature '--)
        (setf feature-type 'comprehension))
       ((eql feature-type 'formulation)
        (push feature list-of-formulation-lock-features))
       ((eql feature-type 'comprehension)
        (push feature list-of-comprehension-lock-features))))
    (make-instance 'conditional-unit
                   :name unit-name
                   :formulation-lock (reverse list-of-formulation-lock-features)
                   :comprehension-lock (reverse list-of-comprehension-lock-features))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-processing-cxn (fcg-cxn)
  "returns the processing cxn for any fcg-light-cxn"
  (cond ((slot-exists-p fcg-cxn 'processing-cxn) ;; Slot was added later in the definition.
         (processing-cxn fcg-cxn))
        ;; Backwards compatability (for grammar images from before 27/06/2018:
        ((eql (type-of (processing-cxn-inventory (cxn-inventory fcg-cxn)))
              'hashed-construction-set)
         (find-cxn (name fcg-cxn) (processing-cxn-inventory (cxn-inventory fcg-cxn))
                   :hash-key (or (attr-val fcg-cxn :string)
                                 (attr-val fcg-cxn :meaning))))
        (t
         (find-cxn (name fcg-cxn) (processing-cxn-inventory (cxn-inventory fcg-cxn))))))

(defun get-original-cxn (processing-cxn)
  "returns the original fcg-cxn for any processing-cxn
   (if it was created using an original fcg-cxn, of course)"
  (cond ((slot-exists-p processing-cxn 'original-cxn)
         (original-cxn processing-cxn))
        ((eql (type-of (original-cxn-set (cxn-inventory processing-cxn))) 'hashed-fcg-construction-set)
         (find-cxn (name processing-cxn) (original-cxn-set (cxn-inventory processing-cxn))
                   :hash-key (or (attr-val processing-cxn :string)
                                 (attr-val processing-cxn :meaning))))
        (t
         (find-cxn (name processing-cxn) (original-cxn-set (cxn-inventory processing-cxn))))))

(defun get-processing-cxn-inventory (fcg-cxn)
  "returns the processing cxn-inventory for any fcg-light-cxn"
  (processing-cxn-inventory (cxn-inventory fcg-cxn)))

(defun get-original-cxn-inventory (processing-cxn)
  "returns the original fcg-cxn-inventory for any processing-cxn
   (if it was created using an original fcg-cxn, of course)"
    (original-cxn-set (cxn-inventory processing-cxn)))

(defun hash-features-only-p (fcg-cxn)
  "test wheter a cxn contains non-hash-features, i.e. whether
   the translation will contain non-j units, except for root
   returns t if fcg-cxn has only hash feature, nil otherwise"
  (let ((cp (conditional-part fcg-cxn))
        (non-hash-feature nil))
    (loop for unit in cp
          until (eq non-hash-feature t)
          do
          (loop for f in (comprehension-lock unit)
                until (eq non-hash-feature t)
                do (unless (string= (first f) 'HASH)
                     (setf non-hash-feature t)))
          (loop for f in (formulation-lock unit)
                until (eq non-hash-feature t)
                do (unless (string= (first f) 'HASH)
                     (setf non-hash-feature t))))
    (if non-hash-feature
      nil
      t)))

(defun check-def-fcg-constructions-keys (keys-and-defs)
  (let ((accepted-keys '(:feature-types :hierarchy-features :fcg-configurations :visualization-configurations
                         :cxn-inventory :disable-automatic-footprints
                         :diagnostics :repairs :hashed :cxn-inventory-type)))
    (dolist (x keys-and-defs)
      (if (keywordp x)
        (unless (member x accepted-keys)
          (error "Unknown keyword ~a. Accepted keywords are: ~a" x accepted-keys))
        (when (and (listp x)
                   (listp (first x)))
          (let ((node-tests (cdr (assoc :node-tests x))))
            (when (and node-tests
                       (member :update-references node-tests)
                       (not (eql (first node-tests) :update-references)))
              (warn "Update-references should ALWAYS be specified as the first node tests in the list. Otherwise :check-duplicates will not function properly."))))))))

(defun find-key-arg (arguments key)
  (loop for (arg . remaining-args) on arguments
        when (eq arg key)
        return (first remaining-args)))

(defun remove-key-arg (arguments key)
  "returns arguments with key and its argument removed, leaves the other elements unaffected"
  (let ((args-to-return nil)
        (previous-symbol-was-key-p nil))
    (loop for arg in arguments
          do
          (cond ((eq key arg)
                 (setf previous-symbol-was-key-p t))
                ((keywordp arg)
                 (setf previous-symbol-was-key-p nil)
                 (push arg args-to-return))
                (t
                 (unless previous-symbol-was-key-p
                   (push arg args-to-return))
                 (setf previous-symbol-was-key-p nil))))
    (reverse args-to-return)))

(defun remove-key-args (arguments)
  (loop for (arg . remaining-args) on arguments by #'cddr
        unless (keywordp arg)
        return (cons arg remaining-args)))

(defgeneric comprehend (utterance &key cxn-inventory silent &allow-other-keys))

(defmethod comprehend (utterance &key (cxn-inventory *fcg-constructions*)  (silent nil))
  "comprehend the input utterance with a given FCG grammar"
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (if processing-cxn-inventory
      (parse utterance processing-cxn-inventory silent)
      (format nil "Failed. ~a does not contain a processing-cxn-inventory" cxn-inventory))))

(defgeneric comprehend-all (utterance &key cxn-inventory silent n &allow-other-keys))

(defmethod comprehend-all (utterance &key (cxn-inventory *fcg-constructions*) (silent nil) (n nil))
  "comprehend the input utterance with a given FCG grammar, obtaining all possible combinations"
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (if processing-cxn-inventory
      (parse-all utterance processing-cxn-inventory :silent silent :n n)
      (format nil "Failed. ~a does not contain a processing-cxn-inventory" cxn-inventory))))



(defgeneric formulate (meaning &key cxn-inventory silent &allow-other-keys))

(defmethod formulate (meaning &key (cxn-inventory *fcg-constructions*) (silent nil))
  "formulate the input meaning in FCG Light"
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (if processing-cxn-inventory
      (produce meaning processing-cxn-inventory silent)
      (format nil "Failed. ~a does not contain a processing-cxn-inventory" cxn-inventory))))

(defgeneric formulate-all (meaning &key cxn-inventory silent n &allow-other-keys))

(defmethod formulate-all (meaning &key (cxn-inventory *fcg-constructions*) (silent nil) (n nil))
  "formulate the input meaning in FCG Light, obtaining all possible combinations"
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (if processing-cxn-inventory
      (produce-all meaning (processing-cxn-inventory cxn-inventory) :silent silent :n n)
      (format nil "Failed. ~a does not contain a processing-cxn-inventory" cxn-inventory))))

(defgeneric comprehend-and-formulate (utterance &key cxn-inventory silent &allow-other-keys))

(defmethod comprehend-and-formulate (utterance &key (cxn-inventory *fcg-constructions*) (silent nil))
  "Comprehend an utterance and formulate it again."
  (let ((meaning (comprehend utterance :cxn-inventory cxn-inventory :silent silent)))
    (when meaning
      (formulate (instantiate-variables meaning)
                 :cxn-inventory cxn-inventory :silent silent))))

(defgeneric formulate-and-comprehend (meaning &key cxn-inventory silent &allow-other-keys))

(defmethod formulate-and-comprehend (meaning &key (cxn-inventory *fcg-constructions*) (silent nil))
  (let ((utterance (formulate meaning :cxn-inventory cxn-inventory :silent silent)))
    (when utterance
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent))))

(defmethod print-object ((cxn fcg-construction) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<cxn ~(~a~):~:_
" (name cxn))
      (format stream " <attributes: ~(~a~)>
" (attributes cxn))
      (format stream " <Contributing Part:")
      (loop for (element . rest) on (contributing-part cxn)
            collect
            (format stream "
  <unit ~(~a~):~:_   unit-structure: ~a~:_>" (name element) (unit-structure element))
            when rest collect (format nil ""))
      (format stream ">")
      (format stream "
 <Conditional Part:")
      (loop for (element . rest) on (conditional-part cxn)
            collect
            (format stream "
  <unit ~(~a~):~:_   formulation-lock: ~a~:_   comprehension-lock: ~a~:_>" (name element) (formulation-lock element) (comprehension-lock element))
            when rest collect (format nil ""))
      (format stream ">")
      (format stream ">"))
    (format stream "<cxn: ~a>" (name cxn))))

;; ---------------------------------------------------------------------------
;; Classes, macros and definitions for backwards compatability:

(defclass fcg-light-construction (fcg-construction) ()
  (:documentation "Depreciated class for ensuring backwards compatibility."))

(defmethod print-object ((cxn fcg-light-construction) stream)
  (call-next-method))

(defclass fcg-light-construction-set (fcg-construction-set) ()
  (:documentation "Depreciated class for backwards compatibility."))

(defmacro def-fcg-light-constructions (&body body)
  "Depreciated: Macro that is kept here for backwards compatibility."
  (warn "Depreciated macro, please use def-fcg-constructions")
  `(def-fcg-constructions ,@body))

(defmacro def-fcg-light-cxn (&body body)
  "Depreciated: macro that is kept for backwards compatibility issues."
  (warn "Depreciated macro, please use def-fcg-cxn")
  `(def-fcg-cxn ,@body))

(defmacro parse-fcg-light-fs (&body body)
  `(parse-fcg-fs ,@body))

;; ---------------------------------------------------------------------------

(defun eval-when-bound (sexp)
  "evaluates sexp, and if it is an unbound atom, doesn't evaluate it"
  (if (and (atom sexp)
            (not (boundp sexp)))
    sexp
    (eval sexp)))

;; Some additional interface utilities.
;; ---------------------------------------------------------------------
(export '(word-in-dictionary-p
          fcg-find-cxn fcg-show-cxn
          hashed-cxn-inventory-p
          pp-configuration pp-visualization-configuration
          pprint-configuration pprint-visualization-configuration
          ))

(defun word-in-dictionary-p (string &optional (cxn-inventory *fcg-constructions*))
  "Checks whether a string is covered by the grammar. Only for hashed construction-sets."
  (gethash string (constructions-hash-table cxn-inventory)))

(defun hashed-cxn-inventory-p (cxn-inventory)
  (slot-exists-p cxn-inventory 'constructions-hash-table))

(defun fcg-find-cxn (name &optional (cxn-inventory *fcg-constructions*) (listify? nil))
  "If no construction with the same name is found, compatible-named cxns will be returned." 
  (labels ((find-all-matches (constructions)
             (find-all (symbol-name name) constructions
                       :key #'(lambda(cxn)
                                (symbol-name (name cxn)))
                       :test #'(lambda(x y)
                                 (cl-ppcre::scan x y)))))
    (let ((solutions nil))
      (if (hashed-cxn-inventory-p cxn-inventory)
        ;; We iterate through the hash table of constructions.
        (with-hash-table-iterator (my-iterator (constructions-hash-table cxn-inventory))
          ;; The iterator always returns three values.
          (loop (multiple-value-bind (entry-p key value)
                    (my-iterator)
                  (declare (ignore key))
                  ;; Now we add new matches to the solutions
                  (if entry-p
                    (let ((the-cxn (find name value :key #'name)))
                      (if the-cxn
                        (return (setf solutions (list the-cxn)))
                        (let ((matching-cxns (find-all-matches value)))
                          (loop for cxn in matching-cxns
                                do (pushnew cxn solutions)))))
                    (return)))))
        ;; If we have a classic inventory it is simpler.
        (setf solutions (listify (or (find name (constructions cxn-inventory) :key #'name)
                                     (find-all-matches (constructions cxn-inventory))))))
      ;; Now return the result:
      (if (and (null (rest solutions)) (not listify?))
        (first solutions)
        solutions))))

(defun fcg-show-cxn (name &optional (cxn-inventory *fcg-constructions*))
    (let ((cxns (fcg-find-cxn name cxn-inventory t)))
      (if (null cxns)
        (add-element
         (make-html (format nil "Could not find constructions matching ~a" name)))
        (progn
          (add-element (make-html "Found:"))
          (dolist (cxn cxns)
            (add-element (make-html cxn)))))))

(defun pprint-configuration (object-with-configuration)
  (pprint (configuration object-with-configuration)))

(defun pp-configuration (object-with-configuration)
  (pp (configuration object-with-configuration)))

(defun pprint-visualization-configuration (object-with-visualization-configuration)
  (pprint (visualization-configuration object-with-visualization-configuration)))

(defun pp-visualization-configuration (object-with-visualization-configuration)
  (pp (visualization-configuration object-with-visualization-configuration)))

