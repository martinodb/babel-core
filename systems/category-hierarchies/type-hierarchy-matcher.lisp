(in-package :fcg)

;; If it turns out that people use the type-hierarchies,
;; this function should become the standard one in systems/fcg

(defun unify-atom (x y bindings &key cxn-inventory)
  "unify-atom function for use with type-hierarchies"
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)) bindings))
        ;; unify symbols on type-hierarchy-basis
        ((and cxn-inventory
              (type-hierarchies::get-type-hierarchy cxn-inventory)
              (symbolp x) (symbolp y)
              (type-hierarchies::node-p (intern (symbol-name x) :type-hierarchies)
                                        (type-hierarchies:get-type-hierarchy cxn-inventory))
              (type-hierarchies::node-p (intern (symbol-name y) :type-hierarchies)
                                        (type-hierarchies:get-type-hierarchy cxn-inventory))
              (type-hierarchies::directed-path-p (intern (symbol-name y) :type-hierarchies)
                                                 (intern (symbol-name x) :type-hierarchies)
                                                 (type-hierarchies:get-type-hierarchy cxn-inventory))
              bindings))
	;; unify variables
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((unify-equal x y) bindings)
	(t (values +fail+ x y))))

(define-event-handler (trace-fcg cip-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (children (top-node cip))
             "Computing next solution for application of "
             "Applying ")
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size (construction-inventory cip)) (get-configuration (construction-inventory cip) 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (get-construction-inventory-title-string (original-cxn-set (construction-inventory cip)))
                (size (original-cxn-set (construction-inventory cip))))
     (make-html (original-cxn-set (construction-inventory cip))))
     #+:type-hierarchies ,(if (type-hierarchies::get-type-hierarchy (original-cxn-set (construction-inventory cip)))
                            (make-html (type-hierarchies::get-type-hierarchy (original-cxn-set (construction-inventory cip)))
                                       :weights? t :render-program "circo")
                           "")
     " in "
     ,(if (eq (direction cip) '->) "formulation" "comprehension"))))