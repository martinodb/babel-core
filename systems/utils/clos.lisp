(in-package :utils)

(export '(set-clos-slot get-clos-slot))

(defun set-clos-slot (instance slot value)
  "Set a clos-slot of an instance of a certain class to a given value"
  #+lispworks (setf (c2mop:slot-value-using-class (find-class (type-of instance))
                                     instance
                                     slot)
                    value)
  #+ccl (setf (slot-value instance slot) value))

(defun get-clos-slot (instance slot)
  "Get the clos-slot of an instance of a certain class"
  (when  (and (slot-exists-p instance slot)
              (slot-boundp instance slot))
    #+lispworks (c2mop:slot-value-using-class (find-class (type-of instance))
                                              instance
                                              slot)
    #+ccl (slot-value instance slot)))