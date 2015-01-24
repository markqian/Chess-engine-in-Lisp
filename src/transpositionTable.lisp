
(defclass TranspositionTable ()
 ((maxSize    :accessor maxSize    :initarg :maxSize    :initform  5000000)
  (data    :accessor data    :initarg :data    :initform (make-hash-table))
  (krono    :accessor krono    :initarg :krono    :initform '())
  (killer1    :accessor killer1    :initarg :killer1    :initform (make-array '80 :initla-element -1))
  (killer2    :accessor killer2    :initarg :killer2    :initform (make-array '80 :initla-element -1))
  (hashmove    :accessor hashmove    :initarg :hashmove    :initform (make-array '80 :initla-element -1))
  (butterfly    :accessor butterfly    :initarg :butterfly    :initform (make-array '4096 :initla-element 0))))

