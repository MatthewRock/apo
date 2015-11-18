;;;; apo.lisp
(defmacro construct (class &rest initlist)
  `(make-instance (quote ,class) ,@initlist))

(defparameter *current-table* 0)

(let ((table-list nil))
  (defun in-list (elem)
    (assoc elem table-list))
  (defun get-list ()
    table-list)
  (defun add-to-list (elem)
    (unless (in-list elem) (push (cons elem (construct table)) table-list)))
  (defun remove-from-list (elem)
    (setf table-list (set-difference table-list (list (assoc elem table-list)))))
  (defun clear-list ()
    (setf table-list nil))
  (defun get-element (identifier)
    (cdr (assoc identifier table-list))))

(let ((counter 0))
  (defun get-new-number ()
    (incf counter))
  (defun reset-counter ()
    (setf counter 0)))

(defclass meal ()
  ((cost :accessor cost :initarg :cost :initform 0 :type fixnum)
   (components :accessor components :initform nil)
   (description :accessor description :initform "" :initarg :description)))

(defmethod print-object ((object meal) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (cost description) object
      (format stream "~&~3tCost: ~A~&~3tDescription: ~A~%" cost description))))

(defclass table()
  ((ordered-meals :initarg :ordered-meals :initform nil :accessor ordered-meals)
   (sum-to-pay :initform 0 :accessor sum-to-pay)
   (realised-meals :initarg :realised-meals :initform nil :accessor realised-meals)))

(defmethod initialize-instance :after ((obj table) &key)
  (setf (sum-to-pay obj) (loop for meal in (ordered-meals obj) summing (cost meal))))

(defgeneric select-table ()
  (:documentation "Select table from TABLE-LIST, a global object."))
(defgeneric register-table (identifier)
  (:documentation "Register table identified by IDENTIFIER in TABLE-LIST as seated."))
(defgeneric list-menu (menu)
  (:documentation "Show menu items"))
(defgeneric order-meal (table meal kitchen)
  (:documentation "Order MEAL for TABLE, but first ask KITCHEN if it's available"))
(defgeneric compose-meal (table meal kitchen)
  (:documentation "Order MEAL for TABLE by creating it, but first ask KITCHEN if it's available"))
(defgeneric drop-table (identifier)
  (:documentation "Close table identified by IDENTIFIER, finishing servicing it."))
(defgeneric pause-servicing ()
  (:documentation "Halt any operations on table without processing requests."))
(defgeneric stop-service (identifier)
  (:documentation "Halt any operations on table, but send operation to kitchen."))
(defgeneric add-comment (thing message)
  (:documentation "Adds comment MESSAGE to the THING"))
(defgeneric send-order (tableid kitchen)
  (:documentation "Send order from TABLE to KITCHEN for realization"))
(defgeneric order-bill (tableid)
  (:documentation "Order BILL for TABLE"))

(defmethod register-table (identifier)
  (add-to-list identifier)
  (format t "Registered table ~D!~%" identifier))

(defmethod select-table ()
  (if (not (get-list)) (progn
                         (print "No table registered!")
                         (return-from select-table)))
  (format t "~&Available tables: ~A~%" (get-list))
  (loop for input = (progn (format t "~&Select table: ") (read))
     if (not (in-list input))
     do (format t "~&Table ~A does not exist. Try again. Available tables: ~A~%Select table: "
                input (get-list))
     else do
       (setf *current-table* input)
       (return))
  (list-menu *global-menu*))

(defmethod list-menu ((menu list))
  (format t "~&Today's menu: ~A~%" menu)
  (print "What would you like to do?")
  (print "1. Order meal")
  (print "2. Compose meal")
  (print "3. Stop for now, come back later")
  (print "4. Stop and send order to kitchen")
  (print "5. Print ordered meals.")
  (print "6. Finish servicing table and close it.")
  (loop for input = (progn (print "Your input: ")(read))
     if (not (member input '(1 2 3 4 5 6))) do (print "Wrong input. Your input: ")
     else do
       (progn
         (case input
           (1 (order-meal *current-table* (car menu) nil))
           (2 (compose-meal *current-table* (car menu) nil))
           (3 (return-from list-menu (pause-servicing)))
           (4 (return-from list-menu (stop-service *current-table*)))
           (5 (print-ordered-meals *current-table*))
           (6 (return-from list-menu (drop-table *current-table*)))))))

(defmethod drop-table (identifier)
  (order-bill identifier)
  (remove-from-list identifier))

(defmethod send-order (tableid kitchen)
  t)

(defmethod stop-service (tableid)
  (pause-servicing))

(defmethod print-ordered-meals (tableid)
  (print "Ordered meals:")
  (format t "~&~A~%Worth: ~D~%" (ordered-meals (get-element tableid))
          (loop for x in (ordered-meals (get-element tableid)) sum 10)))

(defmethod stop-service :before (tableid)
  (when (y-or-n-p "Do you want to order the bill?") (order-bill tableid))
  (when (y-or-n-p "Do you want to add comment?") (add-comment nil (read))))

(defmethod order-bill (tableid)
  (print "Bill ordered"))

(defmethod add-comment (thing message)
  (format t "~&Added comment ~A!~%" message))

(defmethod pause-servicing ()
  (print "Paused servicing!"))

(defmethod order-meal (tableid (meal meal) kitchen)
  (print "Ordered meal.")
  (send-order tableid kitchen)
  (push meal (ordered-meals (get-element tableid))))

(defmethod compose-meal (tableid (meal meal) kitchen)
  (print "Composed meal.")
  (order-meal tableid meal kitchen)); TODO: Implement composing meal.

(defvar *example-meal* (construct meal :cost 10 :description "Zupa pomidorowa"))
(defvar *global-menu* (list *example-meal*))

(defun main ()
  (loop for x = (main-loop)
       if (eql x 'exit) do (return-from main 'success)))

(defun main-loop ()
  (format t "~&What do you want to do?")
  (print "1. Select existing table")
  (print "2. Register new table")
  (print "3. Exit")
  (loop for input = (progn (print "Your selection: ") (read))
       if (not (member input '(1 2 3))) do
       (format t "~&Wrong input. Try again: ")
     else do
       (case input
         (1 (return-from main-loop (select-table)))
         (2 (return-from main-loop (register-table (get-new-number))))
         (3 (return-from main-loop 'exit)))))

(defun test-reset ()
  (reset-counter)
  (clear-list))
