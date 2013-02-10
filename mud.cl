; describe locations
(defparameter *nodes* '((living-room (you are in the living-room. 
                                      a wizard is asleep on the couch.))
                        (garden (you are in a beautiful garden. 
                                 there is a well in front of you.))
                        (attic (you are in the attic. 
                                there is a giant welding torch in the corner))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

; describe connections between locatios
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; describe the objects
(defparameter *objects* '(whiskey bucket frog chain))

; and their locations
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; find objects in a given location
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
              (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

; describe the objects found
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
      `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))

; delcare the initial location of the user
(defparameter *location* 'living-room)

; the user's look command
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))


; walking
(defun walk (direction)
  (let ((next (find direction
                   (cdr (assoc *location* *edges*))
                   :key #'cadr)))
  (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go tat way.))))

; pickup items
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
           `(you are now carrying the ,object))
         (t '(you cannot get that.))))

; check inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))


; custom repl
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
              (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))