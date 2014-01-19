(in-package :octopus)

(defun next-state (balls)
  (mapcar (lambda (b) (collision-between b balls)) (mapcar 'next-position balls)))

(defun collidingp (ball1 ball2)
  (< (distance-between ball1 ball2)(sum-of-radius ball1 ball2)))

(defun distance-between (b1 b2)
  (distance `(:x ,(getf (position-of b1) :x) :y ,(getf (position-of b1) :y))
            `(:x ,(getf (position-of b2) :x) :y ,(getf (position-of b2) :y))))

(defun distance (p1 p2)
  (let* ((x1 (getf p1 :x))
         (x2 (getf p2 :x))
         (y1 (getf p1 :y))
         (y2 (getf p2 :y))
         (diffx (- x1 x2))
         (diffy (- y1 y2)))
    (values (sqrt (+ (* diffx diffx) (* diffy diffy))) (list :x diffx :y diffy))))

(defun sum-of-radius (b1 b2)
  (+ (radius-of b1) (radius-of b2)))

(defun collision-between (ball balls &key (mangle-first t))
  (mapcar (lambda (b) (if (and (not (eq ball b)) (collidingp ball b))
                          (on-collision ball b mangle-first))) balls )
  ball)

(defun multiply-vector (v s)
  `(:x ,(* (getf v :x) s) :y ,(* (getf v :y) s)))

(defun add-vectors (v s)
  (list :x (+ (getf v :x) (getf s :x)) :y (+ (getf v :y) (getf s :y))))

(defun square (x)
  (* x x))

(defun vector-length (v)
  (sqrt (+ (square (getf v :x)) (square (getf v :y)))))

(defun vector-dot (v1 v2)
  (+ (* (getf v1 :x) (getf v2 :x)(* (getf v1 :y) (getf v2 :y)))))

(defun normalize-vector (v1 &key (unit 1))
  (let ((len (vector-length v1)))
    (if (/= len 0)
        (multiply-vector v1 (/ unit len))
        v1)))

(defun substract-vectors (v1 v2)
  (add-vectors v1 (multiply-vector v2 -1)))

;xDD
(defun upper (x)
  (max x 0))

(defun on-collision(b1 b2 manglefirst)
  (multiple-value-bind (dist vec) (distance-between b1 b2)
    (let ((im1 (radius-of b1))
          (im2 (radius-of b2)))
      (if manglefirst
          (setf (direction-of b1) (multiply-vector (add-vectors (direction-of b1) vec) (/ 1 (vector-length (add-vectors (direction-of b1) vec))))
                (v-of b1) (v-of b2)))
          (multiple-value-bind (x dist) (distance (position-of b1) (position-of b2))
            (if (< x (sum-of-radius b1 b2))
                (let* ((diff (- (sum-of-radius b1 b2) x))
                       (nrm (normalize-vector dist :unit diff)))
                  (setf (position-of b1) (add-vectors (position-of b1) nrm))))))))


(defun next-position (ball balls)
  (let* ((x (getf (position-of ball) :x))
         (y (getf (position-of ball) :y))
         (v (v-of ball))
         (r (radius-of ball))
         (direction (direction-of ball))
         (nx (+ (* (getf direction :x) v) x))
         (ny (abs (mod (+ (* (getf direction :y) v) y) 350))))
    (if (> (abs v) 0.1)
        (progn
          (if (> v 0) (setf (v-of ball) (- v 0.09)))
          (if (< v 0) (setf (v-of ball) (+ v 0.09)))
              (if (or (<= (- nx r) 0) (>= (+ nx r) 700))
                  (setf (getf direction :x) (- (getf direction :x))
                        (getf (position-of ball) :x) (if (<= (- nx r) 0) r (- 700 r)))
                  (setf (getf (position-of ball) :x) nx
                        (getf (position-of ball) :y) ny))))
    ball))

(defun make-balls (n)
  (map 'list (lambda (data) (apply 'make-instance (cons 'ball data)))
       (loop for i from 0 to n collect
            `(:v ,(random 10)  :position (:x ,(random 700) :y ,(random 350)) :radius ,(random 20) :direction ,(root-of-unity (random 36))))))


