(in-package :octopus)

(defun draw (balls)
  (pal:with-pal (:width 700 :height 350 :title "papiez")
    (pal:event-loop ()
      (pal:clear-screen (make-instance 'pal:color))
      (setf balls (next-state balls))
      (mapcar 'draw-ball balls)
      (sleep 1/30))))

(defun draw-ball (ball)
  ;(format t "~A ~A~%" (getf (direction-of ball) :y) (y-of ball))
  (pal:draw-circle (pal:v (ceiling (getf (position-of ball) :x))
                          (ceiling (getf (position-of ball) :y)))
                   (radius-of ball)
                   255 255 255 255))

(defun next-state (balls)
  (mapcar (lambda (b) (collision-between b balls)) (mapcar 'next-position balls)))

(defun collidingp (ball1 ball2)
  (< (distance-between ball1 ball2) (sum-of-radius ball1 ball2)))

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

(defun collision-between (ball balls)
  (mapcar (lambda (b) (if (and (not (eq ball b)) (collidingp ball b))
                          (on-collision ball b))) balls)
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

(defun normalize-vector (v1)
  (let ((len (vector-length v1)))
    (if (/= len 0)
        (multiply-vector v1 (/ 1 len))
        v1)))

(defun substract-vectors (v1 v2)
  (add-vectors v1 (multiply-vector v2 -1)))

(defun upper (x)
  (if (>= x 0)
      x
      0))

(defun on-collision (b1 b2)
  (multiple-value-bind (dist vec) (distance-between b1 b2)
    (let ((im1 (radius-of b1))
          (im2 (radius-of b2)))
    (setf (direction-of b1) (multiply-vector (add-vectors (direction-of b1) vec) (/ 1 (vector-length (add-vectors (direction-of b1) vec))))
          (v-of b1) (/ (+ (* (v-of b1) (- im1 im2)) (* 2 im2 (v-of b2)))
                       (+ im1 im2))))))
;          (position-of b1) (add-vectors (position-of b1) (multiply-vector vec (/ im1 (+ im1 im2)))))))))

(defun next-position (ball)
  (let* ((x (getf (position-of ball) :x))
        (y (getf (position-of ball) :y))
        (v (v-of ball))
        (r (radius-of ball))
        (direction (direction-of ball))
        (nx (+ (* (getf direction :x) v) x))
        (ny (+ (* (getf direction :y) v) y)))
    (if (> (abs v) 0.01)
        (progn
          (if (> v 0) (setf (v-of ball) (- v 0.09)))
          (if (< v 0) (setf (v-of ball) (+ v 0.09)))
          (if (or (<= (- ny r) 0) (>= (+ ny r) 350))
              (setf (getf direction :y) (- (getf direction :y))
                    (getf (position-of ball) :x) (+ (* (getf direction :x) v) x)
                    (getf (position-of ball) :y) (+ (* (getf direction :y) v) y))
              (if (or (<= (- nx r) 0) (>= (+ nx r) 700))
                  (setf (getf direction :x) (- (getf direction :x))
                        (getf (position-of ball) :x) (+ (* (getf direction :x) v) x)
                        (getf (position-of ball) :y) (+ (* (getf direction :y) v) y))
                  (setf (getf (position-of ball) :x) nx
                        (getf (position-of ball) :y) ny)))))
        ball))


(defun make-balls (n)
  (map 'list (lambda (data) (apply 'make-instance (cons 'ball data)))
       (loop for i from 0 to n collect
            `(:v ,(random 10)  :position (:x ,(random 700) :y ,(random 350)) :radius ,(random 20) :direction ,(root-of-unity (random 36))))))


