(defpackage aoc2021.day21
  (:use #:cl))

(in-package :aoc2021.day21)

(defclass game ()
  ((die :reader die :initform 1)
   (roll-count :reader roll-count :initform 0)
   (winning-score :reader winning-score :initarg :winning-score :initform (error "winning-score is required"))
   (player)))

(defmethod initialize-instance :after ((game game) &key player-one-position player-two-position)
  (let ((player-one (make-instance 'player :position player-one-position))
        (player-two (make-instance 'player :position player-two-position)))
    (setf (slot-value game 'player)
          (lambda (number)
            (ccase number
              (:one player-one)
              (:two player-two))))))

(defmethod copy-game ((game game))
  (let ((new-game (make-instance 'game :player-one-position (player-position (player game :one))
                                       :player-two-position (player-position (player game :two))
                                       :winning-score (winning-score game))))
    (setf (slot-value new-game 'roll-count) (roll-count game)
          (slot-value (player new-game :one) 'score) (score (player game :one))
          (slot-value (player new-game :two) 'score) (score (player game :two)))
    new-game))

(defmethod player ((game game) player-num)
  (funcall (slot-value game 'player) player-num))

(defclass player ()
  ((position :reader player-position :initarg :position :initform (error "position is required"))
   (score :reader score :initform 0)))

(defmethod deterministic-roll ((game game) &optional (times 1))
  (loop for i from 1 to times
        sum (die game)
        do (setf (slot-value game 'die)
                 (1+ (mod (die game)
                          100))

                 (slot-value game 'roll-count)
                 (1+ (roll-count game)))))

(defmethod quantum-roll ((game game) player-num &key (sides 3) (times 1) (sum 0))
  (if (zerop times)
      (play-quantum-game game player-num sum)
      (loop for i from 1 to sides
            collecting (quantum-roll game player-num
                                     :sides sides
                                     :times (1- times)
                                     :sum (+ sum i))
              into results
            finally (return (reduce (lambda (a b)
                                      (list (+ (first a) (first b))
                                            (+ (second a) (second b))))
                                    results)))))

(defun switch-player (player-num)
  (case player-num
    (:one :two)
    (:two :one)))

(defmethod play-quantum-game ((game game) &optional (player-num :two) spaces)
  (let ((game (copy-game game)))
    (flet ((make-wins (player-num)
             (ccase player-num
               (:one (list 1 0))
               (:two (list 0 1)))))
      (let ((player (player game player-num)))
        (when spaces
          (move player spaces)
          (update-score player))
        (let ((wins (cache-hit game player-num)))
          (cond
            (wins)
            ((>= (score player) (winning-score game))
             (make-wins player-num))
            (t
             (cache-wins game
                         player-num
                         (quantum-roll game
                                       (switch-player player-num)
                                       :times 3)))))))))

(defvar *win-cache* (make-hash-table :test 'equal))

(defun make-cache-key (game player-num)
  (let ((player-one (player game :one))
        (player-two (player game :two)))
    (list
     (winning-score game)
     player-num
     (player-position player-one)
     (score player-one)
     (player-position player-two)
     (score player-two))))

(defun cache-wins (game player-num wins)
  (setf (gethash (make-cache-key game player-num) *win-cache*) wins))

(defun cache-hit (game player-num)
  (gethash (make-cache-key game player-num) *win-cache*))

(defmethod move ((player player) spaces)
  (setf (slot-value player 'position)
        (1+ (mod (+ (player-position player)
                    (1- spaces))
                 10))))

(defmethod update-score ((player player))
  (incf (slot-value player 'score) (player-position player)))

(defmethod play-turn ((game game) player-num)
  (let ((spaces (deterministic-roll game 3))
        (player (player game player-num)))
    (move player spaces)
    (update-score player)))

(defmethod dump ((game game))
  (describe game)
  (describe (player game :one))
  (describe (player game :two)))

(defmethod play-deterministic-game ((game game))
  (loop for (current-player other-player) on '#1=(:one :two . #1#)
        do (play-turn game current-player)
        until (>= (score (player game current-player)) (winning-score game))
        finally (return (values (score (player game other-player))
                                (roll-count game)))))

(defun calculate-part-1 (player-one-position player-two-position)
  (let ((game (make-instance 'game :player-one-position player-one-position
                                   :player-two-position player-two-position
                                   :winning-score 1000)))
    (multiple-value-bind (losing-score roll-count) (play-deterministic-game game)
      (* losing-score roll-count))))

(defun calculate-part-2 (player-one-position player-two-position)
  (let ((game (make-instance 'game :player-one-position player-one-position
                                   :player-two-position player-two-position
                                   :winning-score 21)))
    (apply #'max (play-quantum-game game))))
