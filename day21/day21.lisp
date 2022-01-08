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

(defmethod player ((game game) player-num)
  (funcall (slot-value game 'player) player-num))

(defclass player ()
  ((position :reader player-position :initarg :position :initform (error "position is required"))
   (score :reader score :initform 0)))

(defmethod roll ((game game) &optional (times 1))
  (loop for i from 1 to times
        sum (die game)
        do (setf (slot-value game 'die)
                 (1+ (mod (die game)
                          100))

                 (slot-value game 'roll-count)
                 (1+ (roll-count game)))))

(defmethod move ((player player) spaces)
  (setf (slot-value player 'position)
        (1+ (mod (+ (player-position player)
                    (1- spaces))
                 10))))

(defmethod update-score ((player player))
  (incf (slot-value player 'score) (player-position player)))

(defmethod play-turn ((game game) player-num)
  (let ((roll (roll game 3))
        (player (player game player-num)))
    (move player roll)
    (update-score player)))

(defmethod dump ((game game))
  (describe game)
  (describe (player game :one))
  (describe (player game :two)))

(defmethod play-game ((game game))
  (loop for (current-player other-player) on '#1=(:one :two . #1#)
        do (play-turn game current-player)
        until (>= (score (player game current-player)) (winning-score game))
        finally (return (values (score (player game other-player)) (roll-count game)))))

(defun calculate-part-1 (player-one-position player-two-position)
  (let ((game (make-instance 'game :player-one-position player-one-position
                                   :player-two-position player-two-position
                                   :winning-score 1000)))
    (multiple-value-bind (losing-score roll-count) (play-game game)
      (* losing-score roll-count))))
