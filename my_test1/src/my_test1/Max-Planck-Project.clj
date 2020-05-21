;(test_sm Xvector Yvector MassVec)
(ns my-test1)

;начало блока удаления данных из массива 
(defn index-exclude [r ex] 
  "Take all indices execpted ex" 
  (filter #(not (ex %)) (range r))) 
(defn dissoc-idx [v & ds]
  (map v (index-exclude (count v) (into #{} ds))))
; конец блока

;Нахождение индекса указанного значения
(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll)
  )

;Нахождение и удаление первого "0"
(defn findAndDelZero [inpVec]
  (dissoc-idx
    inpVec
    (first (indices zero? inpVec))
    )
  )

;Нахождение индекса минимального значения
(defn findIndxOfMin [inpVec]
  (.indexOf 
    inpVec 
    (apply min (findAndDelZero inpVec))
    )
  )

;вычитание значений в скобках и возведение их содержимого в квадрат
(defn x1-x2Sqr[inpVec]
  (map (fn [x] (map (fn [y] (Math/pow (- x y) 2)) inpVec))inpVec)
  )

; Вычисление радиуса шарика из его массы и плотности
; Calculation of the ball radius from its mass and density
; m_ball - mass 
; p_ball - density
(defn BallRadiusCalculation [m_ball p_ball]
  (Math/pow
    (/(/ m_ball p_ball)(/ (* 4 (Math/PI)) 3))
    (/ 1 3)
    )
  )

; Вычисление силы взаимодействия шариков используя потенциал Ленарда-Джонса
; Calculation of the force of interaction of balls using the Lenard-Jones potential
; e - the depth of the potential yawner
; r - the distance between the centers of the particles
; q - the distance at which the interaction energy becomes zero
(defn LennardJonesPower [e r q]
  (if (<= r (* 2.5 q))
    ;(-
    (* (/ (* 12 e) q)(-(Math/pow (/ q r) 13)(Math/pow (/ q r) 7)))
    ;(* (/ (* 12 e) q)(-(Math/pow (/ q (* 2.5 q)) 13)(Math/pow (/ q (* 2.5 q)) 7)))
    ; )
    0
    )
  )

; Вычисление ускорения учитывая силу взаимодействия и массы шарика
; Calculation of speed-up given the strength of the interaction and the mass of the ball
(defn Speed_up [m_ball e r q]
  (/ (LennardJonesPower e r q) m_ball)
  )

; Вычисление скорости
(defn Speed [t_step v_current m_ball e r q]
  ( + v_current (* (Speed_up m_ball e r q) t_step))
  )

; Вычисление координаты Х 
(defn X [t_step x_current m_ball v_current e r q]
  
  (+
    (+ x_current (* (Speed t_step v_current m_ball e r q)t_step))
    (* (Speed_up m_ball e r q) (/(Math/pow t_step 2)2))
    )
  )

; Вычисление координаты Y 
(defn Y [t_step y_current m_ball v_current e r q]
  
  (+
    (+ y_current (* (Speed t_step v_current m_ball e r q)t_step))
    (* (Speed_up m_ball e r q) (/(Math/pow t_step 2)2))
    )
  )

;
;;;
(defn distOut [inX inY]
  (def A [inX inY])
  (Math/sqrt(apply min(findAndDelZero (vec(map #(apply + %)(apply map vector A))))))
  )
;
;;;
(defn newFN [inData]
  
  (def particleData {:coordX [13 20 15 18]
                     :coordY [0 12 7 4]
                     :massPart [5 5 5 5]
                     :dist [100 100 100 100]
                     :indecs[0 0 0 0] 
                     })
  
  (while (> (apply max (particleData :dist)) 3)
    
    (doseq [[x y i] (map list (x1-x2Sqr(vec(particleData :coordX))) 
                         (x1-x2Sqr(vec(particleData :coordY)))
                         (range (count(particleData :coordX)))
                         )
            ]
      
      (def particleData (assoc particleData :dist  (assoc (particleData :dist) i (distOut x y))
                               :indecs (assoc (particleData :indecs) i (findIndxOfMin (vec (map + x y))))))
      
      (if (> ((particleData :dist)i) 5)
        (def particleData (assoc particleData :coordX  (assoc (particleData :coordX) i (X  
                                                                                         (get inData 0) (get (particleData :coordX) i) (get (particleData :massPart) i) 
                                                                                         (get inData 1) (get inData 2) (get (particleData :dist) i) (get inData 3)
                                                                                         ))
                                 :coordY (assoc (particleData :coordY) i (Y  
                                                                           (get inData 0) (get (particleData :coordY) i) (get (particleData :massPart) i) 
                                                                           (get inData 1) (get inData 2) (get (particleData :dist) i) (get inData 3)
                                                                           ))
                                 ))
        
        )
      
      )
    
    )
  
  )
;(newFN [1 0 2 5]) 