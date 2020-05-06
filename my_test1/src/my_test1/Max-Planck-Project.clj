;(test_sm Xvector Yvector MassVector)
(ns my-test1)
(def Xvector (into-array Double/TYPE [80 20 45 36 16 78 38 15 10 17]))
(def Yvector (into-array Double/TYPE [00 12 14 45 22 10 41 22 14 14]))
(def MassVector (into-array Double/TYPE [0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]))

;начало блока удаления данных из массива 
(defn index-exclude [r ex] 
  "Take all indices execpted ex" 
  (filter #(not (ex %)) (range r))) 
(defn dissoc-idx [v & ds]
  (map v (index-exclude (count v) (into #{} ds))))
; конец блока

;Реализация цикла for 
(defmacro for-loop [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#))
  )

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

;Из исходного вектора извлечь только расстояния между частицам из них найти минимальное и вернуть его индекс
(defn findMinDistIndex [tempVec]
  
  (def tempVec_2 (into-array (repeat (/(count tempVec)3) 0.)))
  
  (for-loop [i 0 (< i (count (vec tempVec_2))) (inc i)]
            (aset tempVec_2 i  (double (get tempVec (+(* i 3)2))))
            )
  
  (.indexOf (vec tempVec_2) (apply min (vec tempVec_2)))
  )

;Нахождение минимального расстояния между шариками
(defn DistanceBetweenBalls [inpVectorX inpVectorY]  
  (Math/sqrt
    (apply min
           (findAndDelZero (vec (map + inpVectorX inpVectorY)))           
           )
    )
  )

;определение растояний от текущей частицы до всех остальных
(defn GetDist [Xvector Yvector]
  
  (def tempVec (into-array (repeat (*(count Xvector)3) 0.)))
  
  (for-loop [i 0 (< i (count Xvector)) (inc i)] 
            
            (aset tempVec (* i 3) (double i))
            
            (aset tempVec (+(* i 3)1)
                  (double
                    (findIndxOfMin 
                      (vec (map + ((vec (x1-x2Sqr Xvector))i) ((vec (x1-x2Sqr Yvector))i)))
                      )
                    )
                  )
            
            (aset tempVec (+(* i 3)2)
                  (DistanceBetweenBalls 
                    ((vec (x1-x2Sqr Xvector))i) 
                    ((vec (x1-x2Sqr Yvector))i) 
                    )
                  )
            )
  
  (seq tempVec)
  )

(defn sm [tempX currBallIDX]
  
  (def tm (into-array Double/TYPE (range 0 (-(count tempX)2))))
  
  (for-loop [i 0 (< i (count tm)) (inc i)]
            (for-loop [k 0 (< k (count tempX)) (inc k)] 
                      
                      (if (and (> k (get currBallIDX 0)) (< k (get currBallIDX 1)))
                        (aset tm (- k 1) (get tempX k))
                        )
                      (if (> k (get currBallIDX 1)) 
                        (aset tm (- k 2) (get tempX k))
                        )
                      (if (< k (get currBallIDX 0)) 
                        (aset tm k (get tempX k))
                        )
                      
                      )
            )
  (seq tm)
  )
;
;
;
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
    (-
      (* (/ (* 12 e) q)
         (-(Math/pow (/ q r) 13)(Math/pow (/ q r) 7))
         )
      
      (* (/ (* 12 e) q)
         (-(Math/pow (/ q (* 2.5 q)) 13)(Math/pow (/ q (* 2.5 q)) 7))
         )
      )
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



;временная функция для тестирования и отладки других функций
(defn test_sm [Xvector Yvector MassVector]
  
  (def currBallIDX (into-array [0.0 0.0]))
  
  (for-loop [i 0 (< i (/(count Xvector)2)) (inc i)] 
            
            (if(= i 0)
              (def newX (into-array Xvector))
              (def newX (into-array (sm newX currBallIDX)))
              )
            (if(= i 0)
              (def newY (into-array Yvector))
              (def newY (into-array (sm newY currBallIDX)))
              )
            (if(= i 0)
              (def newMass (into-array MassVector))
              (def newMass (into-array (sm newMass currBallIDX)))
              )
            
            
            (if(= i 0)
              (for-loop [j 0 (< j 2) (inc j)] 
                        (aset currBallIDX j
                              (get (vec(GetDist newX newY))
                                   (+(*(findMinDistIndex (vec(GetDist newX newY)))3)j)
                                   )
                              )
                        )
              
              (for-loop [j 0 (< j 2) (inc j)] 
                        (aset currBallIDX j
                              (get (vec(GetDist newX newY))
                                   (+(*(findMinDistIndex (vec(GetDist newX newY)))3)j)
                                   )
                              )
                        )
              )
            
            (if(= i 0)
              (for-loop [k 0 (< k 2) (inc k)]                   
                        
                        (println
                          "Distance" (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                          "X" (get newX (get currBallIDX k))
                          "Y" (get newY (get currBallIDX k)) 
                          "Ball mass" (get (vec newMass) (int(get currBallIDX k)))
                          )
                        ;X [t_step x_current m_ball v_current e r q]
                        (println "X"i
                                 (X 1 (get newX (get currBallIDX k)) (get (vec newMass) (int(get currBallIDX k)))
                                    0 2 (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                                    10
                                    ))
                        (println "Y"i
                                 (Y 1 (get newY (get currBallIDX k)) (get (vec newMass) (int(get currBallIDX k)))
                                    0 2 (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                                    10
                                    ))
                        )
              
              (for-loop [k 0 (< k 2) (inc k)]                   
                        
                        (println
                          "Distance" (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                          "X" (get newX (get currBallIDX k))
                          "Y" (get newY (get currBallIDX k))
                          "Ball mass" (get (vec newMass) (int(get currBallIDX k)))
                          )
                        (println"X"i
                                (X 1 (get newX (get currBallIDX k)) (get (vec newMass) (int(get currBallIDX k)))
                                   0 2 (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                                   10
                                   ))
                        (println "Y"i
                                 (Y 1 (get newY (get currBallIDX k)) (get (vec newMass) (int(get currBallIDX k)))
                                    0 2 (get (vec (GetDist (vec newX) (vec newY))) (+(* (findMinDistIndex (vec (GetDist (vec newX) (vec newY))))3)2))
                                    10
                                    ))
                        )
              )
            
            
            )
  )
(test_sm Xvector Yvector MassVector)
