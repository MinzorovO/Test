(ns my-test1)

; Вычисление радиуса шарика из его массы и плотности
; Calculation of the ball radius from its mass and density
; m_ball - mass 
; p_ball - density
(defn RadiusCalculation [m_ball p_ball]
  (Math/pow
    (/
      (/ m_ball p_ball)
      (/ (* 4 (Math/PI)) 3) 
      )
    (/ 1 3)
    )
  )


; Вычисление силы взаимодействия шариков используя потенциал Ленарда-Джонса
; Calculation of the force of interaction of balls using the Lenard-Jones potential
; e - the depth of the potential yawner
; r - the distance between the centers of the particles
; q - the distance at which the interaction energy becomes zero
(defn LennardJonesPower [e r q]
  (cond
    (<= r (* 2.5 q))
    (*
      (/ (* 12 e) q)
      (-
        (Math/pow (/ q r) 13)
        (Math/pow (/ q r) 7)
        )
      )
    )
  )


; Вычисление ускорения учитывая силу взаимодействия и массы шарика
; Calculation of speed-up given the strength of the interaction and the mass of the ball
(defn Speed_up [m_ball e r q]
  
  (/ (LennardJonesPower e r q) m_ball)
  
  )


; Вычисление скорости
(defn Speed [t_step v_current m_ball e r q]
  
  (+ 
    v_current
    (* (Speed_up m_ball e r q) t_step)
    )
  
  )

; Вычисление координаты Х 
(defn X [t_step x_current m_ball e r q]
  
  (+
    (+
      x_current 
      (* (Speed t_step v_current m_ball e r q)t_step)
      )
    
    (* 
      (Speed_up m_ball e r q) 
      (/(Math/pow t_step 2)2)
      )
    )
  
  )


; Вычисление координаты Y 
(defn Y [t_step y_current m_ball e r q]
  
  (+
    (+
      y_current 
      (* (Speed t_step v_current m_ball e r q)t_step)
      )
    
    (* 
      (Speed_up m_ball e r q) 
      (/(Math/pow t_step 2)2)
      )
    )
  
  )



;
(defn my_main [d_ball h_container w_container k1 k2 k3 temp_max num_of_balls]
  
  )



