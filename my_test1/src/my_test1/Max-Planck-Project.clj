(ns my-test1)

(defn LennardJonesPotential [r e q]
  
  (if ( >= r (* 2.5 q)) 
    
    (conj [] (-
               (*(* 4 e)(-(Math/pow (/ q r) 12)(Math/pow (/ q r) 6)))
               (*(* 4 e)(-(Math/pow (/ q (* 2.5 q)) 12)(Math/pow (/ q (* 2.5 q)) 6)))
               )
          )
    
    (conj [] 0)
    )
  )

(defn HeatingTime [d k1 k2 k3 temp_max]
  (def v [])
  (conj v[0] 
        (/(* (* (* d 0.1) (* k1 k2)) k3)temp_max) 
        )
  )




