(ns marathon.port.data.updatepacket)

(defn ->update-packet
  "Creates an update packet that contains the time of the future update,
   the entity that requested the update, the type of the update, and the 
   time of the request."
  [update-time requested-by update-type request-time]
  {:update-time update-time 
   :requested-by requested-by
   :update-type update-type 
   :request-time request-time})

(defn elapsed 
  "Computes the time elapsed since the last update for this packet."
  ([update-packet tnow last-update]
    (if (= last-update 0)
      (:request-time update-packet)
      (- tnow last-update)))
  ([update-packet tnow] (elapsed update-packet tnow 0)))
  
