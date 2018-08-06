;;A namespace to keep color palettes and
;;style-related information together
;;for charting and whatnot.
(ns marathon.visuals.styling)

(defn ->color [[r g b]] (java.awt.Color. (int r) (int g) (int b)))
(def palette
  (->>
   {:DarkYellow 	[255	255	0]
    :LightYellow	[255	255	153]
    :DarkGreen	 [0	102	0]
    :LightGreen	 [146	208	80]
    :DarkBlue	 [0	0	255]
    :LightBlue	 [0	255	255]
    :red         [255 0 0]
    :green       [0 176 80] 
    :black       [0 0 0]
    :orange      [237 125 49] ; [255 192 0]
    :yellow      [255 255 0]
    :white       [255 255 255]
    :DarkPurple	 [112	48	160]	
    :LightPurple	[205	193	218]}
   (map (fn [[lbl c]] [lbl (->color c)]))
   (into {})))

(def colors
  {"MP_DA_C1"		:DarkPurple
   "MA_DA_C1"		:DarkPurple
   "MA_DA_C2"		:DarkPurple
   "MD_DA_C1"		:DarkPurple
   "MD_DA_C2"		:DarkPurple
   "MP_NDA_C3"		:LightPurple
   "MA_NDA_C3"		:LightPurple
   "MD_NDA_C3"		:LightPurple
   "R_C1"		:DarkGreen
   "R_C2"		:LightGreen
   "PB_C3"		:DarkYellow
   "PB_C4"		:DarkYellow
   "PT_C4"		:LightYellow
   "PL_C4"		:LightYellow
   ":recovery"          :DarkYellow})

(defn loc->color
  ([loc] (or (get palette (get colors loc) ) (throw (Exception. (str [:unknown-location loc])))))
  ([loc & xs] (loc->color loc)))

(def srm-style
  {"R_C2" {:background :LightGreen, :color :black},
   "PL_C4" {:background :LightYellow, :color :red},
   ":recovery" {:background :DarkYellow, :color :black},
   "MA_DA_C1" {:background :DarkPurple, :color :white},
   "MA_DA_C2" {:background :DarkPurple, :color :white},
   "PB_C4" {:background :DarkYellow, :color :red},
   "MP_DA_C1" {:background :DarkPurple, :color :white},
   "MD_DA_C1" {:background :DarkPurple, :color :white},
   "PT_C4" {:background :LightYellow, :color :red},
   "MD_DA_C2" {:background :DarkPurple, :color :black},
   "MD_NDA_C3" {:background :LightPurple, :color :black},
   "MA_NDA_C3" {:background :LightPurple, :color :black},
   "R_C1" {:background :DarkGreen, :color :white},
   "MP_NDA_C3" {:background :LightPurple, :color :black},
   "PB_C3" {:background :DarkYellow, :color :black}
   "Reset"        {:background :red :color :black}
   "Train"        {:background :orange :color :black}
   "Ready"        {:background :yellow  :color :black}
   (str ["Ready" :deployable])         {:background :yellow  :color :black}
   (str ["Available" :non-deployable]) {:background :green   :color :black}
   "Available"  {:background :green   :color :black}
   "DeMobilization" {:background :red :color :black}
   "DeMobilizing" {:background :red :color :black}
   "Deployed"     {:background :DarkPurple :color :white}
   "Overlapping"   {:background :DarkPurple :color :white}
   "R_C3"  {:background :LightYellow, :color :red}
   })


(def default-palette
  (into {} (map (fn [[k v]]
                  [k (palette (:background v))]))
        (seq srm-style)))
                           
