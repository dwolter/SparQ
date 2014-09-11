
;;; SparQ calculi registry
;;;
;;; Specify calculi names and corresponding files as list ("name" "alias-name-1" "alias-name-2" ... "file-name")
;;; Observe that calculi names need to provide a unique prefix - any characters appending the supplied name
;;; will be interpreted as parameters of the calculus, e.g. "OPRA-2" is interpreted as calculus "OPRA-" and
;;; "2" is interpreted as parameter which in the case of the opra calculus family specifies the granularity

(; calculus-name(s)          file-name (without extension !!)
 ; ----------------------------------------------------------
 ("cardir"                                      "cardir")
 ("dra-con"                                     "dra-connectivity")
 ("star"                                        "revised-star-calculus")
 ("dipole" "dra-24" "dipole-coarse"             "dipole-24")
 ("dra-72" "dra-f"
           "dra-fine"
           "dipole-f"
           "dipole-fine"                        "dipole-72")
 ("dra-80" "dra-fp"
           "dra-fine-parallel"
           "dipole-fp"
           "dipole-fine-parallel"               "dipole-80")
 ("pc" 
  "point-calculus" 
  "pa" 
  "point-algebra"                               "point-calculus")
 ("double-cross" 
  "DCC"                                         "double-cross")
 ("alternative-double-cross"                    "adcc" "double-cross2")
 ("single-cross" "SCC"                          "single-cross")
 ("OPRA-"                                       "opra-calculus")
 ("oprastar1" "os1"                             "oprastar_1")
 ("oprastar2" "os2"                             "oprastar_2")

 ("absdistcalculus-"                            "absdistcalculus") 
 ("reldistcalculus"                             "reldistcalculus")
 ("flipflop" "ffc" "ff"                         "flipflop")
 ("rcc-8"                                       "rcc8")
 ("rcc-5"                                       "rcc5")
 ("allen" "aia" "ia"                            "allen")
 ("qtc-b11"                                     "qtc-b11")
 ("qtc-b12"                                     "qtc-b12")
 ("qtc-c22"                                     "qtc-c22")
 ("qtc-c21"                                     "qtc-c21")
 ("qtc-b22"                                     "qtc-b22")
 ("qtc-b21"                                     "qtc-b21")
 ("dep" "depcalc"                               "dependency")
 ("geomori" "ori" "align"                       "geometric-orientation")
 ("star"                                        "revised-star-calculus")
 ("indu" "interval-duration"                    "indu")
 ("block-algebra" "ba"                          "block-algebra")
 ("9i" "9intersection"                          "9intersection")
 ("CC2" "cycord2"                               "cycord_binary")
 ("CC3" "cycord3"                               "cycord_ternary")
 ("CDC"                                         "cdc")
)
 
 
