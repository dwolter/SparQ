
(defun block->rcc8 (csp)
  "converts a CSP using block-algebra relations into a RCC CSP"
  (let ((rcc8 (calculi:load-calculus "rcc8"))
	(ba   (constraint-reasoning::constraint-network-calculus csp)))
    (simple-relation-rewrite csp
			     rcc8
			     (relation-rewrite-fn ba
						  rcc8
						  (list (cons (relation ba '(b_b b_bi b_d b_di b_eq b_f b_fi b_m b_mi b_o b_oi b_s b_si bi_b bi_bi bi_d bi_di bi_eq bi_f bi_fi 
									     bi_m bi_mi bi_o bi_oi bi_s bi_si d_b d_bi di_b di_bi eq_b eq_bi f_b f_bi fi_b fi_bi m_b m_bi mi_b
									     mi_bi o_b o_bi oi_b oi_bi s_b s_bi si_b si_bi))
							      (relation rcc8 'dc))

							(cons (relation ba '(b_m b_mi bi_m bi_mi d_m d_mi di_m di_mi eq_m eq_mi f_m f_mi fi_m fi_mi m_b m_bi m_d m_di m_eq m_f 
									     m_fi m_m m_mi m_o m_oi m_s m_si mi_b mi_bi mi_d mi_di mi_eq mi_f mi_fi mi_m mi_mi mi_o mi_oi mi_s 
									     mi_si o_m o_mi oi_m oi_mi s_m s_mi si_m si_mi)) 
							      (relation rcc8 'ec))
							
							(cons (relation ba '(d_di d_fi d_o d_oi d_si di_d di_f di_o di_oi di_s eq_f eq_fi eq_o eq_oi eq_s eq_si f_di f_fi f_o 
									     f_oi f_si fi_d fi_f fi_o fi_oi fi_s o_d o_di o_eq o_f o_fi o_o o_oi o_s o_si oi_d oi_di oi_eq oi_f 
									     oi_fi oi_o oi_oi oi_s oi_si s_di s_fi s_o s_oi s_si si_d si_f si_o si_oi si_s)) 
							      (relation rcc8 'po))

							(cons (relation ba '(eq_eq)) (relation rcc8 'eq))

							(cons (relation ba '(d_eq d_f d_s eq_d f_d f_eq f_f f_s s_d s_eq s_f s_s )) 
							      (relation rcc8 'tpp ))

							(cons (relation ba '(di_eq di_fi di_si eq_di fi_di fi_eq fi_fi fi_si si_di si_eq si_fi si_si))
							      (relation rcc8 'tppi))

							(cons (relation ba '(d_d)) 
							      (relation rcc8 'ntpp))

							(cons (relation ba '(di_di)) 
							      (relation rcc8 'ntppi)))))))

(defun block->rcc5 (csp)
  "converts a CSP using block-algebra relations into a RCC CSP"
  (let ((rcc5 (calculi:load-calculus "rcc5"))
	(ba   (constraint-reasoning::constraint-network-calculus csp)))
    (simple-relation-rewrite csp
			     rcc5
			     (relation-rewrite-fn ba
						  rcc5
						  (list (cons (relation ba '(b_b b_bi b_d b_di b_eq b_f b_fi b_m b_mi b_o b_oi b_s b_si bi_b bi_bi bi_d bi_di bi_eq bi_f bi_fi 
									     bi_m bi_mi bi_o bi_oi bi_s bi_si d_b d_bi di_b di_bi eq_b eq_bi f_b f_bi fi_b fi_bi m_b m_bi mi_b
									     mi_bi o_b o_bi oi_b oi_bi s_b s_bi si_b si_bi  mi_si o_m o_mi oi_m oi_mi s_m s_mi si_m si_mi
									     b_m b_mi bi_m bi_mi d_m d_mi di_m di_mi eq_m eq_mi f_m f_mi fi_m fi_mi m_b m_bi m_d m_di m_eq m_f 
									     m_fi m_m m_mi m_o m_oi m_s m_si mi_b mi_bi mi_d mi_di mi_eq mi_f mi_fi mi_m mi_mi mi_o mi_oi mi_s))
							      (relation rcc5 'dr))
							
							(cons (relation ba '(d_di d_fi d_o d_oi d_si di_d di_f di_o di_oi di_s eq_f eq_fi eq_o eq_oi eq_s eq_si f_di f_fi f_o 
									     f_oi f_si fi_d fi_f fi_o fi_oi fi_s o_d o_di o_eq o_f o_fi o_o o_oi o_s o_si oi_d oi_di oi_eq oi_f 
									     oi_fi oi_o oi_oi oi_s oi_si s_di s_fi s_o s_oi s_si si_d si_f si_o si_oi si_s)) 
							      (relation rcc5 'po))

							(cons (relation ba '(eq_eq)) 
							      (relation rcc5 'eq))
							
							(cons (relation ba '(d_eq d_f d_s eq_d f_d f_eq f_f f_s s_d s_eq s_f s_s d_d)) 
							      (relation rcc5 'pp))
							
							(cons (relation ba '(di_eq di_fi di_si eq_di fi_di fi_eq fi_fi fi_si si_di si_eq si_fi si_si di_di))
							      (relation rcc5 'ppi)))))))

