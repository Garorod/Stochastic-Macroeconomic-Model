grapher <- function(t,T,betah,betap,deltah,deltap,a,b, yleft, n_hleft) {
  #определим c[0]
  cleft = (-1*yleft*exp(T/b)*(betah-1+b*deltah))/(betah*(exp((T-b*deltah*T)/(b*betah))-1))
  #определим y от c[0]
  y <- yleft*exp(t/b)+((betah*cleft)/(betah - 1 + b*deltah))*(exp((t-b*deltah*t)/(b*betah))-1)
  #теперь определим c[t]
  c <- cleft * (exp((-t/b)+deltah*t))**(-1/betah)
  #определим p[0]
  pleft = (a/b)*(exp(T/b))
  p <- (a/b)*exp((T-t)/b)
  # прибыль фирмы
  n_pleft = -n_hleft
  #pi[0] находим из условия N_p[T] = 0?(Вы просто сказали, что сами задаем, но в этом случае условие нарушается)
  pileft_p = (((pleft*cleft*(exp(T*((1-betah-deltah*b)/(b*betah)))-1)*b*betah)/(1-deltah*b-betah))+n_pleft)*((1-betap-deltap*b)/(b*betap))*((1-exp(T*(1-betap-deltap*b)/(b*betap)))**(-1))
  pi_p <- pileft_p*(exp(t*((1-betap-deltap*b)/(b*betap))))
  #знаем N_h[0] => знаем N_p[0]
  # N_p
  n_p = ((pleft*cleft*(exp(t*((1-betah-deltah*b)/(b*betah)))-1)*b*betah)/(1-deltah*b-betah)) - ((pileft_p*(1-exp(t*((1-betap-deltap*b)/(b*betap))))*b*betap)/(1-betap-deltap*b)) + n_pleft 
  
  matplot(y,type = c('l'))
  matplot(c,type = c('l'))
  matplot(n_p,type = c('l'))
  matplot(pi_p,type = c('l'))
}

tleft <- 0
tright <-1
dt<- 0.05
# графики для разных beta
grapher(betah = 0.3,betap = 0.5,deltah = 0.4,deltap = 0.3,a = 0.2, b = 0.3, t = seq(from = tleft, to = tright, by = dt),T = tright, yleft = 1,n_hleft = 10)
grapher(betah = 0.9,betap = 0.9 ,deltah = 0.3,deltap = 0.5,a = 0.3, b = 0.3 , t = seq(from = tleft, to = tright, by = dt),T = tright, yleft = 1,n_hleft = 3)
# графики для высокой инфляции
grapher(betah = 0.3,betap = 0.5,deltah = 0.4,deltap = 0.3,a = 0.2, b = 1, t = seq(from = tleft, to = tright, by = dt),T = tright, yleft = 1,n_hleft = 1)
# графики для высоких delta
grapher(betah = 0.3,betap = 0.5,deltah = 0.8  ,deltap = 0.9 ,a = 0.2, b = 0.3 , t = seq(from = tleft, to = tright, by = dt),T = tright, yleft = 1,n_hleft = 1)
grapher(betah = 0.3,betap = 0.5,deltah = 0.4  ,deltap = 0.3 ,a = 0.2, b = 0.3 , t = seq(from = tleft, to = tright, by = dt),T = tright, yleft = 1,n_hleft = 10)
