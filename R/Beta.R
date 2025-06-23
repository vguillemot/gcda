Beta <-
function(nk,etak,Sk) (1/(nk*etak)) * ( nk*(nk-2)*tr(Sk %*% Sk) + ( nk*tr(Sk) )**2 )

