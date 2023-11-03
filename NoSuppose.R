
nosuppose <- function(y,x) 
{
   y <- scale(y,scale=F)
   centery <- attr(y,'scaled:center')
   x <- scale(x,scale=F)
   centerx <- attr(x,'scaled:center')
   xtx <- t(x) %*% x
   xtxi <- solve(xtx)
   xty <- t(x) %*% y

   bhat <- xtxi %*% xty

   n <- nrow(x)
   covxty <- (1/n) * xty %*% t(xty)
   xtxin <- n * xtxi

   covb <- xtxin %*% covxty %*% xtxin
   
   list(bhat,covb)

}

