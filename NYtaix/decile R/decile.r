

#Q1
decile = 
  function(object){
    quantile(object, prob = seq(0,1,0.1))
  }



#Q2
#method1 
#fit = lm(y~x)

#method2
cal_beta =
  function(x,y){
    sumx = sum(x)
    sumy = sum(y)
    sumx_sqr = sum(x^2)
    sumxy = sum(x*y)
    
    if(length(x) != length(y))
      stop("x and y have different length")
    n = length(x)
    
    beta_1 = (n*sumxy - sumx*sumy)/(n*sumx_sqr - sumx^2)
    beta_0 = (sumy - beta_1*sumx)/n
    return(c(beta_0,beta_1))
  }

#method 3
cal_beta2 = 
  function(x,y){
    beta_1 = cov(x,y)/var(x);
    beta_0 = mean(y) - beta_1*mean(x)
    return(c(beta_0,beta_1))
  }

#Q3 
#regression with two regressor
cal_beta3 =
  function(x1,x2,y){
    sumx1 = sum(x1);sumx2 = sum(x2);
    sum_x1x2 = sum(x1*x2); sum_x1y = sum(x1*y); sum_x2y = sum(x2*y);
    sumx1_sqr = sum(x1^2); sumx2_sqr = sum(x2^2);
    
    beta_2 = (sumx1_sqr*sum_x2y - sum_x1x2*sum_x1y)/(sumx1_sqr*sumx2_sqr - sum_x1x2^2)
    beta_1 = (sumx2_sqr*sum_x1y - sum_x1x2*sum_x2y)/(sumx1_sqr*sumx2_sqr - sum_x1x2^2)
    beta_0 = mean(y) - beta_1*mean(x1) - beta_2*mean(x2)
    return(c(beta_0,beta_1,beta_2))
  }

cal_beta4 = 
  function(x1,x2,y){
    if(length(x1) != length(x2)| length(x1) != length(y))
      stop("x1, x2 and y have different length")
    n = length(x1)
    
    sum_x1 = sum(x1);sum_x2 = sum(x2);
    sum_x2x2 = sum(x2*x2); sum_x1x2 = sum(x1*x2); sum_x1x1 = sum(x1*x1);
    matX = matrix(c(n, sum_x1, sum_x2,
                    sum_x1, sum_x1x1, sum_x1x2,
                    sum_x2, sum_x1x2, sum_x2x2),nrow = 3, byrow =TRUE)
    rev_matX = solve(matX)
    a = rev_matX
    
    ele = rep(1,n)
    beta_0 = sum((a[1]*ele + a[4]*x1 +a[7]*x2)*y)
    beta_1 = sum((a[2]*ele + a[5]*x1 +a[8]*x2)*y) 
    beta_2 = sum((a[3]*ele + a[6]*x1 +a[9]*x2)*y)
    return(c(beta_0,beta_1,beta_2))
  }
