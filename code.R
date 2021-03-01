#### Random generator for Q1
random_generator_1 <- function(seed=3267, a=1569, m=4096, c=987, len=5) {
  # Generate the first uniform RV
  z = (a*seed+c)%%m
  u = z[1]/m
  
  # Using it to find the corresponding repair time by inverse cdf
  # For different ranges, apply different functions for inverse cdf
  if (u <= 0.2-0.2*exp(-(5/6)^0.5)) {
    x = 6(-log(1-5*u[1]))^2
  } else if(0.2-0.2*exp(-(5/6)^0.5) < u & u <= 1-0.2*exp(-(5/6)^0.5)) {
    x = 5
  } else x = 6*(-log(5-5*u))^2

  # Generate the rest RVs
  for (i in 1:(len-1)) {
    z[i+1] = ((a*z[i]+c)%%m)
    u[i+1] = z[i+1]/m    
    # Using uniform RVs to find the corresponding repair time by inverse cdf
    # For different ranges, apply different functions for inverse cdf
    if (u[i+1] <= 0.2-0.2*exp(-(5/6)^0.5)) {
      x[i+1] = 6*(-log(1-5*u[i+1]))^2
    } else if(0.2-0.2*exp(-(5/6)^0.5) < u[i+1] & u[i+1] <= 1-0.2*exp(-(5/6)^0.5)) {
      x[i+1] = 5
    } else x[i+1] = 6*(-log(5-5*u[i+1]))^2
  }
  return(x)
}

random_generator_1(len=5)


#### Random Generator for Q2
random_generator_2 <- function(seed=3267, a=1569, m=4096, c=987, lenth=2) {
  # Generate the first uniform RV
  z = (a*seed+c)%%m
  u = z[1]/m

  # Using it to find the corresponding repair time by composition method
  if (u>=0.2) {
    # If u>=0.2, repair time is 5
    x = 5
  } else {
    # If u<0.2, generate another uniform RV and calculate the repair time
    z[2] = ((a*z[1]+c)%%m)
    u[2] = z[2]/m
    x = 6*(-log(1-u[2]))^2
  }
  # As the first sample of repair time is generated, 
  # the length of RV generated is 1
  len = 1

  # Generate the rest RVs
  # Loop until find the required-length random variables
  while (len<lenth) {
    # Count the current length of vector "u"
    len_u = length(u)
    z[len_u+1] = ((a*z[len_u]+c)%%m)
    u[len_u+1] = z[len_u+1]/m
    
    # Using them to find the corresponding repair time by composition method
    # For different ranges, apply different functions for inverse cdf
    if (u[len_u+1]>=0.2) {
      # If u>=0.2, repair time is 5
      x[len+1] = 5
    } else {
      # If u<0.2, generate another uniform RV and calculate the repair time
      z[len_u+2] = ((a*z[len_u+1]+c)%%m)
      u[len_u+2] = z[len_u+2]/m
      x[len+1] = 6*(-log(1-u[len_u+2]))^2
    }
    # Count the current length of vector "x"
    len = length(x)
  }
  return(x)
}


random_generator_2(lenth=2)


# Random Generator for Q3
random_generator_3 <- function(seed=3267, a=1569, m=4096, c=987, lenth=3) {
  x = c()
  
  # Generate the first uniform RV
  z = (a*seed+c)%%m
  u = z[1]/m
  # Using it to get the sample from U(1,4)
  u_1 = u*3+1
  
  # Generate the second uniform RV
  z[2] = (a*z[1]+c)%%m
  u[2] = z[2]/m
  # Using it to get the sample from U(0,3/7)
  u_2 = 3/7*u[2]
  
  # Using them to find the corresponding RV by acceptance-rejection method
  if (u_2<=3*sqrt(u_1)/14) {
    # If accept, let x=u_1 and length=1
    x = u_1
    len = 1
  } else {
    # If reject, let length=0
    len = 0 
  }
  
  # Generate the rest RVs
  # Loop until find the required-length random variables
  while (len<lenth) {
    # Count the current length of vector "u"
    len_u = length(u)
    # Generate U_1 and U_2 one by one
    z[len_u+1] = ((a*z[len_u]+c)%%m)
    u[len_u+1] = z[len_u+1]/m
    u_1[len_u/2+1] = u[len_u+1]*3+1
    
    z[len_u+2] = (a*z[len_u+1]+c)%%m
    u[len_u+2] = z[len_u+2]/m
    u_2[len_u/2+1] = 3/7*u[len_u+2]
    
    # Using them to find the corresponding RV by acceptance-rejection method
    if (u_2[len_u/2+1]<=3*sqrt(u_1[len_u/2+1])/14) {
      x[len+1] = u_1[len_u/2+1]
    }
    # Count the current length of vector "x"
    len = length(x)
  }
  return(x)
}

random_generator_3(lenth=3)


#### Interpolate the distribution for Q4
library(ggplot2)

df = data.frame(x = seq(1, 6)*2, y = c(0, 22, 20, 26, 32, 0))
ggplot(df, aes(x=x, y=y)) +
  geom_line() +
  geom_text(aes(label=y, vjust=-0.5)) +
  scale_x_continuous(name="", breaks = seq(1, 6)*2) +
  scale_y_continuous(name="Freq")

#### Define the density function f(x) at point x 
f_x = function(x) {
  if (x<4) {
    f = (11*x-22)/200
  } else if (x<6) {
    f = (-x+26)/200
  } else if (x<10) {
    f = (3*x+2)/200
  } else f = (-16*x+192)/200
  
  return(f)
}

#### Random Generator for Q4
# Same code structure as random_generator_3
random_generator_4 <- function(seed=3267, a=1569, m=4096, c=987, lenth=3) {
  x = c()
  z = (a*seed+c)%%m
  u = z[1]/m
  
  u_1 = u*10+2
  
  z[2] = (a*z[1]+c)%%m
  u[2] = z[2]/m
  u_2 = 4/25*u[2]
  
  if (u_2<=f_x(u_1)) {
    x = u_1
    len = 1
  } else {
    len = 0 
  }
  
  while (len<lenth) {
    len_u = length(u)
    z[len_u+1] = ((a*z[len_u]+c)%%m)
    u[len_u+1] = z[len_u+1]/m
    u_1[len_u/2+1] = u[len_u+1]*10+2
    
    z[len_u+2] = (a*z[len_u+1]+c)%%m
    u[len_u+2] = z[len_u+2]/m
    u_2[len_u/2+1] = 4/25*u[len_u+2]
    
    if (u_2[len_u/2+1]<=f_x(u_1[len_u/2+1])) {
      x[len+1] = u_1[len_u/2+1]
    }
    len = length(x)
  }
  return(x)
}

random_generator_4(lenth=2)


#### Random generator for Q5
random_generator_5 <- function(seed=3267, a=1569, m=4096, c=987, len=3, dof=4) {
  # Generate the first Uniform RV
  chi_square = c()
  z = (a*seed+c)%%m
  u = z[1]/m
  
  # Generate the rest Uniform RVs
  for (i in 1:(len*dof*2-1)) {
    z[i+1] = ((a*z[i]+c)%%m)
    u[i+1] = z[i+1]/m
  }
  
  # Allocate the "u" vector to u_1 and u_2
  u_1 = u[seq(1:(len*dof*2))%%2!=0]
  u_2 = u[seq(1:(len*dof*2))%%2==0]

    for (j in 1:len) {
    z = c()
    # Allocate the subset of "u_1" and "u_2 vector to u_f and u_s
    u_f = u_1[(1+dof*(j-1)):(dof*j)]
    u_s = u_2[(1+dof*(j-1)):(dof*j)]
    for (k in 1:dof) {
      # Generate the normal RVs
      z[k] = sqrt(-2*log(u_f[k]))*cos(2*pi*u_s[k])
    }
    # Generate the chi_square RVs with dof k
    chi_square[j] = sum(z^2)
  }
  return(chi_square)
}

random_generator_5(len=3, dof=4)
