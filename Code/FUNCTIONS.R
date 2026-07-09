# ESTIMATIONS FUNCTIONS


get.hs <- function(bdraw,lambda.hs,nu.hs,tau.hs,zeta.hs){
  k <- length(bdraw)
  if (is.na(tau.hs)){
    tau.hs <- 1   
  }else{
    tau.hs <- invgamma::rinvgamma(1,shape=(k+1)/2,rate=1/zeta.hs+sum(bdraw^2/lambda.hs)/2) 
  }
  
  lambda.hs <- invgamma::rinvgamma(k,shape=1,rate=1/nu.hs+bdraw^2/(2*tau.hs))
  
  nu.hs <- invgamma::rinvgamma(k,shape=1,rate=1+1/lambda.hs)
  zeta.hs <- invgamma::rinvgamma(1,shape=1,rate=1+1/tau.hs)
  
  ret <- list("psi"=(lambda.hs*tau.hs),"lambda"=lambda.hs,"tau"=tau.hs,"nu"=nu.hs,"zeta"=zeta.hs)
  return(ret)
}
Big_BART    <- function(R,X,F,Z,
                        nsave,nburn,num.trees,
                        set.sector,set.grid.Z,set.Z,
                        cgm.exp,cgm.level,sd.mu,
                        Q.q){
  
  ###########################################
  # INPUT FUNCTION:
  ###########################################
  # R:           excess returns 
  # x:           Text index (either Climate Change or Natural Disasters)
  # F:           Controls factors (Fama & French and Momentum factors)
  # Z:           Effect modifiers
  # nsave:       Number of saved draws
  # nburn:       Number of burn-ins
  # num.trees:   Number of trees in BART
  # set.sectors: Number of sectors considered
  # set.grid.Z:  Number of grid points for which we compute the scenarios
  # set.Z:       Number of effect modifiers
  # cgm.exp:     CGM prior that penalizes complex trees
  # cgm.level:   pi
  # sd.mu: 
  # Q.q:         Number of latent factors
  
  h_task <- 0 # contemporaneous: no lags
  
  # Here starts the code within the function
  ntot <- nsave+nburn
  
  X   <- as.matrix(X)
  q   <- matrix(rnorm(nrow(R)*Q.q), nrow(R), Q.q) #Starting values for the latent factors
  
  #Get dimensions of the input data
  Q.f <- ncol(F) #number of controls          
  Q.x <- ncol(X) #number of observed factors (text indexes)  
  Q.z <- ncol(Z) #number of effect modifiers
  M   <- ncol(R) #number of stocks
  
  #Now include appropriate lags of the right-hand sided variables
  Xl <- X[1:(nrow(X)-h_task), , drop=FALSE] 
  fl <- F[1:(nrow(F)-h_task), ]             
  R  <- R[(h_task+1):nrow(R), ]
  X  <- X[(h_task+1):nrow(X),, drop=FALSE]
  q  <- q[(h_task+1):nrow(q),, drop=FALSE]
  T  <- nrow(R)
  
  ################################################
  # 1.  SOME PRELIMINARIES
  ################################################
  
  ########
  # 1.1.1 Compute starting values for Lambda.f, Lambda.q and Phi and the Omega's
  #       Simple OLS regression for each stock individually
  ########
  Lambda.f <- matrix(NA, M, Q.f)
  Lambda.q <- matrix(NA, M, Q.q)
  Phi      <- matrix(NA, M, Q.x)
  Omega    <- diag(M)
  for (i in seq_len(M)){
    r.i       <- R[,i]
    X.big     <- cbind(Xl, q, fl) 
    beta.hat  <- solve(crossprod(X.big))%*%crossprod(X.big, r.i)
    omega.hat <- crossprod(r.i - X.big%*%beta.hat)/(T-ncol(X.big))
    
    Phi[i,]       <- beta.hat[1:Q.x,]                        
    Lambda.q[i, ] <- beta.hat[(Q.x+1):(Q.x + Q.q)]           
    Lambda.f[i, ] <- beta.hat[(Q.x+ Q.q +1):nrow(beta.hat)]  
    Omega[i,i]    <- as.numeric(omega.hat)
  }
  
  #######
  # 1.1.2 Do a starting value for the prior variances
  #######
  phi      <- as.vector(Phi)          #vectorize everything CHK CHK CHK
  zeta.x   <- rep(M*Q.x)
  nu.x     <- rep(M*Q.x)
  tau.q    <- tau.f <- tau.x <- 1
  kappa.q  <- kappa.f <- kappa.x <- 1
  
  hs.x     <- get.hs(phi, zeta.x, nu.hs = nu.x, tau.hs = tau.x, zeta.hs=kappa.x)                      
  theta.x  <- matrix(hs.x$psi, M, Q.x)             #This is a M times Qx matrix of prior variances                                                        
  hs.q     <- get.hs(as.vector(Lambda.q), rep(M*Q.q), nu.hs = rep(M*Q.q), tau.hs = tau.q, zeta.hs=kappa.q) 
  theta.q  <- matrix(hs.q$psi, M, Q.q)             #This is a M times Qq matrix of prior variances                                                        
  hs.f     <- get.hs(as.vector(Lambda.f), rep(M*Q.f), nu.hs = rep(M*Q.f), tau.hs = tau.f, zeta.hs=kappa.f) 
  theta.f  <- matrix(hs.f$psi, M, Q.f)             #This is a M times Qf matrix of prior variances                                                        

  #######
  # 1.2 Add BART in the prior mean
  #######
  prior.sig = c(10000^50, 0.5)
  control <- dbartsControl(verbose = FALSE, keepTrainingFits = TRUE, useQuantiles = FALSE,
                           keepTrees = FALSE, n.samples = ntot,
                           n.cuts = 100L, n.burn = nburn, n.trees = num.trees, n.chains = 1,
                           n.threads = 1, n.thin = 1L, printEvery = 1,
                           printCutoffs = 0L, rngKind = "default", rngNormalKind = "default",
                           updateState = FALSE)
  sampler.list <- list()
  svdraw.list <- list()
  for (jj in seq_len(Q.x)){
    cgm.exp0           <- cgm.exp
    cgm.level0         <- cgm.level
    sampler.list[[jj]] <- dbarts(Phi[,jj] ~ Z, control = control, tree.prior = cgm(cgm.exp0, cgm.level0), node.prior = normal(sd.mu),n.samples = nsave, weights=rep(1,M), sigma = 1, resid.prior = chisq(prior.sig[[1]], prior.sig[[2]]))  #sampler.list[[jj]] <- dbarts(P[,jj] ~ Z, control = control, tree.prior = cgm(cgm.exp0, cgm.level0), node.prior = normal(sd.mu),n.samples = nsave, weights=rep(1,M), sigma = 1, resid.prior = chisq(prior.sig[[1]], prior.sig[[2]]))  
  }
  sampler.run <- list()
  
  #######
  # 1.3 Construct storage matrices
  #######
  Phi.store         <- array(NA, c(nsave,   M, Q.x))   #Storage matrix for the posterior of the factor loadings related to f
  lambda.q.store    <- array(NA, c(nsave,   M, Q.q))   #Storage matrix for the posterior of the factor loadings related to q
  lambda.f.store    <- array(NA, c(nsave,   M, Q.f))   #Storage matrix for the posterior of the coefficients related to x
  Omega.store       <- array(NA, c(nsave,   M))        #Storage for the measurement error variances
  q.store           <- array(NA, c(nsave,   T, Q.q))   #Storage matrix for the latent factors
  mu.store          <- array(NA, c(nsave,   M, Q.x))   #Storage matrix over the prior means
  thetax.store      <- array(NA, c(nsave,   M, Q.x))   #Storage matrix for the prior variances
  count.store       <- array(NA, c(nsave, Q.x, Q.z))   #Storage matrix that stores the #of times a Z_j shows up in a decision rule
  variations.store  <- array(NA, c(nsave,   M, Q.x))   #Systematic Variance Explained
  
  if (set.Z == 1){phi.scen.store <- array(NA, c(nsave, set.grid.Z))}
  if (set.Z == 2){phi.scen.store <- array(NA, c(nsave, set.grid.Z, set.sector))}
  
  ############################
  # 2. BIG GIBBS LOOP
  ############################
  
  print("Running repetitions:")
  progress_bar = txtProgressBar(min=0, max=ntot, style = 1, char="=")
  
  for (irep in seq_len(ntot)){
    
    #########
    # 2.1 Step 1: Do BART first
    #########
    fit.vals    <- matrix(0, M, Q.x)    #These are the fitted values for the BART model in the prior mean
    count.trees <- matrix(0, Q.x, Q.z)  #These are the number of times a given variable shows up in the splitting rules
    for (i in seq_len(Q.x)){ 
      #Does the BART magic
      sampler.list[[i]]$setResponse(as.vector(Phi[,i])) 
      sampler.list[[i]]$setWeights(1/theta.x[,i])       
        
      rep.i            <- sampler.list[[i]]$run(0L, 1L)
      sampler.run[[i]] <- rep.i
      fit.vals[ , i]   <- rep.i$train 
        
      count.trees[i, ] <- t(sampler.run[[i]]$varcount)
    }
    
    #########
    # 2.2 Step 2: Sample the loadings on the observed factors (text indices)
    #########
    for (i in seq_len(M)){
      r.hat.i <- (R[,i] - q%*%Lambda.q[i,] - fl%*% Lambda.f[i,])/sqrt(Omega[i,i])  
      Xl.i    <- Xl/sqrt(Omega[i,i])  
      #Compute the posterior covariance
      if (Q.x > 1){ #if (Q.f > 1){
        V.Phi.i    <- solve(crossprod(Xl.i) + diag(1/theta.x[i,]))   
        #Compute the posterior mean
        Phi.i.mean <- V.Phi.i %*% (crossprod(Xl.i, r.hat.i) + diag(1/theta.x[i,])%*%fit.vals[i,]) 
        #Draw Phi from a Gaussian #Draw lambda.f from a Gaussian
        Phi[i,]    <- Phi.i.mean + t(chol(V.Phi.i))%*%rnorm(Q.x, 0, 1)  
      } else {
        V.Phi.i    <- 1/(crossprod(Xl.i) + (1/theta.x[i,])) 
        Phi.i.mean <- V.Phi.i * (crossprod(Xl.i, r.hat.i) + 1/theta.x[i,]*fit.vals[i])  
        Phi[i,]  <- Phi.i.mean + sqrt(V.Phi.i)%*%rnorm(Q.x, 0, 1)
      } 
    }
    
    #########
    # 2.3 Step 3: Sample Lambda.q from a Gaussian posterior
    #########
    for (i in seq_len(M)){
      r.hat.i <- (R[,i] - Xl%*%Phi[i,] - fl%*% Lambda.f[i,])/sqrt(Omega[i,i]) 
      q.i <- q/sqrt(Omega[i,i])
      #Compute the posterior covariance
      if (Q.q > 1){
        V.lambda.i <- solve(crossprod(q.i) + diag(1/theta.q[i,]))
        #Compute the posterior mean
        Lambda.i.mean <- V.lambda.i %*% (crossprod(q.i, r.hat.i))
        #Draw lambda.f from a Gaussian
        Lambda.q[i,] <- Lambda.i.mean + t(chol(V.lambda.i))%*%rnorm(Q.q)
      } else {
        V.lambda.i <- 1/(crossprod(q.i) + (1/theta.q[i,]))
        Lambda.i.mean <- V.lambda.i * (crossprod(q.i, r.hat.i)) 
        Lambda.q[i,] <- Lambda.i.mean + sqrt(V.lambda.i)%*%rnorm(Q.q, 0, 1)
      } 
    }
    
    #########
    # 2.4 Step 4: Sample loading for controls from a Gaussian posterior
    #########
    for (i in seq_len(M)){
      r.hat.i <- (R[,i] - Xl%*%Phi[i,] - q%*%Lambda.q[i,] )/sqrt(Omega[i,i]) 
      f.i <- fl/sqrt(Omega[i,i])
      #Compute the posterior covariance
      if (Q.f > 1){
        #--> check for singularity:: 
        V.Lambda.i <- try(solve(crossprod(f.i) + diag(1/theta.f[i,])), silent=TRUE)                       
        if (is(V.Lambda.i, "try-error")) {V.Lambda.i <- MASS::ginv(crossprod(f.i) + diag(1/theta.f[i,]))} 
        #Compute the posterior mean
        Lambda.i.mean <- V.Lambda.i %*% (crossprod(f.i, r.hat.i))  
        #Draw Lambda from a Gaussian
        Lambda.f[i,] <- Lambda.i.mean + t(chol(V.Lambda.i))%*%rnorm(Q.f)      
      } else {
        V.Lambda.i <- 1/(crossprod(f.i) + 1/theta.f[i,])  
        #Compute the posterior mean
        Lambda.i.mean <- V.Lambda.i %*% (crossprod(f.i, r.hat.i)) 
        #Draw lambda.f from a Gaussian
        Lambda.f[i,] <- Lambda.i.mean + sqrt(V.Lambda.i)%*%rnorm(Q.f)        
      } 
    }
    
    #########
    # 2.5 Step 5: Simulate the Omega's from inverse Gammas
    #########
    for (i in seq_len(M)){
      r.i <- R[,i] - Xl%*%Phi[i,] - q%*%Lambda.q[i,] - fl%*%Lambda.f[i,] 
      a.i <- nrow(R)/2 + 0.01    
      b.i <- sum(r.i^2)/2 + 0.01 
      Omega[i, i] <- 1/rgamma(1, a.i, b.i)  
    }
    
    #########
    # 2.6 Step 6: Sample the latent factors from Gaussians
    #########
    error.factor <- R - Xl%*%t(Phi) - fl%*%t(Lambda.f) 
    
    for (t in seq_len(T)){
      X.q <- Lambda.q*1/sqrt(diag(Omega))
      r.q <- error.factor[t,] * 1/sqrt(diag(Omega)) 
      
      V.q <- solve(crossprod(X.q) + diag(Q.q))
      q.mu <- V.q %*% crossprod(X.q, r.q)  
      q.draw  <- q.mu + t(chol(V.q))%*%rnorm(Q.q)
      q[t, ] <- q.draw
    }
    
    #########
    # 2.7 Step 7: Sample the prior hyperparameters for tau using a Horseshoe
    #########
    if (irep > 0.2*nburn){
        hs.x     <- get.hs(as.vector(Phi - fit.vals), hs.x$lambda, nu.hs = hs.x$nu, tau.hs = hs.x$tau, zeta.hs=hs.x$zeta) 
        theta.x  <- matrix(hs.x$psi, M, Q.x) #This is a M times Qx matrix of prior variances
        theta.x[theta.x<1e-6] <- 1e-6        
    }else{
      theta.x <- matrix(1, M, Q.x)   
      }
    
    hs.q     <- get.hs(as.vector(Lambda.q), hs.q$lambda, nu.hs = hs.q$nu, tau.hs = hs.q$tau, zeta.hs=hs.q$zeta)
    theta.q  <- matrix(hs.q$psi, M, Q.q)
    theta.q[theta.q<1e-6] <-1e-6
    hs.f     <- get.hs(as.vector(Lambda.f)      , hs.f$lambda, nu.hs = hs.f$nu, tau.hs = hs.f$tau, zeta.hs=hs.f$zeta) 
    theta.f  <- matrix(hs.f$psi, M, Q.f) 
    theta.f[theta.f<1e-6] <- 1e-6      
      
    
    if (irep > nburn){
      
      ########
      # 2.8 Scenario Analysis
      ########
      
      #Start computing the mu's based on specific assumptions of z

      set.z <- seq(base::min(Z[,1]), base::max(Z[,1]), length.out=set.grid.Z)
      resp <- matrix(NA, set.grid.Z, set.sector)
      
      if (set.Z == 2){
        for (ii in 1:set.sector){
          #z.j   <- Z[sample(1:M,1), ]#[set.z]
          z.j   <- c(0,0)
          z.j[2]<- ii
          count <- 0
          for (jj in set.z){
            count          <- count+1
            #z.j[set.Z]     <- jj
            z.j[1]         <- jj
            
            resp.j         <- sampler.list[[1]]$predict(as.numeric(z.j))
            resp[count,ii] <- resp.j
          } 
        }
        phi.scen.store[irep-nburn,,] <- resp
      }
      if (set.Z == 1){ 
        #Start computing the mu's based on specific assumptions of z
        set.z <- round(seq(base::min(Z[,set.Z]), base::max(Z[,set.Z]), length.out=set.grid.Z),digits=2)
        resp <- matrix(NA, set.grid.Z, 1)
        z.j <- Z[sample(1:M,1), ]#[set.z]
        count <- 0
        for (jj in set.z){
          count <- count+1
          z.j[set.Z] <- jj
          
          resp.j <- sampler.list[[1]]$predict(as.numeric(z.j))
          resp[count] <- resp.j
        } 
        phi.scen.store[irep-nburn,] <- resp
      }
      
      ############################
      # 3. STORAGE RESULTS
      ############################
      Phi.store[irep-nburn,,]      <- Phi      
      lambda.q.store[irep-nburn,,] <- Lambda.q   
      lambda.f.store[irep-nburn,,] <- Lambda.f 
      q.store[irep-nburn,,]        <- q
      Omega.store[irep-nburn,]     <- diag(Omega)
      mu.store[irep-nburn,,]       <- fit.vals
      count.store[irep-nburn,,]    <- count.trees
      thetax.store[irep-nburn,,]   <- theta.x 
      variations.store[irep-nburn,,] <- Phi^2*as.numeric(var(Xl))/diag(tcrossprod(Phi)*as.numeric(var(Xl))+tcrossprod(Lambda.q)*as.numeric(var(q))+Lambda.f%*%diag(apply(fl,2,var))%*%t(Lambda.f))
      
    }
    
    setTxtProgressBar(progress_bar, value = irep)
    
  }
  close(progress_bar)
  
  ############################
  # 4. FINAL OUTPUT 
  ############################
  
  FINAL <- list(Phi.store,lambda.q.store,lambda.f.store,q.store,Omega.store,
                mu.store,
                count.store,
                thetax.store,
                phi.scen.store,
                variations.store)
  
  names(FINAL) <- c("Phi.store", "lambda.q.store", "lambda.f.store","q.store","Omega.store",
                    "mu.store",
                    "count.store",
                    "thetaf.store",
                    "phi.scen.store",
                    "variations.store")
  
  return(FINAL)
}


