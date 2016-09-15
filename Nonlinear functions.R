iTableResults <- function(resultsList) {
  temp <- summary(get(resultsList[1]))@coefs
  results <- data.frame(id=rownames(as.data.frame(temp)))
  for (i in 1:length(resultsList)) {
    x <- summary(get(resultsList[i]))@coefs
    x <- as.data.frame(x)
    n <- names(x)
    n <- recode(n, "'Estimate'='Est'; 'Std. Error'='SE'; 't value'='t'")
    names(x) <- paste(resultsList[i], "_", n,sep="")
    x$id <- rownames(x)
    results <- merge(results, x, by="id", all=T)
  }
  return(results)
}

# ------------------------------------------------------------------------------------------------------------------
iAsympDiff <- function(seq, bef, aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * (asympBef + (reactDiff)*(1/(1-changeBef))^seq) +
    aft * ((asympBef+asympDiff) + (reactDiff-asympDiff)*(1-changeAft)^seq)
}
# gradient
iAsympDiffG <- deriv(body(iAsympDiff)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft"), function.arg=iAsympDiff)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_1mod <- function(seq, bef, aft, mod, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB, modRD, modAD) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB*mod) + (reactDiff + modRD*mod)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef+modAB*mod)+(asympDiff+modAD*mod)) + ((reactDiff+modRD*mod)-(asympDiff+modAD*mod))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_1modG <- deriv(body(iAsympDiff_1mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB", "modRD", "modAD"), function.arg=iAsympDiff_1mod)
# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_2mod <- function(seq, bef, aft, mod1, mod2, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2) + (reactDiff + modRD1*mod1 + modRD2*mod2)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2)+(asympDiff+modAD1*mod1+modAD2*mod2)) + ((reactDiff+modRD1*mod1+modRD2*mod2)-
    (asympDiff+modAD1*mod1+modAD2*mod2))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_2modG <- deriv(body(iAsympDiff_2mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2"), function.arg=iAsympDiff_2mod)
# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_3mod <- function(seq, bef, aft, mod1, mod2, mod3, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3)+(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3)) + ((reactDiff+modRD1*mod1+modRD2*mod2+modRD3*mod3)-
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_3modG <- deriv(body(iAsympDiff_3mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3"), function.arg=iAsympDiff_3mod)
# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_4mod <- function(seq, bef, aft, mod1, mod2, modage, modsex, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modABage, modRDage, modADage,modABsex, modRDsex, modADsex) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modABage*modage + modABsex*modsex) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRDage*modage + modRDsex*modsex)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modABage*modage + modABsex*modsex)+(asympDiff+modAD1*mod1+modAD2*mod2+modADage*modage + modADsex*modsex)) + ((reactDiff+modRD1*mod1+modRD2*mod2+modRDage*modage+modRDsex*modsex)-
    (asympDiff+modAD1*mod1+modAD2*mod2+modADage*modage + modADsex*modsex))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_4modG <- deriv(body(iAsympDiff_4mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2",
                                                                "modABage","modRDage","modADage","modABsex","modRDsex","modADsex"), function.arg=iAsympDiff_4mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_5mod <- function(seq, bef, aft, mod1, mod2, mod3, modage, modsex, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3,modABage, modRDage, modADage,modABsex, modRDsex, modADsex) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 +modABage*modage + modABsex*modsex) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3 +modRDage*modage + modRDsex*modsex)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 + modABage*modage + modABsex*modsex)+(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3 + modADage*modage + modADsex*modsex)) + ((reactDiff+modRD1*mod1+modRD2*mod2+modRD3*mod3 + modRDage*modage+modRDsex*modsex)-
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3+modADage*modage + modADsex*modsex))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_5modG <- deriv(body(iAsympDiff_5mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3",
                                                                "modABage","modRDage","modADage","modABsex","modRDsex","modADsex"), function.arg=iAsympDiff_5mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_8mod <- function(seq, bef, aft, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3,
                            modAB4, modRD4, modAD4, modAB5, modRD5, modAD5, modAB6, modRD6, modAD6,
                            modAB7, modRD7, modAD7, modAB8, modRD8, modAD8) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 +modAB4*mod4 + modAB5*mod5+modAB6*mod6 + modAB7*mod7 + modAB8*mod8) + 
    (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3 +modRD4*mod4 + modRD5*mod5 +modRD6*mod6 + modRD7*mod7 + modRD8*mod8)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 + modAB4*mod4 + modAB5*mod5 +modAB6*mod6 + modAB7*mod7 + modAB8*mod8)+
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3 + modAD4*mod4 + modAD5*mod5 + modAD6*mod6+modAD7*mod7+modAD8*mod8)) + 
    ((reactDiff+modRD1*mod1+modRD2*mod2+modRD3*mod3 + modRD4*mod4+modRD5*mod5+modRD6*mod6+modRD7*mod7+modRD8*mod8)-
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3+modAD4*mod4 + modAD5*mod5+modAD6*mod6+modAD7*mod7+modAD8*mod8))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_8modG <- deriv(body(iAsympDiff_8mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3",
                                                                "modAB4","modRD4","modAD4","modAB5","modRD5","modAD5",
                                                                "modAB6", "modRD6", "modAD6","modAB7", "modRD7", "modAD7","modAB8", "modRD8", "modAD8"), function.arg=iAsympDiff_8mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiff_11mod <- function(seq, bef, aft, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3,
                            modAB4, modRD4, modAD4, modAB5, modRD5, modAD5, modAB6, modRD6, modAD6,
                            modAB7, modRD7, modAD7, modAB8, modRD8, modAD8, modAB9, modRD9, modAD9,
                            modAB10, modRD10, modAD10, modAB11, modRD11, modAD11) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 +modAB4*mod4 + modAB5*mod5+modAB6*mod6 + modAB7*mod7 + modAB8*mod8 + modAB9*mod9 + modAB10*mod10 + modAB11*mod11) + 
    (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3 +modRD4*mod4 + modRD5*mod5 +modRD6*mod6 + modRD7*mod7 + modRD8*mod8 + modRD9*mod9 + modRD10*mod10 + modRD11*mod11)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 + modAB4*mod4 + modAB5*mod5 +modAB6*mod6 + modAB7*mod7 + modAB8*mod8 + modAB9*mod9 + modAB10*mod10 + modAB11*mod11)+
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3 + modAD4*mod4 + modAD5*mod5 + modAD6*mod6+modAD7*mod7+modAD8*mod8 + modAD9*mod9+ modAD10*mod10 + modAD11*mod11)) + 
    ((reactDiff+modRD1*mod1+modRD2*mod2+modRD3*mod3 + modRD4*mod4+modRD5*mod5+modRD6*mod6+modRD7*mod7+modRD8*mod8 + modRD9*mod9 + modRD10*mod10 + modRD11*mod11)-
    (asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3+modAD4*mod4 + modAD5*mod5+modAD6*mod6+modAD7*mod7+modAD8*mod8 + modAD9*mod9 + modAD10*mod10 + modAD11*mod11))*(1-(changeAft))^seq)
}
# gradient
iAsympDiff_11modG <- deriv(body(iAsympDiff_11mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3",
                                                                "modAB4","modRD4","modAD4","modAB5","modRD5","modAD5",
                                                                "modAB6", "modRD6", "modAD6","modAB7", "modRD7", "modAD7","modAB8", "modRD8", "modAD8",
                                                                "modAB9", "modRD9", "modAD9","modAB10", "modRD10", "modAD10","modAB11", "modRD11", "modAD11"), function.arg=iAsympDiff_11mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiffR <- function(seq, bef, aft, asympBef, changeBef, reactDiff, asympDiff, changeAft) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * (asympBef + (reactDiff)*(1/(1-changeBef))^seq) +
    aft * ((asympBef+reactDiff+asympDiff) + (-asympDiff)*(1-changeAft)^seq)
}
# gradient
iAsympDiffRG <- deriv(body(iAsympDiffR)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft"), function.arg=iAsympDiffR)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiffR_3mod <- function(seq, bef, aft, mod1, mod2, mod3, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3)+ (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3)+(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3)) + 
    (-(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3))*(1-(changeAft))^seq)
}
# gradient
iAsympDiffR_3modG <- deriv(body(iAsympDiffR_3mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3"), function.arg=iAsympDiffR_3mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiffR_4mod <- function(seq, bef, aft, mod1, mod2, modage, modsex, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modABage, modRDage, modADage,modABsex, modRDsex, modADsex) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modABage*modage + modABsex*modsex) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRDage*modage + modRDsex*modsex)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modABage*modage + modABsex*modsex)+(reactDiff + modRD1*mod1 + modRD2*mod2 + modRDage*modage + modRDsex*modsex)+(asympDiff+modAD1*mod1+modAD2*mod2+modADage*modage + modADsex*modsex)) + 
    (-(asympDiff+modAD1*mod1+modAD2*mod2+modADage*modage + modADsex*modsex))*(1-(changeAft))^seq)
}
# gradient
iAsympDiffR_4modG <- deriv(body(iAsympDiffR_4mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2",
                                                                "modABage","modRDage","modADage","modABsex","modRDsex","modADsex"), function.arg=iAsympDiffR_4mod)

# ------------------------------------------------------------------------------------------------------------------
iAsympDiffR_5mod <- function(seq, bef, aft, mod1, mod2, mod3, modage, modsex, asympBef, changeBef, reactDiff, asympDiff, changeAft, 
                            modAB1, modRD1, modAD1, modAB2, modRD2, modAD2, modAB3, modRD3, modAD3,modABage, modRDage, modADage,modABsex, modRDsex, modADsex) {
  # seq is a sequence variable
  # bef is a dummy variable, 1 = before, 0 = after event
  # aft is a dummy variable, 0 = before, 1 = after event
  bef * ((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 +modABage*modage + modABsex*modsex) + (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3 +modRDage*modage + modRDsex*modsex)*(1/(1-(changeBef)))^seq) +
    aft * (((asympBef + modAB1*mod1 + modAB2*mod2 + modAB3*mod3 + modABage*modage + modABsex*modsex)+ (reactDiff + modRD1*mod1 + modRD2*mod2 + modRD3*mod3 +modRDage*modage + modRDsex*modsex)+(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3 + modADage*modage + modADsex*modsex)) + 
    (-(asympDiff+modAD1*mod1+modAD2*mod2+modAD3*mod3+modADage*modage + modADsex*modsex))*(1-(changeAft))^seq)
}
# gradient
iAsympDiffR_5modG <- deriv(body(iAsympDiffR_5mod)[[2]], namevec=c("asympBef", "changeBef", "reactDiff", "asympDiff", "changeAft", 
                                                                "modAB1", "modRD1", "modAD1","modAB2", "modRD2", "modAD2","modAB3", "modRD3", "modAD3",
                                                                "modABage","modRDage","modADage","modABsex","modRDsex","modADsex"), function.arg=iAsympDiffR_5mod)
