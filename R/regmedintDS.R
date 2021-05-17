regmedintDS <- function(data=data.name, yvar=yvar.name, avar=avar.name, mvar=mvar.name, cvar=cvar.name, eventvar=eventvar.name,
                        interaction = TRUE, casecontrol = FALSE, na.omit = FALSE){
  
  data <- eval(parse(text=data), envir = parent.frame())
  
  #cvar <- unlist(strsplit(cvar.transmit, split=","))

  regmedint.out <- regmedint::regmedint(data=data, yvar=yvar, avar=avar, mvar=mvar, cvar=cvar, eventvar=eventvar,
                                        a0=0, a1=1, m_cde = 1, c_cond = 0.5, mreg = "logistic", yreg = "survAFT_weibull",
                                        interaction = interaction, casecontrol = casecontrol)
  
  out <- summary(regmedint.out)
  return(out)
  
}