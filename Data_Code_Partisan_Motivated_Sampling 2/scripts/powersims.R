#Simulations for power using the simr package. 

#Experiment 1a power analysis. 
#####
load("data/Experiment1amodel.rda")
mod1PostPower <- powerSim(Study1Power, fixed("Samp_GroupB_dumIn.Group", "z"), seed = 5, nsim = 800, alpha = .05)#calculating observed power for Experiment 1a
extend <- extend(Study1Power, along="Participant", n=1000) 
mod1PostPower2 <- powerSim(extend, fixed("Samp_GroupB_dumIn.Group", "z"), seed = 2, nsim = 800, alpha = .05)
sim2.power.curve <- powerCurve(Study1Power, test = fixed("Samp_GroupB_dumIn.Group", "z"), along ="Participant" , nsim=800) #run power curve. Takes a long time. 
#####


print("Simulations for power")
print("Experiment 1a")
print(mod1PostPower)
print("Experiment 1a with 1k participants")
print(mod1PostPower2)

