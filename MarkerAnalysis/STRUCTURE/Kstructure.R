source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

Ln_1M_run_1 <- c(-19974.7,-18233.3,-17535.8,-16845.0,-16300.2,-15953.8,-15565.6,-15392.7,-15116.8, -14889.5, -14814.3, -14519.1, -14567.1, -14609.2, -14372.8, -14188.3, -16641.8, -16354.0, -15812.2, -18145.1, -15438.7, -17828.1, -15791.3, -32298.5, -17306.5, -19529.3, -29057.5, -17120.6, -16624.6, -16868.1, -17620.0, -18136.7, -17748.2, -18237.0, -19311.1, -17739.8, -17264.7, -18417.3, -18389.6, -18793.8, -18692.9, -19000.0, -19193.9, -19666.8, -18909.5, -19234.3, -19665.7)

Ln_1M_run_2 <- c(-15449.4, -15055.9, -14916.8, -14683.5, -18667.5, -14461.9, -15489.7, -14619.1, -15444.1, -21500.4, -16298.5)
  
Ln_1M_run_3 <- c(-15026.9, -14933.1, -14646.6, -14626.4, -14781.2,-14376.7,-15226.9,-14214.8, -14875.4,-15441.6, -15226.7)

Ln_1M_run_4 <- c(-15467.7, -15153.3, -15031.0, -14715.7, -15306.4, -14582.3, -14511.3, -14366.1, -14371.7, -15347.8, -14269.0)

Ln_1M_run_5 <- c(-15518.9,-15282.5, -14966.5, -14759.7, -14706.9, -14399.6, -15031.3, -14239.5, -14389.8, -15113.3, -14235.8)

Ln_1M_alpha <- c(-19975.3,-18237.2,-17284.5,-16821.2,-16275.7,-15939.6,-15587.0,-15287.2,-15076.6, -14874.4, -14647.1, -14481.3, -14404.1, -16185.3, -13895.3, -13898.4, -13665.2, -13614.5, -13542.3, -13420.8, -13351.3, -13769.0, -13049.4, -14023.5, -13905.6, -12758.8, -12788.3, -13247.7, -12999.6, -12623.4, -14216.7, -13018.9, -12540.6, -12484.2, -12426.4, -12503.2, -12596.3, -12416.1, -12442.5, -12533.2, -12357.1, -12738.4, -12612.9, -13004.3, -12886.4, -13407.4, -14237.2)

k_run1 <- c(1:38,40:48)
k_runRest <- 8:18
k_run_alpha <- c(1:46,48)

ABFrun1 <- ApproxBayesFac(Ln_1M_run_1)
ABFrun2 <- ApproxBayesFac(Ln_1M_run_2)
ABFrun3 <- ApproxBayesFac(Ln_1M_run_3)
ABFrun4 <- ApproxBayesFac(Ln_1M_run_4)
ABFrun5 <- ApproxBayesFac(Ln_1M_run_5)
ABFalphaRun <- ApproxBayesFac(Ln_1M_alpha)

cbind(ABFrun1,k_run1)
cbind(ABFrun2,k_runRest)
cbind(ABFrun3,k_runRest)
cbind(ABFrun4,k_runRest)
cbind(ABFrun5,k_runRest)
cbind(ABFalphaRun,k_run_alpha)

par(mfrow=c(1,5))
plot(ABFrun1, k_run1, xlim=c(0,1.2), main= "All possible values of K", xlab= "Probability", ylab= "Knumber") 
text(ABFrun1, k_run1, labels=k_run1, cex=0.8, pos=4)

plot(ABFrun2, k_runRest, xlim=c(0,1.2), main= "K Second Run", xlab= "Probability", ylab= "Knumber")
text(ABFrun2, k_runRest, labels=k_runRest, cex=0.8, pos=4)

plot(ABFrun3, k_runRest, xlim=c(0,1.2), main= "K Third Run", xlab= "Probability", ylab= "Knumber")
text(ABFrun3, k_runRest, labels=k_runRest, cex=0.8, pos=4)

plot(ABFrun4, k_runRest, xlim=c(0,1.2), main= "K Fourth Run", xlab= "Probability", ylab= "Knumber")
text(ABFrun4, k_runRest, labels=k_runRest, cex=0.8, pos=4)

plot(ABFrun5, k_runRest, xlim=c(0,1.2), main= "K Fifth Run", xlab= "Probability", ylab= "Knumber")
text(ABFrun5, k_runRest, labels=k_runRest, cex=0.8, pos=4)

#Alpha plots
par(mfrow=c(1,1))
plot(ABFalphaRun, k_run_alpha, xlim=c(0,1.2), main= "K with constant Alpha", xlab= "Probability", ylab= "Knumber")
text(ABFalphaRun, k_run_alpha, labels=k_run_alpha, cex=0.8, pos=4)

par(mfrow=c(1,5))
plot(abs(Ln_1M_run_1), k_run1, xlim=c(14000, 35000))
text(abs(Ln_1M_run_1), k_run1, labels=k_run1, cex=0.8, pos=4)
plot(Ln_1M_run_1), k_run1, xlim=c(14000, 35000))