clear
global path "M:\Documents\University\Dissertation"
cd "$path"

capture log close
log using "Dissertation.smcl", replace

use "Racial_Bias_EER_dataset.dta"

ssc install oaxaca
ssc install rif

*** RIF ***
forval q = 10(20)90 {
	rifhdreg rating_AVG Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) 
}


forval q = 10(20)90 {
	rifhdreg lnwage Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) 
}

**** Oaxaca-Blinder Decomposition At Mean ****
xi: oaxaca rating_AVG Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) by(Dblack) relax swap

xi: oaxaca rating_G Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) by(Dblack) relax swap

xi: oaxaca rating_C Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) by(Dblack) relax swap

xi: oaxaca rating_T Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) by(Dblack) relax swap

xi: oaxaca lnwage Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) by(Dblack) relax swap


*** RIF Oaxaca-Blinder ***
forval q = 10(20)90 {
	xi: oaxaca_rif rating_AVG Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) by(Dblack) relax swap
}

forval q = 10(20)90 {
	xi: oaxaca_rif rating_G Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) by(Dblack) relax swap
}

forval q = 10(20)90 {
	xi: oaxaca_rif rating_C Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) by(Dblack) relax swap
}

forval q = 10(20)90 {
	xi: oaxaca_rif rating_T Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) by(Dblack) relax swap
}

forval q = 10(20)90 {
	xi: oaxaca_rif lnwage Dblack age age2  Goals Assists Yel Red SpG PS Aerialswon Tackles Interceptions Fouls Offsidesdefensive Clear Drbdefensive Blocks KeyP Drboffensive Fouled Off  UnsTch AvgP Crosses LongB ThrB OffTarget OnPost OnTarget Blocked Dispossessed  InAccCr InAccCrn i.TEAM i.ROLE i.SeasonStart, cluster(id_name) rif(q(`q')) by(Dblack) relax swap
}