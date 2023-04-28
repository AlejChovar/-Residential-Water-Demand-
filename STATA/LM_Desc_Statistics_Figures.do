
/*
Title: Estimating Residential Water Demand  under Systematic Shifts between Uniform Price (UP) and Increasing blokc Tariffs (IBT)
Date: 24 abril 2023
Author: A M. Chovar Vera, F.A. Vasqu??z-Lav??n, and R.Ponce Oliva
emails: achovarv@bcentral.cl and fvasquez@udd.cl

This dofile allow reprocude the lineal models considered in the manuscritps. In addition to descriptive statistics and Figures.

*/


***********************************************
*** Table 1: Water consumption in m3/month.
clear
*** data 
use water_data.dta, clear
sum x, d
sum x if tariff_str==1, d
sum x if tariff_str==0, d

***********************************************
*** Figure 1: Water consumption distribution over the period (m3/month)
histogram x if x!=0, bin(30) percent fcolor(silver) lcolor(silver) lpattern(tight_dot) gap(4) ytitle(Percent) yscale(line) xtitle(,size(zero)) xscale(line) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))
save Figure_1.jpg, replace


***********************************************
*** Table 2: Descriptive statistic variables
global variables "prec temp_max temp_min bill numberH averageP marginalP1 marginalP2 fixedc income"
foreach var in $variables{
sum `var', d 
}
*

***********************************************
*** Table 3: Prices tariff structures, 2007-2012 
by tariff_str: sum fixedc p1 p2 bill w

***********************************************
*** Figure 2: Prices levels (nominal prices)

sort grupo_tar ano mes
by grupo_tar ano mes: egen average_g1=mean(p1_)
by grupo_tar ano mes: egen average_g2=mean(p2_)
by grupo_tar ano mes: gen id_g=_n


twoway (tsline average_g1 if id_g==1 & grupo_tar==1) ( tsline average_g1 if id_g==1 & grupo_tar==2), ///
				ytitle(" ") ylabel(#5, labsize(medsmall)) title(p1) ///
				graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
				ttitle(date) tscale(noline) tlabel(#12, labsize(small) angle(forty_five) format(%tm))  ///
				legend(on order(1 "Group 1" 2 " Group 2"))
graph save var1, replace 

twoway (tsline average_g2 if id_g==1 & grupo_tar==1 & tariff_str==1) ( tsline average_g2 if id_g==1 & grupo_tar==2 & tariff_str==1), ///
				ytitle(" ") ylabel(, labsize(medsmall)) title(p2) subtitle(- summer months -) ///
				graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ///
				ttitle(date) tscale(noline) tlabel(#7, labsize(small) angle(forty_five) format(%tm)) ///
				legend(on order(1 "Group 1" 2 " Group 2"))
graph save var2, replace 

graph combine var1.gph var2.gph
save Figure_2.png, replace

***********************************************
*** Table 4: Estimations of residential water demand 
** GLS - UP monnths 
generate double date=ym(ano,mes)
tsset id_servicio date
xtreg  logx1 pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12 logp y if tariff_str==0, re vce(robust) theta


***********************************************
*** Table 4: Estimations of residential water demand 
** IV model
xtivreg logx1 pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12 if tariff_str==1 (logp y = logbasic logp2 )  , re first vce(robust)


***********************************************
*** Models according group Table 5

** preparing data
forvalues i=1(1)10{
gen perc`i'=0
}
*


forvalues i=2007(1)2007{
set more off
forvalues j=1(1)12{
xtile percent_`i'_`j'= x if ano==`i' & mes==`j', nq(10)
forvalues t=1(1)10{
replace perc`t'=1 if percent_`i'_`j'==`t'
}
drop percent_`i'_`j'
}
gen prueba2= perc1+ perc2+ perc3+ perc4+ perc5+ perc6+ perc7+ perc8+ perc9+ perc10
tab prueba2 if ano==`i', m
drop prueba2
}
*

sort id_servicio ano mes
by id_servicio: egen veces=count(_n)
by id_servicio: egen sump1=sum(perc1)
by id_servicio: egen sump2=sum(perc2)
by id_servicio: egen sump3=sum(perc3)
by id_servicio: egen sump4=sum(perc4)
by id_servicio: egen sump5=sum(perc5)
by id_servicio: egen sump6=sum(perc6)
by id_servicio: egen sump7=sum(perc7)
by id_servicio: egen sump8=sum(perc8)
by id_servicio: egen sump9=sum(perc9)
by id_servicio: egen sump10=sum(perc10)

gen tasa1=sump1/veces
gen tasa2=sump2/veces
gen tasa3=sump3/veces
gen tasa4=sump4/veces
gen tasa5=sump5/veces
gen tasa6=sump6/veces
gen tasa7=sump7/veces
gen tasa8=sump8/veces
gen tasa9=sump9/veces
gen tasa10=sump10/veces

gen nunca=0
replace nunca=1 if (tasa6+tasa7+tasa8+tasa9+tasa10==0) 
replace nunca=2 if (tasa1+tasa2+tasa3+tasa4+tasa5==0) 

gen indicador=.
replace indicador=1 if (tasa1+ tasa2+ tasa3 >=0.70) & nunca==1 
replace indicador=2 if (tasa10 + tasa9+ tasa8   >=0.70) & nunca==2 

gen pmarginal=.
replace pmarginal=p1_ if logx1<=lnw1 & tariff_str==1
replace pmarginal=p2_ if logx1>lnw1 & tariff_str==1
replace pmarginal=p1_  if tariff_str==0

gen logp=log(pmarginal)

** low consumption 
** GLS non-summer and summer
eststo: quietly xtreg  logx1 logp y  pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12 if tariff_str==1 & indicador==1, re vce(robust) theta
eststo: quietly xtreg  logx1 logp y  pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12 if tariff_str==0 & indicador==1, re vce(robust) theta
esttab using low.doc, se replace
eststo clear


** High consumption
** GLS non-summer
eststo:   xtreg  logx1 logp y pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12  if indicador==2 & tariff_str==0, re vce(robust) theta
esttab using non_summer_hc.doc, se replace
eststo clear

** 
gen y=.
replace y=y1_1 if logx1<=lnw1 & tariff_str==1
replace y=y2_1 if logx1>lnw1 & tariff_str==1

eststo: xtivreg logx1 pp_  tem media_nt terremoto com_2 com_3 com_4 com_5 com_6 com_7 com_8 com_9 com_10 com_11 com_12 if indicador==2 & tariff_str==0 (logp y = logbasic logp2 ), re first vce(robust)
esttab using IV_HIGH.doc, se
eststo clear







