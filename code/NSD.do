

import delimited "/Users/johnholbein/Desktop/NSD/study-1-names.csv", clear
save "/Users/johnholbein/Desktop/NSD/names-1.dta", replace

import delimited "/Users/johnholbein/Desktop/NSD/study-2-names.csv", clear
drop res*
save "/Users/johnholbein/Desktop/NSD/names-2.dta", replace

import delimited "/Users/johnholbein/Desktop/NSD/study-3-names.csv", clear
drop res*
save "/Users/johnholbein/Desktop/NSD/names-3.dta", replace

use "/Users/johnholbein/Desktop/NSD/names-1.dta", clear
append using "/Users/johnholbein/Desktop/NSD/names-2.dta"
append using "/Users/johnholbein/Desktop/NSD/names-3.dta"

foreach var in citizen incomeord educationord correct {
capture replace `var'="" if `var'=="NA"
capture destring `var', replace

}

keep name citizen incomeord educationord correct identity

gen num=_n

foreach var in citizen incomeord educationord correct {
	
	gen `var'_SEM=`var'
}

collapse (firstnm) identity (mean) citizen incomeord educationord correct ///
(semean)  citizen_SEM incomeord_SEM educationord_SEM correct_SEM ///
 (count) num, by(name)
 
 order name identity correct
 
 gsort  identity  - correct
 
 replace identity="Black" if identity=="Black or African American"
  replace identity="Asian" if identity=="Asian or Pacific Islander"


  dataout, save( /Users/johnholbein/Desktop/NSD/namesperceptionsaggregate) tex replace
 
 
