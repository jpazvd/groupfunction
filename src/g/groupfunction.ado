*! groupfunction
* Paul Corral - World Bank Group 
* Minh Nguyen - World Bank Group 
* Joao Pedro Azevedo - World Bank Group 


cap prog drop groupfunction
program define groupfunction, eclass
version 11.2
syntax [aw pw fw] , by(varlist) [ sum(varlist numeric)  rawsum(varlist numeric) ///
mean(varlist numeric) first(varlist numeric) max(varlist numeric) min(varlist numeric) ///
count(varlist numeric) sd(varlist numeric) VARiance(varlist numeric)]
qui{
	tempvar _useit _gr0up _thesort
	//gen `_thesort'   =_n
	gen `_useit'	 = 1
	
	//save label
	foreach x of varlist `by' {
		local Nm: val lab `x'
	
		if ("`Nm'"!=""){
		local l: value label `x'
		local vallabs "`vallabs' `l'"
		}
	}
	
	if ("`vallabs'"!=""){
	tempfile labeldo
	label save `vallabs' using `labeldo', replace
	}
	
	//Weights
	local wvar : word 2 of `exp'
	if "`wvar'"==""{
		tempvar peso
		gen `peso'=1
		local wvar `peso'
	}
	else{
		replace `_useit'=0 if `wvar'==.
	}
	
	
	if ("`sum'"=="" & "`mean'"=="" & "`first'"=="" & "`max'"=="" & "`min'"=="" & "`count'"=="" & "`variance'"==""& "`sd'"=="" & "`rawsum'"==""){
		display as error "Please specify variables for sum, rawsum, mean, min, max, count, sd, variance, or first"
		exit 
	}
	
	if ("`by'"==""){
		gen `_gr0up' =1
		local by `_gr0up'
	}
	//adjust here when adding new functions
	local procs sum mean first max min count rawsum sd variance
	local check
	foreach x of local procs{
		local `x': list uniq `x'
		local check: list check & `x'
	}
	
	
	if ("`checks'"!=""){
		display as error "Please specify unique names for variables in sum, first, mean, max, and min options"
	}
	
	
	sort `by'
	
	local numby = wordcount("`by'")
	
	//Account for more than one by variable, and strings
	foreach x of local by{
		if ("`:val lab `x''"!="") local lbl_`x' : val lab `x'
		cap confirm string variable `x'
		if (_rc==0){
			local strpres =1
			local strs `strs' `x'
			mata: st_sview(strs=.,.,tokens("`strs'"),"`_useit'")
		}
		
	}
	
	local by2: list by - strs
	mata: st_view(nostrs=.,.,tokens("`by2'"),"`_useit'")
	
	
	/*
	foreach x of local by{
	if ("`:val lab `x''"!=""){
	levelsof `x', local(nb)
	local _val`x' lab def _val`x' 			
	foreach i of local nb{
	local _val`x' `_val`x'' `i'  `"Group `:lab `:val lab `x'' `i''"'
	}
	}
	}
	dis "`_valforeign'"
	*/
	if (`numby'>1|"`strs'"!=""){
		tempvar _1x1
		egen `_1x1' = group(`by')
		local thearea `_1x1'
	}
	else local thearea `by'
	
	//Import data into mata
	mata: st_view(w=.,.,tokens("`wvar'"),"`_useit'")	
	mata: st_view(area=.,.,tokens("`thearea'"),"`_useit'")
	mata: info = panelsetup(area,1)
	mata: rows(info)
	//Get area matrix
	
	if ("`strs'"!=""){
		mata: strs=strs[info[.,1],.]
		mata: nostrs=nostrs[info[.,1],.]
	}
	else{
		mata: nostrs=nostrs[info[.,1],.]
	}
	
	
	if ("`sum'"!=""){
		mata: st_view(x=.,.,tokens("`sum'"),"`_useit'")			
		mata: xsum = _fastsum(x,w,info)
	}
	
	if ("`rawsum'"!=""){
		mata: st_view(x=.,.,tokens("`rawsum'"),"`_useit'")	
		
		mata: w2=J(rows(w),1,1)	
		mata: xrawsum = _fastsum(x,w2,info)
	}
	
	if ("`count'"!="") {
		mata: st_view(x=.,.,tokens("`count'"),"`_useit'")	
		mata: xcount = _fastcount(x,info)
	}
	
	if ("`mean'"!=""){
		mata: st_view(x=.,.,tokens("`mean'"),"`_useit'")	
		mata: xmean = _fastmean(x,w,info)
	}
	
	if ("`sd'"!=""){
		mata: st_view(x=.,.,tokens("`sd'"),"`_useit'")	
		mata: xsd = sqrt(_fastvariance(x,w,info))
	}
	
	if ("`variance'"!=""){
		mata: st_view(x=.,.,tokens("`variance'"),"`_useit'")	
		mata: xvariance = (_fastvariance(x,w,info))
	}
	
	if ("`first'"!=""){
		mata: st_view(x=.,.,tokens("`first'"),"`_useit'")
		mata: xfirst = _fastfirst(x,info)
	}
	
	if ("`max'"!=""){
		mata: st_view(x=.,.,tokens("`max'"),"`_useit'")
		mata: xmax = _fastmax(x,info)
	}
	
	if ("`min'"!=""){
		mata: st_view(x=.,.,tokens("`min'"),"`_useit'")
		mata: xmin = _fastmin(x,info)
	}
	
	foreach x of local procs{
		if ("``x''"!=""){
			local finmat `finmat' x`x'
			local procs2 `procs2' `x'
		}
	}
	dis 
	local finmat=subinstr("`finmat'"," ",",",.)
	mata: xx=(`finmat')
	//Save number of observations
	mata: st_matrix("_obs_",rows(info))
	
	
	//SAVE RESULTS in STATA
	clear
	local lasvar  by2 `procs2'
	local o=_obs_[1,1]
	
	set obs `o'
	if "`strs'"!=""{
		local ccc=1
		foreach x of local strs{
			mata:st_local("ha",strofreal(max(strlen(strs[.,`ccc']))))
			gen str`ha' `x'= ""
			lab var `x' "Group by `x'"
			local ccc=`ccc'+1
		}
		mata: st_sstore(.,tokens(st_local("strs")),.,strs)
	}
	
	foreach x of local lasvar{
		foreach y of local `x'{
			if ("`x'"!="by2"){
				gen double `y' = .
				lab var `y' "`x' of `y'"
				local _all `_all' `y'
			}
			else{
				gen double `y' = .
				lab var `y' "Group by `y'"
				//if `"`_val`y''"'!=""{
				//lab def `y' `_val`y''
				//lab val `y' `y' 
				//}
			}
		}
	}
	
	if ("`by2'"!="") mata: st_store(.,tokens(st_local("by2")),.,nostrs)
	mata: st_store(.,tokens(st_local("_all")),.,xx)
	//apply label back
	if ("`vallabs'"!=""){
		do `labeldo'
		foreach x of local by2 {
			if "`lbl_`x''"~="" lab val `x' `lbl_`x''
		}
	}
}
	
end

mata 
mata set matastrict off
//data should have been previously sorted
function _fastmean(real matrix x, real matrix w, real matrix info){
	
	
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	if ((hasmissing(x)+hasmissing(w))!=0){
		//slow option
		for(i=1; i<=r;i++){
			panelsubview(xi=.,x,i,info)
			panelsubview(wi=.,w,i,info)
			for(j=1;j<=jj;j++){
				X1[i,j] = mean(xi[.,j],wi)
			}
		}
	}
	else{
		for(i=1; i<=r; i++){
			rr  = info[i,1],. \info[i,2],.
			rr2 = info[i,1],1 \ info[i,2],1
			X1[i,.] = mean(x[|rr|],w[|rr2|])
		}
	}
	return(X1)
}

function _fastvariance(real matrix x, real matrix w, real matrix info){
	
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	if ((hasmissing(x)+hasmissing(w))!=0){
		//slow option
		for(i=1; i<=r;i++){
			panelsubview(xi=.,x,i,info)
			panelsubview(wi=.,w,i,info)
			for(j=1;j<=jj;j++){
				X1[i,j] = diagonal(quadvariance(xi[.,j],wi))'
			}
		}
	}
	else{
		for(i=1; i<=r; i++){
			rr  = info[i,1],. \info[i,2],.
			rr2 = info[i,1],1 \ info[i,2],1
			X1[i,.] = diagonal(quadvariance(x[|rr|],w[|rr2|]))'
		}
	}
	return(X1)
}


//data should have been previously sorted
function _fastsum(real matrix x, real matrix w, real matrix info){
	//ww = strtoreal(stlocal("rawsum")
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	if ((hasmissing(x)+hasmissing(w))!=0){
		//slow option
		for(i=1; i<=r;i++){
			panelsubview(xi=.,x,i,info)
			panelsubview(wi=.,w,i,info)
			for(j=1;j<=jj;j++){
				if(j==1) X1[i,1] = quadcolsum(xi[.,j]:*wi)
				else     X1[i,j] = quadcolsum(xi[.,j]:*wi)
			}
		}
	}
	else{
		for(i=1; i<=r; i++){
			rr  = info[i,1],. \info[i,2],.
			rr2 = info[i,1],1 \ info[i,2],1
			X1[i,.] =  quadcolsum(x[|rr|]:*w[|rr2|])
		}
	}
	return(X1)
}

function _fastfirst(real matrix x, real matrix info){
	
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	
	for(i=1; i<=r; i++){
		rr  = info[i,1],. \info[i,2],.
		X1[i,.] =  x[info[i,1],.]
	}
	
	
	return(X1)
}

function _fastmax(real matrix x, real matrix info){
	
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	
	for(i=1; i<=r; i++){
		rr  = info[i,1],. \info[i,2],.
		X1[i,.] =  colmax(x[|rr|])
	}
	
	
	return(X1)
}

function _fastmin(real matrix x, real matrix info){
	
	r  = rows(info)
	jj = cols(x)
	X1 = J(rows(info),cols(x),0)
	//check to see if we can use block 
	
	for(i=1; i<=r; i++){
		rr  = info[i,1],. \info[i,2],.
		X1[i,.] =  colmin(x[|rr|])
	}
	
	
	return(X1)
}

//data should have been previously sorted
function _fastcount(real matrix x, real matrix info) {
	r  = rows(info)
	jj = cols(x)
	X1 = J(r,jj,0)
	//fest = (st_local("std")~="" ? J(2,3,NULL) : J(1,3,NULL))        
	for(i=1; i<=r; i++){
		rr  = info[i,1],. \ info[i,2],.
		rr2 = info[i,1],1 \ info[i,2],1
		X1[i,.] = quadcolsum((x[|rr|]:<.))
	}
	return(X1)
}

end

//		groupfunction [aw=weight], sum(`todosaqui2' `pptarsa') mean(`pp1' `ppcovsa' `ppadsa' `ppdepsa' `pppov0' `pppov1' `pppov2' `todosaqui' `medexp_red' `fullcredit2016' `tax_owed0' `agtax' `discount') by(decile) rawsum



