' Permutation Generation Methods 
'' EViews program that calculates and displays all permutations
''' Add-in written by : SMIDA Houcine L. 2025
' ------------------------------------------------------------------
' dialog box options 
' ------------------------------------------------------------------
%list = "1 2 3 4"
!type = 3
%typelist = " "" 1. Backtracking permutations by swapping ""  "" 2. Heap's permutation ""  "" 3. Lexicographic order permutation ""  "" 4. Smida's permutation ""  "" 5. Woodall's permutation "" "
!sort = 1
%sortlist = " "" a. Yes (sort the first permutation) ""  "" b. No (do not sort the first permutation) "" "
!result = @uidialog("caption", "Permutation Generation Methods", _
"edit", %list, "Enter the elements to permute, separated by spaces (e.g., 1 2 3 4, A B C D, Blue Green Red Yellow)", 100000, _
"text", "If sentences must be swapped, they should be placed in double quotes “ ” ", _

"radio", !type, "Select a permutation method", %typelist , _
"radio", !sort, "Sort the first permutation ?", %sortlist, _ 
"text", "EViews program that calculates all permutations", _
 "text", "Add-in written by : SMIDA Houcine L. 2025") 
' ------------------------------------------------------------------
' display a warning if the list is empty when OK is clicked 
' ------------------------------------------------------------------
if @wcount(%list) < 1 and !result <> -1 then
	@uiprompt("The list is empty, please enter the elements to permute")
	return
endif
' ------------------------------------------------------------------
' choose different elements (without repetition)
' ------------------------------------------------------------------
for !i= 1 to @wcount(%list) -1
	for !j= !i+1 to @wcount(%list)
		if @word(%list,!i)=@word(%list,!j) then
			@uiprompt("Please choose different elements (without repetition)")
			return
		endif
	next
next
' ------------------------------------------------------------------
' maximum number of elements that can be permuted
' ------------------------------------------------------------------
if @wcount(%list) > 10 and !result <> -1 then
	@uiprompt("The maximum number of elements to permute is limited to 10")
	return
endif
' ------------------------------------------------------------------
' stop if the Cancel button is clicked 
' ------------------------------------------------------------------
if !result = -1 then
	stop
endif
' ------------------------------------------------------------------
' create workfile  
' ------------------------------------------------------------------
!n=@wcount(%list)
workfile permutation u 1 !n
' ------------------------------------------------------------------
' delete some existing tables
' ------------------------------------------------------------------
table Tabperm
d Tabperm
table Tab
d Tab
table Tab0
d Tab0
table Tab1
d Tab1
table Tab2
d Tab2
' ------------------------------------------------------------------
table Tab0
table Tab
' ------------------------------------------------------------------
' define the table to be permuted
' ------------------------------------------------------------------
for !i=1 to !n
	Tab(!i)=@word(%list,!i)
next
' ------------------------------------------------------------------
' sorte the first permutation in ascending order ?
' ------------------------------------------------------------------
if !sort = 1 then
	Tab.sort(A1:A!n) A ' returns sorted elements in ascending order
endif
' ------------------------------------------------------------------
' set width of table column
' ------------------------------------------------------------------
for !i=1 to !n  
	!colwidthi=@length(Tab(!i))
!maxcolwidth=@length(Tab(1))
	if !colwidthi>!maxcolwidth then
		!maxcolwidth=!colwidthi
	endif
next
!colwidth=!maxcolwidth+2
' ------------------------------------------------------------------
' 1. Backtracking permutations by swapping	
'	 Permutations by backtracking using simple swapping
' ------------------------------------------------------------------
subroutine Backtracking_by_swapping(table E, scalar !i, scalar !j, scalar !n) 
	if !j=!n then 
		E.copytable Tab0 1 1  
		Tab0.insertcol(A)
		return
	else
		for !i=!j to !n 
			%x=E(!i)
			E(!i)=E(!j)
			E(!j)=%x
			call Backtracking_by_swapping(E, 1, !j+1, !n)
			%x=E(!i)
			E(!i)=E(!j)
			E(!j)=%x
		next
	endif
endsub
' ------------------------------------------------------------------
if !type = 1 then
	call Backtracking_by_swapping(Tab, 1, 1, !n)
	Tab0.deletecol(A)
	' -----------------------
	table Tabperm
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabperm(!i,!j)=Tab0(!i,@fact(!n)-!j+1)
		next
	next
' -----------------------
	d Tab
	d Tab0
	Tabperm.setwidth(@all) !colwidth
	show Tabperm ' display permutation
endif
' ------------------------------------------------------------------
' 2. Heap permutation
' ------------------------------------------------------------------
subroutine Heap(table E, scalar !i, scalar !n) 
	if !n=1 then ' Base Case : n=1 
		E.copytable Tab0 1 1  
		Tab0.insertcol(A)
		return
	else
		call Heap(E, 1, !n-1) ' recursive call on the first n-1 elements
		for !i=1 to !n-1
			if @mod(!n,2)=1 then ' if n is odd, swap the first element with the last element
				%temp=E(1)
				E(1)=E(!n)
				E(!n)=%temp
			else ' if n is even, swap the i-th element with the last element
				%temp=E(!i)
				E(!i)=E(!n)
				E(!n)=%temp
			endif
			call Heap(E, 1, !n-1) ' recursive call on the first n-1 elements
		next 
	endif
endsub
' ------------------------------------------------------------------
if !type = 2 then
	call Heap(Tab, 1, !n)
	Tab0.deletecol(A)
	' -----------------------
	table Tabperm
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabperm(!i,!j)=Tab0(!i,@fact(!n)-!j+1) 'order invertion
		next
	next
	' -----------------------
	d Tab
	d Tab0
	Tabperm.setwidth(@all) !colwidth
	show Tabperm ' display permutation
endif
' ------------------------------------------------------------------
' 3. Lexicographic order permutation
' ------------------------------------------------------------------
subroutine Lexicographic(table E, scalar !n)
	if !n=1 then 
		return
	endif
	!i=!n-1
	while !i>=1 and E(!i)>=E(!i+1)
		!i=!i-1 ' decrease i by 1
		if !i<1 then   'terminate the algorithm if i=0
				E.sort(A1:A!n) A
			return
		endif
	wend
	!j=!n
	while E(!j)<=E(!i) and !j>!i  ''' 
		!j=!j-1  ' decrease j by 1
	wend
	%temp=E(!i)
	E(!i)=E(!j) ' interchange
	E(!j)=%temp
	!l=!i+1
	!r=!n
	while !l<!r  ' reverse E(i+1, i+2, ..., n-1, n)
		%temp=E(!l)
		E(!l)=E(!r) ' interchange
		E(!r)=%temp
		!l=!l+1
		!r=!r-1
	wend
endsub
' ------------------------------------------------------------------
if !type = 3 then
	for !k=1 to @fact(!n)
		Tab.copytable Tab0 1 1
		Tab0.insertcol(A)
	call Lexicographic(Tab, !n)
	next
	Tab0.deletecol(A)
	' -----------------------
	table Tabperm
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabperm(!i,!j)=Tab0(!i,@fact(!n)-!j+1) 'order invertion
		next
	next
	' -----------------------
	d Tab
	d Tab0
	Tabperm.setwidth(@all) !colwidth
	show Tabperm ' display permutation
endif
' ------------------------------------------------------------------
' 4 . Smida permutation
' ------------------------------------------------------------------
subroutine Smida(table E, scalar !g) 
	if !g=1 then
		return
	endif
	!g=2
	while !g<=!n
		table F=E
		for !i=1 to !g  
			table B!i=F
			B!i.insertrow(!i) 1		
			for !j=1 to @fact(!g-1)  
				B!i(!i,!j)=Tab(!g)  
			next
			tabplace(E,B!i, 1, (@fact(!g-1))*(!i-1)+1, 1, 1, @fact(!g-1),@fact(!g-1))
			d B!i
		next
		d F
		!g=!g+1
	wend
endsub
' ------------------------------------------------------------------
if !type = 4 then
	table Tab0
	Tab0(1)=Tab(1)  '''
	call Smida(Tab0, !n+1)
	' -----------------------
	table Tabperm
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabperm(!i,!j)=Tab0(!i,!j)
		next
	next
	' -----------------------
	d Tab
	d Tab0
	Tabperm.setwidth(@all) !colwidth
	show Tabperm ' display permutation
endif
' ------------------------------------------------------------------
' 5. Woodall permutation
' ------------------------------------------------------------------
subroutine Woodall(table E, scalar !i, scalar !mp, scalar !n) 
	if !n=1 then ' Base Case : n=1 
		E.copytable Tab0 1 1  
		Tab0.insertcol(A)
		return
	else
		if !n=2 then 
			E.copytable Tab0 1 1  
			Tab0.insertcol(A)
			%temp=E(1)
			E(1)=E(2)
			E(2)=%temp
			E.copytable Tab0 1 1  
			Tab0.insertcol(A)
			return
		else
			for !mp=!n-1 to 1 step -1
				call Woodall(E, 1, 1, !n-1) ' recursive call on the first n-1 elements	
				if @mod(!n,2)=1 then ' if n is odd
					!swpt=1
				else ' if n is even
					!swpt=!mp
				endif
				%temp=E(!swpt)
				E(!swpt)=E(!n) ' swap with the last element
				E(!n)=%temp
 			next
			call Woodall(E, 1, 1, !n-1) ' recursive call on the first n-1 elements
		endif
	endif
endsub
' ------------------------------------------------------------------
if !type = 5 then
	table Tab2
	for !i=1 to !n
		Tab2(!i)=Tab(!n-!i+1) ' Convert string to table
	next
	call Woodall(Tab2, 1, 1, !n) ' call subroutine
	Tab0.deletecol(A)
	' -----------------------
	table Tabnoninv
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabnoninv(!i,!j)=Tab0(!i,@fact(!n)-!j+1) ' Woodall (1977) says that the table is indexed from right to left. Its equivalent in EViews is a table indexed from bottom to top.
		next
	next
	Tabnoninv.setwidth(@all) !colwidth
	' -----------------------
	table Tabperm
	for !i=1 to !n
		for !j=1 to @fact(!n)
			Tabperm(!i,!j)=Tab0(!n-!i+1,@fact(!n)-!j+1) 'order invertion
		next
	next
	' -----------------------
	d Tab
	d Tab0
	d Tab2
	Tabperm.setwidth(@all) !colwidth
	show Tabperm ' display permutation
endif
' ------------------------------------------------------------------
' End
' ------------------------------------------------------------------


