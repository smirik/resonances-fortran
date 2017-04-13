#r2t=2000
r2t=3.14159265358979/2.5

set terminal png size 12000,14500
    set output './'.name.'_res2_'.i.'.png'
    set multiplot layout 13,1 title "\nAsteroid ".name."\n".resonance."" font ",200"
    #set tmargin 2
    set lmargin at screen 0.025
    set rmargin at screen 0.975
    set grid lt rgb 'light-grey'
    set xrange [0:100000]
    set xtics 5000
    set ytics pi/4
    #1
    set title "PHASE" font ",100"
    plot './'.name.'.phout2' index i u 1:2 w p pt 7 title 'Phase'
    #2
    set title "\nSMOOTHED PHASE" font ",100"
    plot './'.name.'.phout2' index i u 1:3 w p pt 7 title 'Smoothed phase'

    set ytics auto

    #3
    set title "\nSEMIMAJOR AXIS OSCILLATIONS" font ",100"
    plot './'.name.'.smooth2' index i u 1:6 w l lt rgb 'blue' title 'Axis',\
    '' index i u 1:7 w l lt rgb 'red' lw 2 title 'Smoothed axis'

    set title "\nPARAMETERS" font ",100"
    unset grid    
#    set grid lt rgb 'light-grey'
    #4
    plot './'.name.'.circ2' index i u 2:5 w lp lt rgb 'blue' pt 7 lw 2 title 'R2'\
    ,r2t lt rgb 'red' lw 2 title 'treshold'
    #5
    plot './'.name.'.smooth2' index i u 1:8 w l lt rgb 'blue' lw 2 title 'line deviation'

    set xtics auto
    #6
    set title "\nSINE AND COSINE OSCILLATIONS" font ",100"
    plot './'.name.'.smooth2' index i u 1:4 w l lt rgb 'red' title 'Smoothed phase cosine oscillation'
    #7
    plot './'.name.'.smooth2' index i u 1:5 w l lt rgb 'red' title 'Smoothed phase sine oscillation'

    set xrange [0:0.05]

    #8
    set title "\nPERIODOGRAMS" font ",100"
    plot './'.name.'.per2' index i u 1:2 w lp lt rgb 'red' pt 7 title 'Phase periodogram',\
    ph_t lt rgb 'dark-green' title '0.99 treshold'
    unset title
    #9
    plot './'.name.'.per2' index i u 1:3 w lp lt rgb 'red' pt 7 title 'Phase new periodogram',\
    ph_t lt rgb 'dark-green' title '0.99 treshold'
    #10
    plot './'.name.'.per2' index i u 1:4 w l lt rgb 'blue' title 'Axis periodogram',\
    ax_t lt rgb 'dark-green' title '0.99 treshold'
    #11
    plot './'.name.'.per2' index i u 1:5 w l lt rgb 'blue' title 'Axis new periodogram',\
    ax_t lt rgb 'dark-green' title '0.99 treshold'

    #12
    plot './'.name.'.per2' index i u 1:6 w lp lt rgb 'red' pt 7 title 'Phase normed periodogram',\
    '' index i u 1:7 w l lt rgb 'blue' title 'Axis normed periodogram'
    #13
    plot './'.name.'.per2' index i u 1:8 w lp lt rgb 'dark-green' pt 7 title 'Cross-periodogram',\
    cr_t lt rgb 'red' title '0.99 treshold'

    unset multiplot
reset
