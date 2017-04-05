r2t=2000

set terminal png size 12000,10500
    set output './'.name.'_res2_'.i.'.png'
    set multiplot layout 10,1 title "\nAsteroid ".name."\n".resonance."" font ",200"
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

    set ytics auto

    #6
    set title "\nSEMIMAJOR AXIS OSCILLATIONS" font ",100"
    plot './'.name.'.smooth2' index i u 1:4 w l lt rgb 'blue' title 'Smoothed axis oscillation'

    set title "\nPARAMETERS" font ",100"
    set grid lt rgb 'light-grey'
    #2
    plot './'.name.'.circ2' index i u 2:5 w lp lt rgb 'blue' pt 7 lw 2 title 'R2*D'\
    ,r2t lt rgb 'red' lw 2 title 'treshold'
    unset grid    
    #3
    plot './'.name.'.smooth2' index i u 1:5 w l lt rgb 'blue' lw 2 title 'line deviation'\

    
    set xtics auto
    #4
    set title "\nSINE AND COSINE OSCILLATIONS" font ",100"
    plot './'.name.'.smooth2' index i u 1:2 w l lt rgb 'red' title 'Smoothed phase cosine oscillation'
    #5
    plot './'.name.'.smooth2' index i u 1:3 w l lt rgb 'red' title 'Smoothed phase sine oscillation'
    
    set xrange [0:0.05]
    
    #7
    set title "\nPERIODOGRAMS" font ",100"
    plot './'.name.'.per2' index i u 1:2 w lp lt rgb 'red' pt 7 title 'Phase periodogram',\
    ph_t lt rgb 'dark-green' title '0.99 treshold'
    #8
    unset title
    plot './'.name.'.per2' index i u 1:3 w l lt rgb 'blue' title 'Axis periodogram',\
    ax_t lt rgb 'dark-green' title '0.99 treshold'
    #9
    plot './'.name.'.per2' index i u 1:4 w lp lt rgb 'red' pt 7 title 'Phase normed periodogram',\
    '' index i u 1:5 w l lt rgb 'blue' title 'Axis normed periodogram'
    #10
    plot './'.name.'.per2' index i u 1:6 w l lt rgb 'dark-green' title 'Cross-periodogram',\
    cr_t lt rgb 'red' title '0.99 treshold'

    unset multiplot
reset
