# A gnuplot script to

set output 'benchmark.svg'
set terminal svg size 1100,400 fname 'Verdana' fsize 10

set grid
set multiplot layout 1,3 title 'benchmark.escript all 100 10000'

set style fill solid 0.9
set style data boxes

set style line 11 lc rgb '#808080' lt 1
set border 31 back ls 11
set tics nomirror
set grid noxtics
set style line 12 lc rgb '#808080' lt 0 lw 1
set grid back ls 12

set bmargin 3
set boxwidth 0.6 absolute

set title 'Throughput'
set ylabel 'logs per second'
plot 'benchmark.dat' using 3:xticlabel(1) lt rgb '#8b1a0e' title ''

set title 'Total Duration'
set ylabel 'milliseconds'
plot 'benchmark.dat' using 4:xticlabel(1) lt rgb '#8b1a0e' title ''

set title 'Maximum Memory Used'
set ylabel 'megabytes'
set logscale y
set yrange [10:4000]
plot 'benchmark.dat' using 5:xticlabel(1) lt rgb '#8b1a0e' title ''

unset multiplot
