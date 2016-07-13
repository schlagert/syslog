################################################################################
# Copyright 2013-2016, Tobias Schlager <schlagert@github.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
# A gnuplot script to visualize the 'benchmark-small.dat' file written by the
# benchmark.escript.
################################################################################

set output 'benchmark-small.png'
set terminal png size 1100,400 font 'Ubuntu' 11

set grid
set multiplot layout 1,3 title 'benchmark.escript all small 1000 10000'

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
plot 'benchmark-small.dat' using 3:xticlabel(1) lt rgb '#8b1a0e' title ''

set title 'Total Duration'
set ylabel 'milliseconds'
plot 'benchmark-small.dat' using 4:xticlabel(1) lt rgb '#8b1a0e' title ''

set title 'Peak Memory Used'
set ylabel 'megabytes'
plot 'benchmark-small.dat' using 5:xticlabel(1) lt rgb '#8b1a0e' title ''

unset multiplot
