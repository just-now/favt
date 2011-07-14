#!/bin/bash

#sed -i '1 a \ ranksep=1;\n' $1

colors[0]=red
colors[1]=blue
colors[2]=green
colors[3]=blanchedalmond
colors[4]=lavender
colors[5]=linen
colors[6]=mintcream
colors[7]=moccasin
colors[8]=oldlace
colors[9]=papayawhip
colors[10]=white
colors[11]=whitesmoke
colors[12]=gray
colors[13]=lightgray
colors[14]=lightslategray
colors[15]=black
colors[16]=crimson
colors[17]=darksalmon
colors[18]=mediumvioletred
colors[19]=beige
colors[20]=darkkhaki
colors[21]=peru
colors[22]=saddlebrown
colors[23]=sandybrown
colors[24]=darkorange
colors[25]=orange
colors[26]=orangered
colors[27]=darkgoldenrod
colors[28]=gold
colors[29]=goldenrod
colors[30]=greenyellow
colors[31]=lightgoldenrodyellow
colors[32]=palegoldenrod
colors[33]=yellow
colors[34]=yellowgreen
colors[35]=darkgreen
colors[36]=forestgreen
colors[37]=green
colors[38]=greenyellow
colors[39]=lawngreen
colors[40]=lightseagreen
colors[41]=limegreen
colors[42]=mediumseagreen
colors[43]=mediumspringgreen
colors[44]=mintcream
colors[45]=darkviolet
colors[46]=magenta
colors[47]=mediumorchid

let j=0
cat metis.groups | (while read line
    do echo $line;
    echo iteration $j, ${colors[$j]}
    for i in $line; do sed -i -r 's/(^[ ]+\"\('$i'\)[^ ]*);/\1 [color='${colors[$j]}'];/' out.dot; done
    let "j=j+1"
    done)

