#!/usr/bin/env gawk -f
BEGIN{
    column_count=split(cols,column_numbers," ");
}
   {
       for(i=1; i<=column_count; i++)
           if(column_numbers[i] < 0)
               printf "%s", $(NF + 1 + column_numbers[i])
           else
               printf "%s", $column_numbers[i]
           if(i <= column_count)
               printf "%s", OFS
        
       printf "%s", ORS
   }
