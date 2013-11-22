#!/usr/bin/env gawk -f
BEGIN{
    column_count=split(cols,column_numbers," ");
}
    {
        for(i=1; i<=column_count; i++)
            if(column_numbers[i] < 0)
                printf "%s%s", $(NF + 1 + column_numbers[i]), OFS
            else
                printf "%s%s", $column_numbers[i], OFS
        printf "%s", ORS
    }
