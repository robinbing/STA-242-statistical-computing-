time for file in trip_fare_*.csv
do
cut -d , -f 10,11 $file >>fare
done

awk -F "," '{print $2 - $1}' fare| sort -n >>res

