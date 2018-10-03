
for d in *
do
    if [ -d $d ]
    then
        cd $d
        echo $d
        ./experiment.sh
        cd ../
    fi
done
