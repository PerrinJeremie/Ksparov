for image in `ls | egrep ".png"`
do
	convert $image -resize $1 $image
done
