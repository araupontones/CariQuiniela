#push to repo
timestamp=$(date +%c)

cd /home/rstudio/CariQuiniela/
git checkout walle
git add data/3.clean/.
git commit -m "Wallee: $timestamp"
git push origin walle
