

#git config --global user.email "araupontones@gmail.com"
timestamp=$(date +%c)
git add .
git commit -m "Wallee: $timestamp"
git push origin main
