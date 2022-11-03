echo "Pushing to repo"
git config --global user.email "araupontones@gmail.com"
git add .
git commit -m 'Walle $(date + "%d%B")' 
git push origin main
